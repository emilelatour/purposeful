
#' @title
#' Purposeful selection step #3
#'
#' @description
#' Assess whether variables removed in steps #1 and #2 are potential
#' confounders. Add them back to the model if they are. Recommendation is that
#' if the changes in beta coefficients (log odds) exceed 20%, then the excluded
#' variable could be a confounder and should be included in the reduced model
#' due to its effect on the other estimates.
#'
#' @references
#' Hosmer DW, Lemeshow S (2000) \emph{Applied Logistic Regression}. John Wiley
#' & Sons, Inc.
#'
#'
#' @param data A tibble or data frame with the full data set.
#' @param outcome Character string. The dependent variable (outcome) for
#'   logistic regression.
#' @param predictors Character vector. Independent variables
#'   (predictors/covariates) for univariable and/or multivariable modelling.
#' @param ref_level Character string. The factor level of outcome variable that
#'   corresponds to the true condition (1). If not provided then default is
#'   `NULL` and the model fit will determine the reference level.
#' @param potential_confounders Character vector. Those variables removed in
#'   steps #1 and #2.
#' @param ... Additional arguments.
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr if_else
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom scales percent
#' @importFrom tibble tibble
#' @importFrom tidyr crossing
#' @importFrom tidyr unnest
#'
#' @return
#' A tibble
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#'
#' #### Sample data set --------------------------------
#'
#' set.seed(888)
#' age <- abs(round(rnorm(n = 1000, mean = 67, sd = 14)))
#' lac <- abs(round(rnorm(n = 1000, mean = 5, sd = 3), 1))
#' gender <-factor(rbinom(n = 1000, size = 1, prob = 0.6),
#'                 labels = c("male", "female"))
#' wbc <- abs(round(rnorm(n = 1000, mean = 10, sd = 3), 1))
#' hb <- abs(round(rnorm(n = 1000, mean = 120, sd = 40)))
#' z <- 0.1 * age - 0.02 * hb + lac - 10
#' pr = 1 / (1 + exp(-z))
#' y = rbinom(1000, 1, pr)
#' mort <- factor(rbinom(1000, 1, pr),
#'                labels = c("alive", "dead"))
#' data <- tibble::tibble(age, gender, lac, wbc, hb, mort)
#'
#' #### Example 1 --------------------------------
#'
#' purposeful_step_3(data = data,
#'                   outcome = "mort",
#'                   predictors = c("age", "lac"),
#'                   ref_level = "dead",
#'                   potential_confounders = c("gender", "wbc"))
#'
#'
#' #### Example 2 --------------------------------
#'
#' purposeful_step_3(data = data,
#'                   outcome = "mort",
#'                   predictors = c("age", "lac"),
#'                   potential_confounders = c("gender", "wbc"))
#'
#'

purposeful_step_3 <- function(data,
                              outcome,
                              ref_level = NULL,
                              predictors,
                              potential_confounders = NULL, ...) {


  ## Make the regression formula ----------------

  lhs <- if (!is.null(ref_level)) {

    glue::glue("({outcome} == '{ref_level}')")

  } else {

    glue::glue("{outcome}")

  }

  rhs <- paste(predictors, collapse = " + ")

  form0 <- glue::glue("{lhs} ~ {rhs}")
  outcome <- lhs


  ## order for variables ----------------

  vars_order <- potential_confounders


  res <- tidyr::crossing(form0,
                  potential_confounders) %>%
    mutate(potential_confounders = factor(potential_confounders,
                                          levels = vars_order)) %>%
    dplyr::arrange(potential_confounders) %>%
    mutate(potential_confounders = as.character(potential_confounders),
           form1 = paste0(form0, " + ", potential_confounders)) %>%
    mutate(res_0 = purrr::map(.x = form0,
                              .f = ~ .do_lr_for_confounding(data = data,
                                                            form = .x,
                                                            keep = predictors)),
           res_1 = purrr::map(.x = form1,
                              .f = ~ .do_lr_for_confounding(data = data,
                                                            form = .x,
                                                            keep = predictors)))

  res_0 <- res %>%
    tidyr::unnest(cols = c(res_0)) %>%
    dplyr::select(potential_confounders,
                  term,
                  log_odds0 = log_odds)

  res_1 <- res %>%
    tidyr::unnest(cols = c(res_1)) %>%
    dplyr::select(potential_confounders,
                  term,
                  log_odds1 = log_odds)

  res_0 %>%
    dplyr::left_join(.,
                     res_1,
                     by = c("potential_confounders", "term")) %>%
    mutate(pct_change = (log_odds1 - log_odds0) / log_odds0,
           assessment = dplyr::if_else(abs(pct_change) > 0.20,
                                       "Potential",
                                       ""),
           pct_change = scales::percent(pct_change)) %>%
    dplyr::select(potential_confounders,
                  term,
                  log_odds0,
                  log_odds1,
                  pct_change,
                  assessment)

  # %>%
  #   tidyr::unnest(cols = c(res_0, res_1),
  #                 names_repair = "universal") %>%
    # # mutate(pct_change = (log_odds1 - log_odds) / log_odds)
    # mutate(pct_change = (log_odds1 - log_odds) / log_odds,
    #        assessment = dplyr::if_else(abs(pct_change) > 0.20,
    #                                    "Potential",
    #                                    ""),
    #        pct_change = scales::percent(pct_change)) %>%
    # dplyr::select(potential_confounders,
    #               term,
    #               log_odds0 = log_odds,
    #               log_odds1,
    #               pct_change,
    #               assessment) %>%
    # {.}

}

