#' @title
#' Purposeful selection step #1
#'
#' @description
#' Univariable logistic regression for variables of intererst. A simple logistic
#' regression is fir for each variable. Significance is judged at a higher level
#' for inclusion (suggested 0.25).
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
#' @param ref_level haracter string. The factor level of outcome variable that
#'   corresponds to the true condition (1). If not provided then default is
#'   `NULL` and the model fit will determine the referenc level.
#' @param cutoff_value Numeric between 0 and 1. Include any covariate with
#'   p-value less than this value. Suggested default is 0.25.
#' @param format Display format in case I need to escape some characters. A
#'   place holder for now in case I need it in the future. Default is "html".
#' @param conf_level The confidence level to use for the confidence interval.
#'   Must be strictly greater than 0 and less than 1. Defaults to 0.95, which
#'   corresponds to a 95 percent confidence interval.
#' @param exponentiate Logical indicating whether or not to exponentiate the
#'   the coefficient estimates. This is typical for logistic and multinomial
#'   regressions, but a bad idea if there is no log or logit link. Defaults to
#'   `TRUE`.
#' @param digits Integer; number of decimals to round to.
#' @param ... Additional arguments.
#'
#' @importFrom broom tidy
#' @importFrom dplyr arrange
#' @importFrom dplyr coalesce
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr na_if
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom stats anova
#' @importFrom stats as.formula
#' @importFrom stats binomial
#' @importFrom stats drop1
#' @importFrom stats formula
#' @importFrom stats glm
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom tidyr crossing
#' @importFrom tidyr unnest
#'
#' @return
#' Atibble
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
#'
#' #### Example 1 --------------------------------
#'
#' purposeful_step_1(data = data,
#'                   outcome = "mort",
#'                   predictors = c("age", "gender", "wbc", "lac"),
#'                   # ref_level = "dead",
#'                   format = TRUE,
#'                   exponentiate = FALSE)
#'
#' #### Example 2 --------------------------------
#'
#' purposeful_step_1(data = data,
#'                   outcome = "mort",
#'                   predictors = c("age", "gender", "wbc", "lac"),
#'                   ref_level = "dead",
#'                   format = FALSE,
#'                   exponentiate = TRUE)


purposeful_step_1 <- function(data,
                              outcome,
                              predictors,
                              ref_level = NULL,
                              cutoff_value = 0.25,
                              conf_level = 0.95,
                              format = FALSE,
                              exponentiate = TRUE,
                              digits = 1, ...) {




  ## Make the regression formula ----------------

  lhs <- if (!is.null(ref_level)) {

    glue::glue("({outcome} == '{ref_level}')")

  } else {

    glue::glue("{outcome}")

  }

  rhs <- paste(predictors, collapse = " + ")

  form <- glue::glue("{lhs} ~ {rhs}")
  outcome <- lhs


  ## order for variables ----------------

  vars_order <- predictors


  ## Univariable analysis ----------------

  univ_res <- tidyr::crossing(outcome,
                              predictors) %>%
    mutate(predictors = factor(predictors,
                               levels = vars_order)) %>%
    dplyr::arrange(predictors) %>%
    mutate(predictors = as.character(predictors),
           formula = paste(outcome, "~", predictors),
           res_univ =
             purrr::map(.x = formula,
                        .f = ~ .do_logistic_univ(formula = .x,
                                                 data = data,
                                                 format = format,
                                                 exponentiate = exponentiate))) %>%
    dplyr::select(res_univ) %>%
    tidyr::unnest() %>%
    mutate(below_cutoff = dplyr::if_else(p_value_lrt < cutoff_value,
                                         # paste0("Less than", cutoff_value),
                                         glue::glue("Less than {cutoff_value}"),
                                         NA_character_)) %>%
    mutate_at(.vars = vars(p_value_wald, p_value_lrt),
              .funs = list(~ .mypval(.)))


  ## Format ----------------

  if (format == TRUE) {


    univ_res <- univ_res %>%
      mutate_at(.vars = vars(estimate, lower_ci, upper_ci),
                .funs = list(~ .myround(., digits = digits))) %>%
      mutate(estimate_ci =
               dplyr::if_else(is.na(p_value_wald),
                              NA_character_,
                              as.character(glue::glue("{estimate} ({lower_ci} to {upper_ci})"))),
             ref = dplyr::na_if(ref, ""),
             estimate_ci = dplyr::coalesce(ref, estimate_ci),
             p_value = dplyr::coalesce(p_value_wald, p_value_lrt)) %>%
      dplyr::select(covariate, term, estimate_ci, p_value, below_cutoff) %>%
      mutate_all(.tbl = .,
                 .funs = list(~ replace(., is.na(.), "")))


    if (exponentiate == TRUE) {
      univ_res <- univ_res %>%
        dplyr::rename(or_ci = estimate_ci)
    } else {
      univ_res <- univ_res %>%
        dplyr::rename(log_odds_ci = estimate_ci)
    }


  } else if (format == FALSE) {

    if (exponentiate == TRUE) {
      univ_res <- univ_res %>%
        dplyr::rename(or = estimate)
    } else {
      univ_res <- univ_res %>%
        dplyr::rename(log_odds = estimate)
    }


  }


  ## Return result ----------------


  univ_res

}





