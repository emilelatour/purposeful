
#' @title
#' Purposeful selection step #2
#'
#' @description
#' Fit a multivariable model and assess the importance of each covariate with
#' the purpose to get to a smaller, reduced model. Start with all covariates
#' identified for inclusion in Step #1. The smaller model will include only
#' covariates that are below a 0.05 cutoff for significance or that have strong
#' clinical reasons to stay in the model. A partial likelihood ratio test will
#' compare the full model with the reduced. The same data set used for the full
#' model is used to fit the reduced model.
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
#' @param keep_in_mod Character vector. Variables with strong clinical reasons
#'   to stay in the model. These will appear in both the full and reduced model
#'   regardless of statistical significance.
#' @param ref_level Character string. The factor level of outcome variable that
#'   corresponds to the true condition (1). If not provided then default is
#'   `NULL` and the model fit will determine the reference level.
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
#' @importFrom car Anova
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#'
#' @return
#' A list with:
#' \itemize{
#'   \item Covariates included in the full model
#'   \item Covariates included in the reduced model
#'   \item Model results for the full model
#'   \item Model results for the reduced model
#'   \item Partial likelihood ratio test results
#'   }
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
#' purposeful_step_2(data = data,
#'                   outcome = "mort",
#'                   predictors = c("age", "gender", "hb", "lac", "wbc"))
#'
#' #### Example 2 --------------------------------
#'
#' purposeful_step_2(data = data,
#'                   outcome = "mort",
#'                   predictors = c("age", "gender", "hb", "lac"),
#'                   keep_in_mod = "wbc")
#'
#'
purposeful_step_2 <- function(data,
                              outcome,
                              predictors,
                              keep_in_mod = NULL,
                              ref_level = NULL,
                              format = FALSE,
                              conf_level = 0.95,
                              exponentiate = TRUE,
                              digits = 2, ...) {


  ## Predictors ----------------

  vars0 <- union(predictors, keep_in_mod)

  ## Make the regression formula ----------------

  lhs <- if (!is.null(ref_level)) {

    glue::glue("({outcome} == '{ref_level}')")

  } else {

    glue::glue("{outcome}")

  }

  rhs0 <- paste(vars0, collapse = " + ")
  outcome <- lhs

  ## order for variables ----------------

  vars_order <- vars0


  ## Full model ----------------

  form0 <- glue::glue("{lhs} ~ {rhs0}")

  # Fit full model
  mod0 <- glm(as.formula(form0),
              data = data,
              family = binomial(link = "logit"))

  # Results full model
  res0 <- mod0 %>%
    broom::tidy() %>%
    janitor::clean_names()

  # LRT results
  lrt0 <- car::Anova(mod0) %>%
    broom::tidy() %>%
    janitor::clean_names()

  # Vars in full model
  pred0 <- lrt0 %>%
    dplyr::pull(term)

  # Predictors < 0.05
  pred1 <- lrt0 %>%
    dplyr::filter(p_value < 0.05) %>%
    dplyr::pull(term)


  ## Reduced model ----------------

  pred1 <- union(pred1, keep_in_mod)

  rhs1 <- paste(pred1, collapse = " + ")

  form1 <- glue::glue("{lhs} ~ {rhs1}")

  # Fit reduced model
  mod1 <- glm(as.formula(form1),
              data = mod0$model,   # Data from full model
              family = binomial(link = "logit"))

  # Results reduced model
  res1 <- mod1 %>%
    broom::tidy() %>%
    janitor::clean_names()

  # LRT results
  lrt1 <- car::Anova(mod1) %>%
    broom::tidy() %>%
    janitor::clean_names()


  ## Likelihood ratio test ----------------

  lr_test <- anova(mod0, mod1, test = "LRT") %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::rename(p_value = pr_chi)




  ## Results ----------------

  list(
    covariates_full_model = pred0,
    covariates_red_model = pred1,
    results_full_model = res0,
    results_red_model = res1,
    lrt_full_model = lrt0,
    lrt_red_model = lrt1,
    lr_test_results = lr_test)

}




