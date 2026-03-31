#' purposeful: Purposeful selection for regression models
#'
#' Application of the purposeful selection procedure developed by Hosmer and
#' Lemeshow for selecting predictors in regression models.  The package
#' provides functions for logistic regression (\code{\link{purposeful_glm}}),
#' Cox proportional hazards regression (\code{\link{purposeful_cox}}), and
#' ordinary linear regression (\code{\link{purposeful_lm}}).
#'
#' The older step-by-step functions (\code{purposeful_step_1},
#' \code{purposeful_step_2}, \code{purposeful_step_3}) are deprecated.
#' Please use \code{\link{purposeful_glm}} instead.
#'
#' @examples
#' # See ?purposeful_glm, ?purposeful_cox, ?purposeful_lm for full examples.
#'
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## From Jenny Bryan's googlesheets package / infer package
## https://github.com/tidymodels/infer/blob/master/R/infer.R
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(".",
      ## --- existing variables (purposeful_step_1/2/3) ---
      "assessment",
      "below_cutoff",
      "conf_high",
      "conf_low",
      "covariate",
      "estimate",
      "estimate_ci",
      "form1",
      "formula_str",   # renamed from "formula" to avoid stats::formula clash
      "log_odds",
      "log_odds0",
      "log_odds1",
      "lower_ci",
      "p_value",
      "p_value_lrt",
      "p_value_wald",
      "pct_change",
      "pr_chi",
      "ref",
      "res_univ",
      "term",
      "upper_ci",
      "vars",
      ## --- new variables (purposeful_glm/cox/lm engine) ---
      "action",
      "beta_new",
      "beta_old",
      "confounding",
      "max_pct_change",
      "n_vars",
      "removed_var",
      "worst_p",
      "worst_var"
    )
  )
}
