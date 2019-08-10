#' purposeful: Purposeful selection in logistic regression
#'
#' Application of the purposeful selection procedure developed by Homser and
#' Lemeshow for selecting predictors in logistic regresssion.
#'
#' @examples
#' # Example usage:
#' library(purposeful)
#'
#' @docType package
#' @name purposeful
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## From Jenny Bryan's googlesheets package
## From infer package
## https://github.com/tidymodels/infer/blob/master/R/infer.R
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(".",
      "assessment",
      "below_cutoff",
      "conf_high",
      "conf_low",
      "covariate",
      "estimate",
      "estimate_ci",
      "form1",
      "log_odds",
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
      "vars"
    )
  )
}
