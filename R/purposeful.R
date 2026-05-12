#' purposeful: Purposeful selection for regression models
#'
#' Application of the purposeful selection procedure developed by Hosmer and
#' Lemeshow for selecting predictors in regression models.  The package
#' provides functions for logistic regression (\code{\link{purposeful_glm}}),
#' Cox proportional hazards regression (\code{\link{purposeful_cox}}), and
#' ordinary linear regression (\code{\link{purposeful_lm}}).
#'
#' @examples
#' # See ?purposeful_glm, ?purposeful_cox, ?purposeful_lm for full examples.
#'
"_PACKAGE"

## quiets concerns of R CMD check re: bare column names in dplyr pipelines
## and the `.` placeholder used by magrittr
## From Jenny Bryan's googlesheets package / infer package
## https://github.com/tidymodels/infer/blob/master/R/infer.R
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(".",
      ## ---- column names referenced bare inside dplyr verbs --------------
      "covariate",
      "estimate",
      "std.error",
      "term",
      "p_value",
      "pct_change",
      "abs_change",
      "stabilized_change",
      ## ---- new variables created in history rows + engine internals -----
      "action",
      "beta_new",
      "beta_old",
      "confound_metric",
      "confounding",
      "max_change",
      "n_vars",
      "removed_var",
      "se_old",
      "vars",
      "worst_p",
      "worst_var"
    )
  )
}
