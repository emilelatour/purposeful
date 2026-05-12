
#' @title
#' Purposeful selection for Cox proportional hazards models
#'
#' @description
#' Applies the purposeful selection procedure of Hosmer and Lemeshow to a Cox
#' proportional hazards model fitted via \code{\link[survival]{coxph}}.  The
#' three phases (univariable screening, backward elimination with confounding
#' check, final model) are identical to \code{\link{purposeful_glm}}; only
#' the model-fitting call differs.
#'
#' Variable-level p-values from \code{drop1()} are used throughout.
#'
#' The \code{survival} package must be installed but need not be attached.
#'
#' @references
#' Hosmer DW, Lemeshow S, May S (2008) \emph{Applied Survival Analysis},
#' 2nd ed.  John Wiley & Sons, Inc.
#'
#' @param data A data frame or tibble containing all variables.
#' @param outcome Character string.  A \code{Surv()} expression written as a
#'   string, e.g. \code{"Surv(time, status)"}.  This is used as-is on the
#'   left-hand side of the model formula.
#' @param candidate_vars Character vector.  Variables to consider as
#'   predictors.
#' @param keep_in_mod Character vector or \code{NULL}.  Variables to retain
#'   regardless of statistical significance.  Default is \code{NULL}.
#' @param p_screen Numeric. Univariable screening threshold.  Default
#'   \code{0.25}.
#' @param p_remove Numeric. Elimination threshold.  Default \code{0.05}.
#' @param confound_method Character.  Which metric to use when assessing
#'   coefficient change during the confounding check.  See
#'   \code{\link{purposeful_glm}} for details.  Default \code{"pct_change"}.
#' @param confound_threshold Numeric or \code{NULL}.  Threshold for the
#'   chosen \code{confound_method}.  If \code{NULL}, a method-aware default
#'   is used (0.20 for pct_change, 0.10 for abs_change, 1.00 for
#'   stabilized).
#' @param target_var Character or \code{NULL}.  When supplied, the
#'   confounding check is computed only on this variable's coefficients,
#'   not all remaining coefficients.  Useful when you have a single exposure
#'   of interest.  Default \code{NULL}.
#' @param small_coef_warn Logical.  Warn when \code{"pct_change"} is used
#'   with small initial coefficients.  Default \code{TRUE}.
#' @param confound_pct (deprecated) Use \code{confound_threshold}.
#' @param verbose Logical.  Print a step-by-step log.  Default \code{TRUE}.
#'
#' @return A named list with the same structure as \code{\link{purposeful_glm}}:
#' \describe{
#'   \item{\code{uni_screen}}{Univariable screening results.}
#'   \item{\code{screened_vars}}{Variables passing screen.}
#'   \item{\code{history}}{Step-by-step elimination log.}
#'   \item{\code{final_vars}}{Variables in the final model.}
#'   \item{\code{final_fit}}{Fitted \code{coxph} object.}
#'   \item{\code{final_term_tests}}{Variable-level drop1() p-values.}
#'   \item{\code{final_tidy}}{Tidied coefficients (hazard ratios, with CI).}
#' }
#'
#' @export
#'
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   library(survival)
#'
#'   # lung dataset from the survival package
#'   dat <- survival::lung
#'   dat$status01 <- dat$status - 1L  # recode to 0/1
#'
#'   res <- purposeful_cox(
#'     data           = dat,
#'     outcome        = "Surv(time, status01)",
#'     candidate_vars = c("age", "sex", "ph.ecog", "wt.loss"),
#'     p_screen       = 0.25,
#'     p_remove       = 0.05,
#'     verbose        = TRUE
#'   )
#'
#'   res$final_vars
#'   res$final_tidy
#' }

purposeful_cox <- function(data,
                           outcome,
                           candidate_vars,
                           keep_in_mod        = NULL,
                           p_screen           = 0.25,
                           p_remove           = 0.05,
                           confound_method    = c("pct_change",
                                                  "abs_change",
                                                  "stabilized"),
                           confound_threshold = NULL,
                           target_var         = NULL,
                           small_coef_warn    = TRUE,
                           confound_pct       = NULL,
                           verbose            = TRUE) {

  confound_method <- match.arg(confound_method)

  # Note: the `family` argument is intentionally absent here. Cox models do
  # not use a family; passing one would be misleading.  The engine accepts a
  # family argument internally but ignores it when model = "cox".

  .purposeful_select_engine(
    data               = data,
    outcome            = outcome,
    candidate_vars     = candidate_vars,
    keep_in_mod        = keep_in_mod,
    model              = "cox",
    family             = stats::binomial(),  # placeholder; ignored by coxph
    p_screen           = p_screen,
    p_remove           = p_remove,
    confound_method    = confound_method,
    confound_threshold = confound_threshold,
    target_var         = target_var,
    small_coef_warn    = small_coef_warn,
    confound_pct       = confound_pct,
    verbose            = verbose
  )
}
