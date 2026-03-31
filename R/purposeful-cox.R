
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
#' @param confound_pct Numeric. Maximum acceptable proportional change in any
#'   log-hazard coefficient before a variable is retained as a confounder.
#'   Default \code{0.20}.
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
#'     confound_pct   = 0.20,
#'     verbose        = TRUE
#'   )
#'
#'   res$final_vars
#'   res$final_tidy
#' }

purposeful_cox <- function(data,
                           outcome,
                           candidate_vars,
                           keep_in_mod  = NULL,
                           p_screen     = 0.25,
                           p_remove     = 0.05,
                           confound_pct = 0.20,
                           verbose      = TRUE) {

  # Note: the `family` argument is intentionally absent here. Cox models do
  # not use a family; passing one would be misleading.  The engine accepts a
  # family argument internally but ignores it when model = "cox".

  .purposeful_select_engine(
    data           = data,
    outcome        = outcome,
    candidate_vars = candidate_vars,
    keep_in_mod    = keep_in_mod,
    model          = "cox",
    family         = stats::binomial(),  # placeholder; ignored by coxph
    p_screen       = p_screen,
    p_remove       = p_remove,
    confound_pct   = confound_pct,
    verbose        = verbose
  )
}
