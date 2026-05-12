
# =========================================================================
# Internal helpers for purposeful selection
# Not exported -- prefixed with "." per package convention
# =========================================================================


# ---- .fit_purposeful_model -----------------------------------------------
#
# Fit a GLM, Cox, or LM from a character outcome and character predictor
# vector.  The outcome string is used as-is on the left-hand side, which
# lets callers pass a Surv() expression for Cox models without any special
# handling.
#
#' @importFrom stats as.formula glm lm binomial
.fit_purposeful_model <- function(data,
                                  outcome,
                                  predictors,
                                  model  = c("glm", "cox", "lm"),
                                  family = stats::binomial()) {

  model <- match.arg(model)

  if (length(predictors) == 0L) {
    stop("predictors must contain at least one variable.", call. = FALSE)
  }

  if (model == "cox" && !requireNamespace("survival", quietly = TRUE)) {
    stop(
      "Package 'survival' is required for Cox regression. ",
      "Install it with: install.packages('survival')",
      call. = FALSE
    )
  }

  fml <- stats::as.formula(
    paste(outcome, "~", paste(predictors, collapse = " + "))
  )

  switch(model,
    glm = stats::glm(formula = fml, data = data, family = family),
    cox = survival::coxph(formula = fml, data = data, x = TRUE),
    lm  = stats::lm(formula = fml, data = data)
  )
}


# ---- .get_univar_pvalue ----------------------------------------------------
#
# Compute a single overall p-value for a univariable model without using
# drop1().
#
# drop1() is the right tool for MULTIVARIABLE models (one variable removed at
# a time from a shared complete-case dataset), but for UNIVARIABLE models it
# must refit a null model via update(), which can see a different number of
# rows when the predictor has missing values -- producing the error:
#   "number of rows in use has changed: remove missing values?"
#
# Instead we extract the LRT p-value directly from the fitted object:
#   GLM : LRT chi-square via null.deviance vs deviance
#   Cox : LRT chi-square via fit$loglik
#   LM  : F-test via summary()$fstatistic
#
# All three use information already stored in the fit -- no refitting needed.
#
#' @importFrom stats pchisq pf coef
.get_univar_pvalue <- function(fit, model = c("glm", "cox", "lm")) {

  model <- match.arg(model)

  if (model == "glm") {

    lrt_stat <- fit$null.deviance - fit$deviance
    lrt_df   <- fit$df.null - fit$df.residual
    stats::pchisq(lrt_stat, df = lrt_df, lower.tail = FALSE)

  } else if (model == "cox") {

    # coxph stores loglik as length-2: [1] = null loglik, [2] = fitted loglik
    lrt_stat <- 2 * diff(fit$loglik)
    lrt_df   <- length(stats::coef(fit))
    stats::pchisq(lrt_stat, df = lrt_df, lower.tail = FALSE)

  } else {  # lm

    fs <- summary(fit)$fstatistic
    if (is.null(fs)) return(NA_real_)  # intercept-only model
    stats::pf(fs["value"], fs["numdf"], fs["dendf"], lower.tail = FALSE)

  }
}


# ---- .get_term_tests -------------------------------------------------------
#
# Variable-level p-values via drop1() for MULTIVARIABLE models.
#
# drop1() gives one p-value per *variable* (not per coefficient level), which
# is essential for multi-level factors.  This is only called on multivariable
# models where all predictors share the same complete-case dataset, so the
# row-count issue that affects univariable screening does not arise here.
#
# Model-specific test types:
#   GLM / Cox : likelihood-ratio chi-square ("Chisq")
#   LM        : F-test                      ("F")
#
#' @importFrom rlang .data
#' @importFrom stats drop1
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter transmute
.get_term_tests <- function(fit, model = c("glm", "cox", "lm")) {

  model     <- match.arg(model)
  test_type <- if (model == "lm") "F" else "Chisq"
  p_col     <- if (model == "lm") "Pr(>F)" else "Pr(>Chi)"

  x <- stats::drop1(fit, test = test_type)

  tibble::as_tibble(
    data.frame(covariate = rownames(x), x, row.names = NULL,
               check.names = FALSE)
  ) %>%
    dplyr::filter(covariate != "<none>") %>%
    dplyr::transmute(
      covariate,
      p_value = .data[[p_col]]
    )
}


# ---- .get_beta_table -------------------------------------------------------
#
# Extract log-scale coefficients and their standard errors from a fitted
# model.  Cox models have no intercept; GLM and LM do (and it is filtered
# out).
#
# The standard error is kept so that .compare_coefficients() can compute the
# stabilized change metric (Delta beta / SE), which is robust to small
# coefficients in a way that percent change is not.
#
#' @importFrom broom tidy
#' @importFrom dplyr filter select
.get_beta_table <- function(fit, model = c("glm", "cox", "lm")) {

  model <- match.arg(model)

  res <- broom::tidy(fit) %>%
    dplyr::select(term, estimate, std.error)

  if (model %in% c("glm", "lm")) {
    res <- dplyr::filter(res, term != "(Intercept)")
  }

  res
}


# ---- .map_terms_to_covariates ----------------------------------------------
#
# Maps coefficient term names (e.g. "genderfemale") back to the parent
# variable name (e.g. "gender") using longest-prefix matching.
#
# Longest-prefix wins, so overlapping names such as "age" and "age_group"
# resolve correctly:
#   "age"          -> "age"
#   "age_groupold" -> "age_group"   (9 chars > 3 chars)
#
#' @importFrom dplyr mutate
.map_terms_to_covariates <- function(beta_tbl, candidate_vars) {

  get_parent_var <- function(term, vars) {
    escaped  <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", vars)
    patterns <- paste0("^", escaped)
    hits     <- vars[vapply(patterns,
                            function(p) grepl(p, term),
                            logical(1L))]
    if (length(hits) == 0L) {
      NA_character_
    } else {
      hits[which.max(nchar(hits))]
    }
  }

  beta_tbl %>%
    dplyr::mutate(
      covariate = vapply(term, get_parent_var, character(1L),
                         vars = candidate_vars)
    )
}


# ---- .compare_coefficients -------------------------------------------------
#
# Compare log-scale coefficients between two nested models and return a tibble
# of all confounding-assessment metrics for each remaining term.  The engine
# picks which metric to threshold on based on `confound_method`.
#
# Metrics returned:
#   pct_change         abs((beta_new - beta_old) / beta_old)
#                      Textbook Hosmer-Lemeshow.  Misbehaves when |beta_old|
#                      is small: tiny absolute shifts get amplified into huge
#                      percentages.
#
#   abs_change         abs(beta_new - beta_old)
#                      Absolute change on the model's natural (log) scale.
#                      Threshold has the same units as the coefficient, so
#                      0.10 corresponds to ~10% change in OR/HR.
#
#   stabilized_change  abs(beta_new - beta_old) / se_old
#                      Change scaled by the precision of the old coefficient.
#                      Robust to small coefficients by construction; a noisy
#                      coefficient needs more absolute movement to look like
#                      one SE of change.
#
#' @importFrom dplyr rename inner_join mutate if_else select
.compare_coefficients <- function(old_fit,
                                  new_fit,
                                  model          = c("glm", "cox", "lm"),
                                  candidate_vars) {

  model <- match.arg(model)

  old_beta <- .get_beta_table(old_fit, model) %>%
    .map_terms_to_covariates(candidate_vars) %>%
    dplyr::rename(beta_old = estimate, se_old = std.error)

  new_beta <- .get_beta_table(new_fit, model) %>%
    .map_terms_to_covariates(candidate_vars) %>%
    dplyr::select(-std.error) %>%
    dplyr::rename(beta_new = estimate)

  old_beta %>%
    dplyr::inner_join(new_beta, by = c("term", "covariate")) %>%
    dplyr::mutate(
      pct_change = dplyr::if_else(
        is.na(beta_old) | beta_old == 0,
        NA_real_,
        abs((beta_new - beta_old) / beta_old)
      ),
      abs_change = abs(beta_new - beta_old),
      stabilized_change = dplyr::if_else(
        is.na(se_old) | se_old == 0,
        NA_real_,
        abs(beta_new - beta_old) / se_old
      )
    )
}


# ---- .screen_univariable_terms ---------------------------------------------
#
# Fit one model per candidate variable and return a variable-level p-value
# via .get_univar_pvalue().
#
# Each model is fit on rows where that predictor is non-missing, which mirrors
# standard univariable screening practice and prevents the drop1() row-count
# mismatch error that occurs for Cox models with missing predictors.
#
# Note: candidate_vars should be simple column names, not expressions.
#
#' @importFrom purrr map
#' @importFrom dplyr bind_rows arrange
#' @importFrom tibble tibble
.screen_univariable_terms <- function(data,
                                      outcome,
                                      candidate_vars,
                                      model  = c("glm", "cox", "lm"),
                                      family = stats::binomial()) {

  model <- match.arg(model)

  purrr::map(candidate_vars, function(v) {

    sub_data <- if (v %in% names(data)) {
      data[!is.na(data[[v]]), , drop = FALSE]
    } else {
      data
    }

    fit <- .fit_purposeful_model(
      data       = sub_data,
      outcome    = outcome,
      predictors = v,
      model      = model,
      family     = family
    )

    tibble::tibble(
      covariate = v,
      p_value   = .get_univar_pvalue(fit, model)
    )
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(p_value)
}


# ---- .make_history_row -----------------------------------------------------
#
# Small helper to build a single-row history tibble.
#
# `confound_metric` records which metric was thresholded against
# (pct_change, abs_change, or stabilized_change), and `max_change` is the
# numeric value of that metric at the worst-aggregating coefficient.  The old
# `max_pct_change` column is gone -- use `max_change` together with
# `confound_metric` to interpret the number.
#
#' @importFrom tibble tibble
.make_history_row <- function(step, action, removed_var, worst_var, worst_p,
                              confounding, confound_metric, max_change,
                              current_vars) {
  tibble::tibble(
    step             = as.integer(step),
    action           = action,
    removed_var      = removed_var,
    worst_var        = worst_var,
    worst_p          = worst_p,
    confounding      = confounding,
    confound_metric  = confound_metric,
    max_change       = max_change,
    n_vars           = length(current_vars),
    vars             = paste(current_vars, collapse = ", ")
  )
}


# ---- .purposeful_select_engine ---------------------------------------------
#
# Shared backbone called by purposeful_glm(), purposeful_cox(),
# purposeful_lm().
#
# Three phases:
#
#   1. Univariable screening
#      Fit one model per candidate variable.  Retain those with p < p_screen.
#      Variables in keep_in_mod are always retained regardless.
#
#   2. Backward elimination with integrated confounding check
#      At each step, find the non-protected variable with the largest drop1()
#      p-value.  If p >= p_remove, attempt removal.  If removal shifts
#      coefficients by more than confound_threshold (under the chosen
#      confound_method), retain the variable as a confounder and continue --
#      do NOT stop.  This ensures other non-significant variables are still
#      evaluated.
#
#      When target_var is supplied, only that variable's coefficients are
#      assessed for change; otherwise all remaining coefficients are.
#
#   3. Final model assembly
#
#' @importFrom dplyr filter pull arrange desc bind_rows
#' @importFrom tibble tibble
#' @importFrom broom tidy
#' @importFrom stats complete.cases
.purposeful_select_engine <- function(data,
                                      outcome,
                                      candidate_vars,
                                      keep_in_mod        = NULL,
                                      model              = c("glm", "cox", "lm"),
                                      family             = stats::binomial(),
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

  model           <- match.arg(model)
  confound_method <- match.arg(confound_method)

  # Legacy: confound_pct -> confound_threshold (one-time message)
  if (!is.null(confound_pct)) {
    if (is.null(confound_threshold)) {
      confound_threshold <- confound_pct
      message(
        "`confound_pct` is deprecated; use `confound_threshold` instead. ",
        "Mapping the value through this call."
      )
    } else {
      stop("Supply either `confound_threshold` or the legacy `confound_pct`, not both.",
           call. = FALSE)
    }
  }

  # Method-aware default threshold
  if (is.null(confound_threshold)) {
    confound_threshold <- switch(confound_method,
      pct_change = 0.20,   # 20% change in any coefficient
      abs_change = 0.10,   # 0.10 on the log scale (~10% in OR/HR)
      stabilized = 1.00    # one SE of movement
    )
  }

  # target_var validation
  if (!is.null(target_var)) {
    if (length(target_var) != 1L || !is.character(target_var)) {
      stop("`target_var` must be a single variable name (character).",
           call. = FALSE)
    }
    if (!target_var %in% c(candidate_vars, keep_in_mod)) {
      warning(
        "`target_var` (\"", target_var, "\") is in neither candidate_vars ",
        "nor keep_in_mod; the confounding check will have no rows to assess.",
        call. = FALSE
      )
    }
  }

  # Map method name to the column in beta_compare
  metric_col <- switch(confound_method,
    pct_change = "pct_change",
    abs_change = "abs_change",
    stabilized = "stabilized_change"
  )

  # ------------------------------------------------------------------
  # Phase 1: Univariable screening
  # ------------------------------------------------------------------

  uni_screen <- .screen_univariable_terms(
    data           = data,
    outcome        = outcome,
    candidate_vars = candidate_vars,
    model          = model,
    family         = family
  )

  screened_vars <- uni_screen %>%
    dplyr::filter(!is.na(p_value), p_value < p_screen) %>%
    dplyr::pull(covariate)

  screened_vars <- union(screened_vars, keep_in_mod)

  if (verbose) {
    cat("\n--- Phase 1: Univariable screening (p <", p_screen, ") ---\n")
    print(uni_screen)
    cat("\nVariables passing screen:", paste(screened_vars, collapse = ", "), "\n")
    if (!is.null(keep_in_mod)) {
      cat("Force-included (keep_in_mod):",
          paste(keep_in_mod, collapse = ", "), "\n")
    }
  }

  if (length(screened_vars) == 0L) {
    if (verbose) cat("\nNo variables passed screening. Stopping.\n")
    return(list(
      uni_screen       = uni_screen,
      screened_vars    = character(0L),
      history          = tibble::tibble(),
      final_vars       = character(0L),
      final_fit        = NULL,
      final_term_tests = tibble::tibble(),
      final_tidy       = tibble::tibble()
    ))
  }

  # ------------------------------------------------------------------
  # Phase 2: Backward elimination with integrated confounding check
  # ------------------------------------------------------------------

  # Restrict to complete cases across all screened variables before entering
  # the elimination loop.  This mirrors what purposeful_step_2() did via
  # mod0$model and is essential for drop1() to work correctly:
  # drop1() refits the model after removing each term, and if different
  # predictors have different missing-data patterns, the row count changes
  # between fits, causing the error "number of rows in use has changed".
  # By using a single complete-case dataset throughout Phase 2, every model
  # in the loop sees identical rows.
  #
  # Note: screened_vars contains only simple column names (not Surv()
  # expressions), so the NA check below is safe.
  phase2_vars_in_data <- screened_vars[screened_vars %in% names(data)]
  complete_rows       <- stats::complete.cases(data[, phase2_vars_in_data,
                                                    drop = FALSE])
  phase2_data         <- data[complete_rows, , drop = FALSE]

  if (verbose && sum(!complete_rows) > 0L) {
    cat("\n  Note:", sum(!complete_rows),
        "row(s) with missing values excluded from Phase 2.\n")
  }

  current_vars   <- screened_vars
  protected_vars <- keep_in_mod   # grows when confounders are retained
  history        <- list()
  step           <- 0L

  if (verbose) {
    cat(
      "\n--- Phase 2: Backward elimination + confounding check",
      "(p_remove =", p_remove,
      "| confound_method =", confound_method,
      "| threshold =", confound_threshold
    )
    if (!is.null(target_var)) cat(" | target_var =", target_var)
    cat(") ---\n")
  }

  # ----- Small-coefficient warning (pct_change only) --------------------
  # Emitted once, against the initial fit (current_vars is screened_vars at
  # this point).  If any coefficient is small (|beta| < 2 * SE), pct_change
  # can amplify tiny absolute shifts into huge percentages.  Warn the user
  # at the start of Phase 2 rather than per-step (which would be noisy).
  if (small_coef_warn && confound_method == "pct_change") {
    initial_fit <- .fit_purposeful_model(
      data       = phase2_data,
      outcome    = outcome,
      predictors = current_vars,
      model      = model,
      family     = family
    )
    initial_beta <- .get_beta_table(initial_fit, model)
    small_terms <- initial_beta[
      !is.na(initial_beta$std.error) &
        initial_beta$std.error > 0 &
        abs(initial_beta$estimate) < 2 * initial_beta$std.error,
      "term", drop = TRUE
    ]
    if (length(small_terms) > 0L) {
      warning(
        "Small coefficient(s) detected (|beta| < 2*SE) in the initial Phase 2 ",
        "model: ", paste(small_terms, collapse = ", "), ". ",
        "`confound_method = \"pct_change\"` can over-retain confounders when ",
        "coefficients are near zero. Consider `confound_method = \"stabilized\"`, ",
        "or set `target_var` to focus the check on a single coefficient. ",
        "Set `small_coef_warn = FALSE` to suppress this message.",
        call. = FALSE
      )
    }
  }

  repeat {

    step <- step + 1L

    current_fit <- .fit_purposeful_model(
      data       = phase2_data,
      outcome    = outcome,
      predictors = current_vars,
      model      = model,
      family     = family
    )

    current_term_tests <- .get_term_tests(current_fit, model) %>%
      dplyr::arrange(dplyr::desc(p_value))

    removable <- dplyr::filter(current_term_tests,
                               !covariate %in% protected_vars)

    if (nrow(removable) == 0L) {
      history[[step]] <- .make_history_row(
        step, "stop_all_protected",
        NA_character_, NA_character_, NA_real_, NA,
        confound_method, NA_real_, current_vars
      )
      if (verbose) {
        cat("\n  Step", step,
            ": all remaining variables are protected. Stopping.\n")
      }
      break
    }

    worst_var <- removable$covariate[1L]
    worst_p   <- removable$p_value[1L]

    if (verbose) {
      cat("\n  Step", step,
          "| vars:", paste(current_vars, collapse = ", "), "\n")
      cat("  Worst non-protected:", worst_var,
          "(p =", round(worst_p, 4L), ")\n")
    }

    if (!is.na(worst_p) && worst_p < p_remove) {
      history[[step]] <- .make_history_row(
        step, "stop_all_significant",
        NA_character_, worst_var, worst_p, NA,
        confound_method, NA_real_, current_vars
      )
      if (verbose) {
        cat("  All non-protected variables significant at p <",
            p_remove, ". Stopping.\n")
      }
      break
    }

    reduced_vars <- setdiff(current_vars, worst_var)

    if (length(reduced_vars) == 0L) {
      history[[step]] <- .make_history_row(
        step, "stop_minimum_vars",
        NA_character_, worst_var, worst_p, NA,
        confound_method, NA_real_, current_vars
      )
      if (verbose) cat("  Cannot reduce below one variable. Stopping.\n")
      break
    }

    reduced_fit <- .fit_purposeful_model(
      data       = phase2_data,
      outcome    = outcome,
      predictors = reduced_vars,
      model      = model,
      family     = family
    )

    beta_compare <- .compare_coefficients(
      old_fit        = current_fit,
      new_fit        = reduced_fit,
      model          = model,
      candidate_vars = current_vars
    )

    # Subset rows to assess: all coefficients, or just target_var's
    rows_for_assess <- if (!is.null(target_var)) {
      beta_compare[!is.na(beta_compare$covariate) &
                     beta_compare$covariate == target_var, , drop = FALSE]
    } else {
      beta_compare
    }

    # If target_var was supplied but isn't in the current model anymore (e.g.,
    # removed in an earlier step despite the warning), there are no rows.  Skip
    # the confounding check in that case -- there's nothing to assess.
    if (nrow(rows_for_assess) == 0L) {
      max_change  <- NA_real_
      confounding <- FALSE
    } else {
      max_change <- suppressWarnings(max(rows_for_assess[[metric_col]],
                                         na.rm = TRUE))
      if (is.infinite(max_change)) max_change <- NA_real_
      confounding <- !is.na(max_change) && max_change > confound_threshold
    }

    if (verbose) {
      change_label <- switch(confound_method,
        pct_change = "max % change",
        abs_change = "max abs change",
        stabilized = "max SE-scaled change"
      )
      formatted <- if (is.na(max_change)) {
        "NA"
      } else if (confound_method == "pct_change") {
        paste0(round(max_change * 100, 1), "%")
      } else {
        format(round(max_change, 3), nsmall = 3)
      }
      cat("  ", change_label, " on removing ", worst_var, ": ",
          formatted, "\n", sep = "")
      cat("  Confounding detected:", confounding, "\n")
    }

    if (confounding) {
      protected_vars <- c(protected_vars, worst_var)
      history[[step]] <- .make_history_row(
        step, "retain_confounder",
        NA_character_, worst_var, worst_p, TRUE,
        confound_method, max_change, current_vars
      )
      if (verbose) {
        cat("  Retaining", worst_var,
            "as confounder. Continuing with remaining variables.\n")
      }
    } else {
      current_vars <- reduced_vars
      history[[step]] <- .make_history_row(
        step, "remove",
        worst_var, worst_var, worst_p, FALSE,
        confound_method, max_change, reduced_vars
      )
      if (verbose) cat("  Removing", worst_var, "\n")
    }
  }

  # ------------------------------------------------------------------
  # Phase 3: Final model
  # ------------------------------------------------------------------

  final_fit <- .fit_purposeful_model(
    data       = phase2_data,
    outcome    = outcome,
    predictors = current_vars,
    model      = model,
    family     = family
  )

  final_term_tests <- .get_term_tests(final_fit, model)

  exponentiate <- switch(model,
    glm = identical(family$family, "binomial"),
    cox = TRUE,
    lm  = FALSE
  )

  final_tidy <- broom::tidy(final_fit,
                             conf.int     = TRUE,
                             exponentiate = exponentiate)

  if (verbose) {
    cat("\n--- Final model variables:",
        paste(current_vars, collapse = ", "), "---\n")
  }

  list(
    uni_screen       = uni_screen,
    screened_vars    = screened_vars,
    history          = dplyr::bind_rows(history),
    final_vars       = current_vars,
    final_fit        = final_fit,
    final_term_tests = final_term_tests,
    final_tidy       = final_tidy
  )
}
