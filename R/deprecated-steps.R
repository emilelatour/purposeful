
# =========================================================================
# Deprecated step functions
#
# purposeful_step_1(), purposeful_step_2(), purposeful_step_3() are kept
# here so that existing user code continues to run with a clear deprecation
# warning rather than an error.  They will be removed in a future version.
#
# New code should use purposeful_glm(), purposeful_cox(), or purposeful_lm().
# =========================================================================


#' Purposeful selection step 1 (deprecated)
#'
#' @description
#' Deprecated. Please use \code{\link{purposeful_glm}} instead, which runs
#' all three steps in a single call and also supports Cox and linear
#' regression.
#'
#' @inheritParams purposeful_glm
#' @param predictors Character vector of candidate predictors.
#' @param ref_level Character string. Factor level of the outcome that
#'   corresponds to the event (1). If \code{NULL} (default) the model
#'   determines the reference level.
#' @param cutoff_value Numeric. Univariable screening threshold passed to
#'   \code{p_screen}. Default \code{0.25}.
#' @param format Logical. Format results for display. Default \code{FALSE}.
#' @param exponentiate Logical. Exponentiate estimates. Default \code{TRUE}.
#' @param digits Integer. Decimal places to round to. Default \code{1}.
#' @param conf_level Confidence level for intervals. Default \code{0.95}.
#' @param ... Additional arguments (ignored).
#'
#' @return A tibble of univariable results.
#'
#' @importFrom dplyr arrange coalesce if_else mutate mutate_all mutate_at
#'   na_if rename select vars
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom stats formula
#' @importFrom tibble tibble
#' @importFrom tidyr crossing unnest
#'
#' @export
#' @keywords internal
purposeful_step_1 <- function(data,
                              outcome,
                              predictors,
                              ref_level    = NULL,
                              cutoff_value = 0.25,
                              conf_level   = 0.95,
                              format       = FALSE,
                              exponentiate = TRUE,
                              digits       = 1, ...) {

  .Deprecated(
    new     = "purposeful_glm",
    package = "purposeful",
    msg     = paste0(
      "'purposeful_step_1()' is deprecated.\n",
      "Use 'purposeful_glm()' instead, which runs all three steps together.\n",
      "See ?purposeful_glm for details."
    )
  )

  lhs <- if (!is.null(ref_level)) {
    glue::glue("({outcome} == '{ref_level}')")
  } else {
    glue::glue("{outcome}")
  }

  rhs     <- paste(predictors, collapse = " + ")
  form    <- glue::glue("{lhs} ~ {rhs}")
  outcome <- lhs

  vars_order <- predictors

  univ_res <- tidyr::crossing(outcome, predictors) %>%
    mutate(predictors = factor(predictors, levels = vars_order)) %>%
    dplyr::arrange(predictors) %>%
    mutate(
      predictors = as.character(predictors),
      formula_str = paste(outcome, "~", predictors),
      res_univ = purrr::map(
        .x = formula_str,
        .f = ~ .do_logistic_univ(
          formula      = .x,
          data         = data,
          format       = format,
          exponentiate = exponentiate
        )
      )
    ) %>%
    dplyr::select(res_univ) %>%
    tidyr::unnest(cols = res_univ) %>%
    mutate(
      below_cutoff = dplyr::if_else(
        p_value_lrt < cutoff_value,
        glue::glue("Less than {cutoff_value}"),
        NA_character_
      )
    ) %>%
    mutate_at(
      .vars = vars(p_value_wald, p_value_lrt),
      .funs = list(~ .mypval(.))
    )

  if (format == TRUE) {

    univ_res <- univ_res %>%
      mutate_at(
        .vars = vars(estimate, lower_ci, upper_ci),
        .funs = list(~ .myround(., digits = digits))
      ) %>%
      mutate(
        estimate_ci = dplyr::if_else(
          is.na(p_value_wald),
          NA_character_,
          as.character(
            glue::glue("{estimate} ({lower_ci} to {upper_ci})")
          )
        ),
        ref         = dplyr::na_if(ref, ""),
        estimate_ci = dplyr::coalesce(ref, estimate_ci),
        p_value     = dplyr::coalesce(p_value_wald, p_value_lrt)
      ) %>%
      dplyr::select(covariate, term, estimate_ci, p_value, below_cutoff) %>%
      mutate_all(.tbl = ., .funs = list(~ replace(., is.na(.), "")))

    if (exponentiate) {
      univ_res <- dplyr::rename(univ_res, or_ci = estimate_ci)
    } else {
      univ_res <- dplyr::rename(univ_res, log_odds_ci = estimate_ci)
    }

  } else {

    if (exponentiate) {
      univ_res <- dplyr::rename(univ_res, or = estimate)
    } else {
      univ_res <- dplyr::rename(univ_res, log_odds = estimate)
    }

  }

  univ_res
}


#' Purposeful selection step 2 (deprecated)
#'
#' @description
#' Deprecated. Please use \code{\link{purposeful_glm}} instead.
#'
#' @inheritParams purposeful_step_1
#' @param keep_in_mod Character vector. Variables to force into the model.
#' @param ... Additional arguments (ignored).
#'
#' @return A list.
#'
#' @importFrom broom tidy
#' @importFrom car Anova
#' @importFrom dplyr filter pull rename
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom stats anova as.formula binomial glm
#' @importFrom tibble as_tibble
#'
#' @export
#' @keywords internal
purposeful_step_2 <- function(data,
                              outcome,
                              predictors,
                              keep_in_mod  = NULL,
                              ref_level    = NULL,
                              format       = FALSE,
                              conf_level   = 0.95,
                              exponentiate = TRUE,
                              digits       = 2, ...) {

  .Deprecated(
    new     = "purposeful_glm",
    package = "purposeful",
    msg     = paste0(
      "'purposeful_step_2()' is deprecated.\n",
      "Use 'purposeful_glm()' instead, which runs all three steps together.\n",
      "See ?purposeful_glm for details."
    )
  )

  vars0 <- union(predictors, keep_in_mod)

  lhs <- if (!is.null(ref_level)) {
    glue::glue("({outcome} == '{ref_level}')")
  } else {
    glue::glue("{outcome}")
  }

  rhs0  <- paste(vars0, collapse = " + ")
  form0 <- glue::glue("{lhs} ~ {rhs0}")

  mod0 <- stats::glm(stats::as.formula(form0),
                     data   = data,
                     family = stats::binomial(link = "logit"))

  res0 <- mod0 %>% broom::tidy() %>% janitor::clean_names()
  lrt0 <- car::Anova(mod0) %>% broom::tidy() %>% janitor::clean_names()

  pred0 <- dplyr::pull(lrt0, term)
  pred1 <- dplyr::pull(dplyr::filter(lrt0, p_value < 0.05), term)
  pred1 <- union(pred1, keep_in_mod)
  rhs1  <- if (length(pred1) == 0L) "1" else paste(pred1, collapse = " + ")
  form1 <- glue::glue("{lhs} ~ {rhs1}")

  names(mod0$model)[1] <- outcome

  mod1 <- stats::glm(stats::as.formula(form1),
                     data   = mod0$model,
                     family = stats::binomial(link = "logit"))

  res1 <- mod1 %>% broom::tidy() %>% janitor::clean_names()
  lrt1 <- if (length(pred1) == 0L) {
    "The model contains only an intercept"
  } else {
    car::Anova(mod1) %>% broom::tidy() %>% janitor::clean_names()
  }

  lr_test <- stats::anova(mod0, mod1, test = "LRT") %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::rename(p_value = pr_chi)

  list(
    covariates_full_model = pred0,
    covariates_red_model  = pred1,
    results_full_model    = res0,
    results_red_model     = res1,
    lrt_full_model        = lrt0,
    lrt_red_model         = lrt1,
    lr_test_results       = lr_test
  )
}


#' Purposeful selection step 3 (deprecated)
#'
#' @description
#' Deprecated. Please use \code{\link{purposeful_glm}} instead -- confounding
#' checks are now integrated into the selection loop.
#'
#' @inheritParams purposeful_step_1
#' @param potential_confounders Character vector of variables removed in
#'   steps 1 and 2 to test as potential confounders.
#' @param ... Additional arguments (ignored).
#'
#' @return A tibble.
#'
#' @importFrom dplyr arrange if_else left_join select
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom scales percent
#' @importFrom tibble tibble
#' @importFrom tidyr crossing unnest
#'
#' @export
#' @keywords internal
purposeful_step_3 <- function(data,
                              outcome,
                              ref_level             = NULL,
                              predictors,
                              potential_confounders = NULL, ...) {

  .Deprecated(
    new     = "purposeful_glm",
    package = "purposeful",
    msg     = paste0(
      "'purposeful_step_3()' is deprecated.\n",
      "Use 'purposeful_glm()' instead -- confounding checks are integrated.\n",
      "See ?purposeful_glm for details."
    )
  )

  lhs <- if (!is.null(ref_level)) {
    glue::glue("({outcome} == '{ref_level}')")
  } else {
    glue::glue("{outcome}")
  }

  rhs     <- paste(predictors, collapse = " + ")
  form0   <- glue::glue("{lhs} ~ {rhs}")
  outcome <- lhs

  vars_order <- potential_confounders

  res <- tidyr::crossing(form0, potential_confounders) %>%
    mutate(
      potential_confounders = factor(potential_confounders, levels = vars_order)
    ) %>%
    dplyr::arrange(potential_confounders) %>%
    mutate(
      potential_confounders = as.character(potential_confounders),
      form1 = paste0(form0, " + ", potential_confounders),
      res_0 = purrr::map(
        .x = form0,
        .f = ~ .do_lr_for_confounding(data = data, form = .x,
                                       keep = predictors)
      ),
      res_1 = purrr::map(
        .x = form1,
        .f = ~ .do_lr_for_confounding(data = data, form = .x,
                                       keep = predictors)
      )
    )

  res_0 <- tidyr::unnest(res, cols = c(res_0)) %>%
    dplyr::select(potential_confounders, term, log_odds0 = log_odds)

  res_1 <- tidyr::unnest(res, cols = c(res_1)) %>%
    dplyr::select(potential_confounders, term, log_odds1 = log_odds)

  res_0 %>%
    dplyr::left_join(res_1, by = c("potential_confounders", "term")) %>%
    mutate(
      pct_change = (log_odds1 - log_odds0) / log_odds0,
      assessment = dplyr::if_else(abs(pct_change) > 0.20, "Potential", ""),
      pct_change = scales::percent(pct_change)
    ) %>%
    dplyr::select(potential_confounders, term, log_odds0, log_odds1,
                  pct_change, assessment)
}
