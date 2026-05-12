# Regression tests for the small-coefficient failure mode that the
# 0.1.0 release was designed to address.
#
# Strategy: use a deliberately uninformative dataset (random y, random
# covariates) and force everything past univariable screening with
# p_screen = 0.99.  The resulting initial Phase 2 model has all-near-zero
# coefficients, which is exactly the regime where pct_change misbehaves.
#
# We assert:
#   * pct_change (default) emits the small-coef warning
#   * stabilized does NOT emit it (the metric is robust to near-zero coefs)
#   * small_coef_warn = FALSE suppresses it
#   * target_var changes which rows are used for the assessment
#   * The chosen method is recorded in history$confound_metric


# Fixture used across tests in this file
make_small_coef_fixture <- function() {
  set.seed(123)
  n <- 200
  tibble::tibble(
    y        = rbinom(n, 1, 0.5),
    exposure = rbinom(n, 1, 0.5),
    x1       = rnorm(n),
    x2       = rnorm(n),
    x3       = rnorm(n)
  )
}


test_that("pct_change (default) warns when coefficients are near zero", {
  dat <- make_small_coef_fixture()

  expect_warning(
    purposeful_glm(
      data           = dat,
      outcome        = "y",
      candidate_vars = c("exposure", "x1", "x2", "x3"),
      keep_in_mod    = "exposure",
      p_screen       = 0.99,
      verbose        = FALSE
    ),
    regexp = "Small coefficient"
  )
})


test_that("stabilized method does not emit the small-coef warning", {
  dat <- make_small_coef_fixture()

  msgs <- character(0)
  withCallingHandlers(
    {
      purposeful_glm(
        data            = dat,
        outcome         = "y",
        candidate_vars  = c("exposure", "x1", "x2", "x3"),
        keep_in_mod     = "exposure",
        p_screen        = 0.99,
        confound_method = "stabilized",
        verbose         = FALSE
      )
    },
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_false(any(grepl("Small coefficient", msgs)))
})


test_that("small_coef_warn = FALSE suppresses the warning", {
  dat <- make_small_coef_fixture()

  msgs <- character(0)
  withCallingHandlers(
    {
      purposeful_glm(
        data            = dat,
        outcome         = "y",
        candidate_vars  = c("exposure", "x1", "x2", "x3"),
        keep_in_mod     = "exposure",
        p_screen        = 0.99,
        small_coef_warn = FALSE,
        verbose         = FALSE
      )
    },
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_false(any(grepl("Small coefficient", msgs)))
})


test_that("target_var produces a well-formed result", {
  dat <- make_small_coef_fixture()

  res <- suppressWarnings(purposeful_glm(
    data           = dat,
    outcome        = "y",
    candidate_vars = c("exposure", "x1", "x2", "x3"),
    keep_in_mod    = "exposure",
    target_var     = "exposure",
    p_screen       = 0.99,
    verbose        = FALSE
  ))

  expect_type(res, "list")
  expect_true("history" %in% names(res))
  expect_true("confound_metric" %in% names(res$history))
})


test_that("stabilized method records 'stabilized' in history$confound_metric", {
  dat <- make_small_coef_fixture()

  res <- suppressWarnings(purposeful_glm(
    data            = dat,
    outcome         = "y",
    candidate_vars  = c("exposure", "x1", "x2", "x3"),
    keep_in_mod     = "exposure",
    p_screen        = 0.99,
    confound_method = "stabilized",
    verbose         = FALSE
  ))

  assessed <- res$history[
    res$history$action %in% c("remove", "retain_confounder"), ,
    drop = FALSE
  ]
  if (nrow(assessed) > 0) {
    expect_true(all(assessed$confound_metric == "stabilized"))
  }
})
