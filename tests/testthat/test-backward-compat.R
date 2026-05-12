# Smoke tests confirming the basic API still works and the history schema
# matches the new design (confound_metric + max_change, not max_pct_change).
# Uses the same simulated dataset as ?purposeful_glm for the main smoke test.


test_that("purposeful_glm returns the expected list structure", {
  set.seed(888)
  n <- 500
  age    <- abs(round(rnorm(n, 67, 14)))
  lac    <- abs(round(rnorm(n, 5, 3), 1))
  gender <- factor(rbinom(n, 1, 0.6), labels = c("male", "female"))
  wbc    <- abs(round(rnorm(n, 10, 3), 1))
  hb     <- abs(round(rnorm(n, 120, 40)))
  z      <- 0.1 * age - 0.02 * hb + lac - 10
  pr     <- 1 / (1 + exp(-z))
  mort   <- factor(rbinom(n, 1, pr), labels = c("alive", "dead"))
  dat    <- tibble::tibble(age, gender, lac, wbc, hb, mort)

  res <- purposeful_glm(
    data           = dat,
    outcome        = "mort",
    candidate_vars = c("age", "gender", "lac", "wbc", "hb"),
    family         = binomial(),
    verbose        = FALSE
  )

  # Standard return structure
  expect_type(res, "list")
  expect_named(
    res,
    c("uni_screen", "screened_vars", "history", "final_vars",
      "final_fit", "final_term_tests", "final_tidy")
  )
  expect_type(res$final_vars, "character")
  expect_gt(length(res$final_vars), 0)

  # The dataset is designed so age and lac are strong real predictors
  expect_true(all(c("age", "lac") %in% res$final_vars))
})


test_that("history has the new schema (confound_metric + max_change)", {
  set.seed(2024)
  n  <- 400
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  y  <- rbinom(n, 1, plogis(1.0 * x1 + 0.05 * x2))
  dat <- tibble::tibble(y, x1, x2, x3)

  res <- suppressWarnings(purposeful_glm(
    data           = dat,
    outcome        = "y",
    candidate_vars = c("x1", "x2", "x3"),
    verbose        = FALSE
  ))

  # New columns present
  expect_true(all(c("confound_metric", "max_change") %in% names(res$history)))

  # Old column removed
  expect_false("max_pct_change" %in% names(res$history))

  # When confounding is assessed, confound_metric is set to "pct_change"
  # (the default).  Some history rows are stop_* actions where the column
  # is NA -- that's fine.
  assessed <- res$history[
    res$history$action %in% c("remove", "retain_confounder"), ,
    drop = FALSE
  ]
  if (nrow(assessed) > 0) {
    expect_true(all(assessed$confound_metric == "pct_change"))
  }
})


test_that("purposeful_cox accepts new arguments", {
  skip_if_not_installed("survival")

  dat <- survival::lung
  dat$status01 <- dat$status - 1L

  res <- suppressWarnings(purposeful_cox(
    data            = dat,
    outcome         = "Surv(time, status01)",
    candidate_vars  = c("age", "sex", "ph.ecog", "wt.loss"),
    confound_method = "stabilized",
    verbose         = FALSE
  ))

  expect_type(res, "list")
  expect_true("confound_metric" %in% names(res$history))

  assessed <- res$history[
    res$history$action %in% c("remove", "retain_confounder"), ,
    drop = FALSE
  ]
  if (nrow(assessed) > 0) {
    expect_true(all(assessed$confound_metric == "stabilized"))
  }
})
