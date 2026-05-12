# Tests for input validation and the legacy confound_pct -> confound_threshold
# migration path.


test_that("confound_pct (legacy) maps to confound_threshold with a message", {
  set.seed(1)
  dat <- tibble::tibble(
    y  = rbinom(100, 1, 0.5),
    x1 = rnorm(100)
  )

  expect_message(
    suppressWarnings(purposeful_glm(
      data           = dat,
      outcome        = "y",
      candidate_vars = "x1",
      confound_pct   = 0.30,
      verbose        = FALSE
    )),
    regexp = "deprecated"
  )
})


test_that("supplying both confound_pct and confound_threshold errors", {
  set.seed(1)
  dat <- tibble::tibble(
    y  = rbinom(100, 1, 0.5),
    x1 = rnorm(100)
  )

  expect_error(
    purposeful_glm(
      data               = dat,
      outcome            = "y",
      candidate_vars     = "x1",
      confound_pct       = 0.30,
      confound_threshold = 0.20,
      verbose            = FALSE
    ),
    regexp = "Supply either"
  )
})


test_that("target_var must be a single character", {
  set.seed(1)
  dat <- tibble::tibble(
    y  = rbinom(100, 1, 0.5),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  # Length > 1 errors
  expect_error(
    purposeful_glm(
      data           = dat,
      outcome        = "y",
      candidate_vars = c("x1", "x2"),
      target_var     = c("x1", "x2"),
      verbose        = FALSE
    ),
    regexp = "single variable"
  )

  # Non-character errors
  expect_error(
    purposeful_glm(
      data           = dat,
      outcome        = "y",
      candidate_vars = c("x1", "x2"),
      target_var     = 1L,
      verbose        = FALSE
    ),
    regexp = "single variable"
  )
})


test_that("target_var outside candidates and keep_in_mod warns", {
  set.seed(1)
  dat <- tibble::tibble(
    y  = rbinom(100, 1, 0.5),
    x1 = rnorm(100)
  )

  # Capture all warnings (there may also be a small-coef warning from the
  # default pct_change method); assert ours is among them
  msgs <- character(0)
  withCallingHandlers(
    {
      purposeful_glm(
        data           = dat,
        outcome        = "y",
        candidate_vars = "x1",
        target_var     = "nonexistent",
        verbose        = FALSE
      )
    },
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("is in neither", msgs)))
})


test_that("invalid confound_method errors via match.arg", {
  set.seed(1)
  dat <- tibble::tibble(
    y  = rbinom(100, 1, 0.5),
    x1 = rnorm(100)
  )

  expect_error(
    purposeful_glm(
      data            = dat,
      outcome         = "y",
      candidate_vars  = "x1",
      confound_method = "not_a_real_method",
      verbose         = FALSE
    )
  )
})
