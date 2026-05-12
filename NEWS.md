# purposeful 0.1.0

First proper release.

## Breaking changes
* The deprecated step-by-step API (`purposeful_step_1()`, `purposeful_step_2()`,
  `purposeful_step_3()`) and its internal helpers have been removed entirely.
  Use `purposeful_glm()`, `purposeful_cox()`, or `purposeful_lm()` instead.

## New features
* `confound_method` argument controls how coefficient change is measured during
  the confounding check: `"pct_change"` (Hosmer-Lemeshow default), `"abs_change"`
  (absolute log-scale change), or `"stabilized"` (SE-scaled, robust to small
  coefficients).
* `target_var` argument focuses the confounding check on a single variable's
  coefficient instead of all remaining coefficients. Useful for causal
  estimation where only one exposure matters.
* Method-aware `confound_threshold` defaults (0.20 / 0.10 / 1.00 for the three
  methods).
* Warning emitted when `"pct_change"` is used with coefficients near zero
  (`|beta| < 2*SE`), the regime where the metric is unreliable. Suppress with
  `small_coef_warn = FALSE`.

## Deprecations
* `confound_pct` is deprecated in favor of `confound_threshold`. Existing
  scripts continue to work; a one-time message is shown when the legacy
  argument is used.
