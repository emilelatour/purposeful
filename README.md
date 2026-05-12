# purposeful

> Purposeful selection for regression models, following Hosmer & Lemeshow.

`purposeful` applies the three-phase purposeful selection procedure to logistic regression, Cox proportional hazards models, and ordinary linear regression. It handles univariable screening, backward elimination with an integrated confounding check, and final-model fitting in a single call, returning a tidy result with step-by-step history.

Purposeful selection prioritizes interpretability and clinical reasoning over predictive performance; for prediction-focused workflows, consider regularization-based alternatives like `glmnet`.

## Installation

```r
# install.packages("pak")
pak::pak("emilelatour/purposeful")
```

## Usage

The three entry points share the same arguments and return the same shape:

- `purposeful_glm()` — logistic regression (or any GLM family)
- `purposeful_cox()` — Cox proportional hazards
- `purposeful_lm()` — ordinary linear regression

```r
library(purposeful)

set.seed(888)
n <- 1000
dat <- tibble::tibble(
  age    = abs(round(rnorm(n, 67, 14))),
  lac    = abs(round(rnorm(n, 5, 3), 1)),
  gender = factor(rbinom(n, 1, 0.6), labels = c("male", "female")),
  wbc    = abs(round(rnorm(n, 10, 3), 1)),
  hb     = abs(round(rnorm(n, 120, 40))),
  mort   = factor(rbinom(n, 1,
                         plogis(0.1 * age - 0.02 * hb + lac - 10)),
                  labels = c("alive", "dead"))
)

res <- purposeful_glm(
  data           = dat,
  outcome        = "mort",
  candidate_vars = c("age", "gender", "lac", "wbc", "hb"),
  family         = binomial(),
  verbose        = FALSE
)

res$final_vars        # variables in the final model
res$history           # step-by-step elimination log
res$final_tidy        # odds ratios with CIs
```

## The confounding check

After backward elimination identifies a candidate for removal (variable-level p ≥ `p_remove`), the procedure refits the model without it and checks whether any remaining coefficient has shifted enough to call the removed variable a confounder. Three metrics are available via `confound_method`:

| Method | Metric | Default threshold | When to use |
|---|---|---|---|
| `"pct_change"` | `\|β_new − β_old\| / \|β_old\|` | 0.20 | Textbook Hosmer-Lemeshow. The default. |
| `"abs_change"` | `\|β_new − β_old\|` | 0.10 | When coefficients vary widely in magnitude. |
| `"stabilized"` | `\|β_new − β_old\| / SE(β_old)` | 1.00 | When any coefficient may be near zero. Robust to the failure mode below. |

### A failure mode of percent change

The textbook `pct_change` metric misbehaves when any coefficient is small in absolute terms. Tiny absolute shifts get amplified into huge percentages, causing spurious confounder retention. This shows up in practice when your exposure of interest has a weak effect (e.g., a log-odds coefficient of 0.03), or when adjusting for noise variables produces near-zero estimates.

To make this visible, `purposeful_glm()` (and friends) emit a one-time warning at the start of backward elimination if any coefficient in the initial multivariable model satisfies `|β| < 2·SE`:

```
Warning: Small coefficient(s) detected (|beta| < 2*SE) in the initial
Phase 2 model: on_biologic, age. `confound_method = "pct_change"` can
over-retain confounders when coefficients are near zero. Consider
`confound_method = "stabilized"`, or set `target_var` to focus the
check on a single coefficient. Set `small_coef_warn = FALSE` to
suppress this message.
```

The two suggested fixes:

```r
# Option A: use the stabilized metric for all coefficients
purposeful_glm(..., confound_method = "stabilized")

# Option B: focus the check on one exposure of interest
purposeful_glm(..., target_var = "on_biologic")
```

`target_var` is orthogonal to `confound_method` — you can combine them.

## Output

Each function returns a list:

| Element | What it is |
|---|---|
| `uni_screen` | Univariable p-values for every candidate |
| `screened_vars` | Variables that passed `p_screen` (plus `keep_in_mod`) |
| `history` | Step-by-step elimination log with action, p-value, metric, and value |
| `final_vars` | Variables in the final model |
| `final_fit` | The fitted `glm` / `coxph` / `lm` object |
| `final_term_tests` | Variable-level `drop1()` p-values for the final model |
| `final_tidy` | `broom::tidy()` table, exponentiated for logistic and Cox |

## Reference

- Hosmer DW, Lemeshow S (2000). *Applied Logistic Regression*. Wiley.
- Hosmer DW, Lemeshow S, May S (2008). *Applied Survival Analysis*, 2nd ed. Wiley.

## License

MIT
