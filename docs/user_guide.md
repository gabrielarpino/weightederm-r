# User Guide

## What `weightederm` does

`weightederm` detects **changepoints** in ordered (e.g. time-series) regression and
classification data. A changepoint is a sample index where the underlying model
parameters shift abruptly. The package estimates those indices under a **Weighted
Empirical Risk Minimization (WERM)** objective that marginalises over possible
changepoint configurations using a combinatorial prior on signal membership.

The result of `werm_*()` is always a set of estimated changepoint indices
(`$changepoints`), not a prediction rule for new data. `predict()` is provided for
convenience and forecasts using the **most recent detected segment** — see
[Prediction](#prediction) below.

---

## How the algorithm works

WERM changepoint detection proceeds in two stages.

### Stage 1 — signal fitting

For a candidate set of `L` changepoints the data is assigned to `L + 1` latent
**signals** (segments). Rather than making a hard assignment, each observation `i`
is associated with signal `l` via a marginal prior weight `w[l, i]` derived from a
uniform prior over all valid configurations with exactly `L` changepoints. These
weights are computed analytically from combinatorial formulae and concentrate near
the true segment boundaries.

For each signal `l`, a weighted regression is solved:

```
minimise  sum_i  w[l, i] * loss(X[i, ] %*% theta_l, y[i])  +  penalty(theta_l)
```

This produces `L + 1` coefficient vectors (`$signal_coefs`) and optionally
`L + 1` intercepts (`$signal_intercepts`).

### Stage 2 — changepoint search

The goodness-of-fit score for a specific configuration `(eta_1, ..., eta_L)` is a
weighted combination of segment-level losses evaluated at the Stage 1 coefficients.
The optimal configuration is found by **`"efficient"`** greedy + local-refinement
search (default) or **`"brute_force"`** enumeration (exact, slow).

---

## A minimal workflow

```r
# R_DOCS_TEST: minimal_workflow
library(weightederm)
set.seed(0L)
n <- 80L; p <- 4L; true_cp <- 40L
X <- matrix(rnorm(n * p), n, p)

y <- numeric(n)
y[seq_len(true_cp)]       <- X[seq_len(true_cp), ]       %*% c(2, -1, 0, 0) +
  rnorm(true_cp,      sd = 0.1)
y[seq(true_cp + 1L, n)] <- X[seq(true_cp + 1L, n), ] %*% c(-1, 2, 0, 0) +
  rnorm(n - true_cp,  sd = 0.1)

fit <- werm_least_squares(X, y, num_chgpts = 1L, delta = 5L, fit_intercept = FALSE)
cat("estimated changepoint:", fit$changepoints[1], "\n")
cat("true changepoint:     ", true_cp, "\n")
stopifnot(abs(fit$changepoints[1] - true_cp) <= 5L)
```

After fitting, `fit$changepoints` is a **1-indexed** integer vector of length
`num_chgpts` (R convention). The data is implicitly partitioned into segments:

| Segment | Row range |
|---------|-----------|
| 1 | `1 : changepoints[1]` |
| 2 | `(changepoints[1]+1) : changepoints[2]` |
| … | … |
| L+1 | `(changepoints[L]+1) : n` |

---

## Fixed vs CV estimators

### Fixed (`werm_least_squares`, `werm_huber`, `werm_logistic`)

Use when the number of changepoints is known from domain knowledge or theory.

```r
# R_DOCS_TEST: fixed_estimator
library(weightederm)
X <- matrix(1, nrow = 60L, ncol = 2L)
y <- c(rep(0, 30L), rep(1, 30L))
fit <- werm_least_squares(X, y, num_chgpts = 1L, delta = 4L, fit_intercept = FALSE)
stopifnot(fit$num_chgpts == 1L)
stopifnot(length(fit$changepoints) == 1L)
```

### CV (`werm_least_squares_cv`, `werm_huber_cv`, `werm_logistic_cv`)

Use when the number of changepoints is unknown. The CV estimator:

1. Tries every `num_chgpts` in `{0, 1, …, max_num_chgpts}`.
2. For each candidate, runs `cv`-fold interleaved cross-validation.
3. Selects the candidate with the lowest mean held-out loss.
4. Refits the selected model on the full dataset.

```r
# R_DOCS_TEST: cv_estimator
library(weightederm)
X <- matrix(1, nrow = 80L, ncol = 2L)
y <- c(rep(0, 40L), rep(1, 40L))
fit <- werm_least_squares_cv(X, y, max_num_chgpts = 3L, cv = 4L, delta = 5L,
                              fit_intercept = FALSE)
stopifnot(fit$best_num_chgpts == 1L)
stopifnot("mean_test_score" %in% names(fit$cv_results))
```

#### Reading CV results

```r
for (i in seq_len(nrow(fit$cv_results))) {
  cat(sprintf("  num_chgpts=%d: mean CV loss = %.4f\n",
              fit$cv_results$num_chgpts[i],
              fit$cv_results$mean_test_score[i]))
}
```

#### CV scoring loss

By default, least-squares and Huber CV score held-out folds with **absolute
error** (more robust than squared error for model selection). To use the
estimator's base loss instead set `use_base_loss_for_cv = TRUE`:

```r
# fit <- werm_least_squares_cv(X, y, max_num_chgpts = 2L, cv = 3L,
#                               use_base_loss_for_cv = TRUE)
```

For the logistic CV estimator the base (logistic) loss is always used.

---

## Choosing `delta`

`delta` sets the **minimum gap** between adjacent candidate changepoints during
the Stage 2 search. It does **not** affect the Stage 1 signal weights.

A practical rule of thumb:

```r
delta <- max(1L, as.integer(0.05 * nrow(X)))   # 5% of the sample size
```

Setting `delta` too small allows spurious micro-changepoints. Setting it too
large prevents detection of closely-spaced true changepoints. For benchmark
experiments, `delta ≈ n / (20 * num_signals)` works well.

---

## Penalties

All six estimators accept `penalty` and `alpha`. The penalty is applied to the
**coefficient vector only** (never the intercept).

| `penalty` | Applied to Stage 1 fits | Default |
|-----------|-------------------------|---------|
| `"none"`  | nothing                 | LS, Huber |
| `"l2"`    | `alpha * sum(coef^2)`   | Logistic |
| `"l1"`    | `alpha * sum(abs(coef))`| — |

```r
# R_DOCS_TEST: penalty_usage
library(weightederm)
X <- matrix(1, nrow = 60L, ncol = 3L)
y <- c(rep(0, 30L), rep(1, 30L))
fit_none <- werm_least_squares(X, y, num_chgpts = 1L, delta = 4L,
                                penalty = "none", fit_intercept = FALSE)
fit_l2   <- werm_least_squares(X, y, num_chgpts = 1L, delta = 4L,
                                penalty = "l2", alpha = 1.0, fit_intercept = FALSE)
stopifnot(length(fit_none$changepoints) == 1L)
stopifnot(length(fit_l2$changepoints) == 1L)
# L2 shrinks the last-segment coefficient toward zero
stopifnot(
  sqrt(sum(fit_l2$last_segment_coef^2)) <=
  sqrt(sum(fit_none$last_segment_coef^2)) + 1e-9
)
```

**Logistic default:** `penalty = "l2"`, `alpha = 1.0`. Unpenalised logistic fits
can diverge when a segment is linearly separable; the default L2 penalty prevents
this.

---

## Prediction

`predict(fit, X_new)` forecasts by fitting a **fresh, unweighted regression on the
last detected segment** using the estimator's base loss, then evaluating it on
`X_new`.

The reasoning: after changepoints are detected, the most recent segment represents
the current regime. New observations are assumed to come from this regime.

```r
# R_DOCS_TEST: predict_usage
library(weightederm)
X_train <- matrix(1, nrow = 60L, ncol = 2L)
y_train <- c(rep(0, 30L), rep(1, 30L))
fit <- werm_least_squares(X_train, y_train, num_chgpts = 1L, delta = 4L,
                          fit_intercept = FALSE)

X_new <- matrix(1, nrow = 10L, ncol = 2L)
y_pred <- predict(fit, X_new)
stopifnot(length(y_pred) == 10L)
```

The last-segment coefficients are available as `fit$last_segment_coef` and
`fit$last_segment_intercept` immediately after fitting.

### Logistic prediction

For logistic estimators, `predict()` accepts a `type` argument:

```r
# R_DOCS_TEST: predict_logistic
library(weightederm)
set.seed(7L)
n <- 80L; p <- 3L; true_cp <- 40L
X <- matrix(rnorm(n * p), n, p)
eta <- c(X[seq_len(true_cp), ]     %*% c(2, -2, 0),
         X[seq(true_cp + 1L, n), ] %*% c(-2, 2, 0))
y <- rbinom(n, 1L, 1 / (1 + exp(-eta)))

fit <- werm_logistic(X, y, num_chgpts = 1L, delta = 5L, fit_intercept = FALSE)

X_new <- matrix(rnorm(10L * p), 10L, p)
labels <- predict(fit, X_new, type = "class")   # character vector
probs  <- predict(fit, X_new, type = "prob")    # (10 × 2) matrix

stopifnot(is.character(labels), length(labels) == 10L)
stopifnot(nrow(probs) == 10L, ncol(probs) == 2L)
stopifnot(all(abs(rowSums(probs) - 1) < 1e-9))
```

> **Note:** `predict()` on a changepoint detector has a different meaning than on
> a standard supervised model. The returned values are *forecasts under the
> assumption that the most recent regime continues* — not predictions derived from
> the full training structure.

---

## S3 methods

Every fitted object is an S3 list of class `c("werm_<type>", "werm_fit")`.

```r
print(fit)            # compact summary: changepoints, objective, num_signals
summary(fit)          # print + coefficient table for all segments
coef(fit)             # last_segment_coef vector (used by predict)
predict(fit, X_new)   # numeric predictions or class labels / probabilities
```

```r
# R_DOCS_TEST: s3_methods
library(weightederm)
set.seed(9L)
n <- 60L; p <- 2L
X <- matrix(rnorm(n * p), n, p)
y <- c(X[1:30, ] %*% c(2, 0), X[31:60, ] %*% c(-2, 0)) + rnorm(n, sd = 0.1)
fit <- werm_least_squares(X, y, num_chgpts = 1L, delta = 4L, fit_intercept = FALSE)

stopifnot(is.numeric(coef(fit)))
stopifnot(length(coef(fit)) == p)

out <- capture.output(print(fit))
stopifnot(any(grepl("WERM", out)))
```
