# weightederm 0.1.0

* Initial release.
* R interface to the `weightederm` Python package via `reticulate`.
* Six estimators: `werm_least_squares()`, `werm_huber()`, `werm_logistic()`,
  and their cross-validated variants `werm_least_squares_cv()`,
  `werm_huber_cv()`, `werm_logistic_cv()`.
* S3 methods: `print()`, `summary()`, `predict()`, `coef()`.
* `weightederm_configure_python()` helper for pointing `reticulate` at the
  correct Python environment.
* All changepoint indices returned in 1-indexed R convention.
