# Method updates (0.2.0)

The wrappers require Python `weightederm >= 0.2.0`. Search now defaults to exact
`search_method = "dp"`; explicit `"efficient"` retains legacy greedy behavior.
Positive L1 penalties use the Python proximal solver.

Fixed estimators accept `marginal_weights`, a signal-by-observation occupancy
matrix with unit column sums and positive row sums. CV estimators accept
`regularization_constants` for exhaustive joint count/penalty selection and
optional `delta_fraction` for fold-local spacing. Joint results include
`best_regularization_constant`, `alpha`, and a `regularization_constant` column
in `cv_results`. Existing CV behavior remains when the new grid is `NULL`.

Automatic research-code prior extrapolation is not included. Supplied reference
marginals support fixed-count comparisons; joint CV uses exact uniform
count-specific marginals. See the [Python method updates](https://github.com/gabrielarpino/weightederm/blob/main/docs/method_updates.md) for normalization and scaling.
