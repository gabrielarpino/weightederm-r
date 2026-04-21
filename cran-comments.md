## R CMD check results

0 errors | 0 warnings | 1 note

## Notes

The package wraps Python via 'reticulate'. On the CRAN Debian check server,
examples and tests involve numpy which uses multiple CPU threads (OpenBLAS /
BLAS). This produces a CPU-time-to-elapsed-time ratio above 2.5x in tests,
which is expected for Python-backed R packages and not indicative of a
correctness issue. Thread count is limited to 1 via OMP_NUM_THREADS in
tests/testthat/setup.R.