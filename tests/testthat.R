# Test runner. From the repo root run:
#   "C:/Program Files/R/R-4.2.3/bin/Rscript.exe" tests/testthat.R
# Tests use small synthetic inputs and do NOT touch the pipeline, Box data, or the
# statewide network -- they run in seconds.
library(testthat)
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
