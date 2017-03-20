
# 1.0.3

* The `imports` argument to `make_packages()` can be a list, with one
  value for each package.

* The packages are installed using the `--no-test-load` argument now.
  The reason for this, is that `install.packages` runs in a new R session,
  with potentially different library directories, and some required
  might not be available when the load test is performed.

# 1.0.2

* `make_packages()` now has a `quiet` argument, set to `FALSE`
  to see the installation of disposable packages. This is mainly
  useful for debugging.

# 1.0.1

* Fix spurious `R CMD check` warnings and notes.

# 1.0.0

First version on CRAN.
