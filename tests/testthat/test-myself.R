
context("Disposable packages")

test_that("We can create and load them", {

  on.exit(unloadNamespace("foo1"), add = TRUE)
  on.exit(unloadNamespace("foo2"), add = TRUE)
  on.exit(unlink(lib_dir, recursive = TRUE))
  
  lib_dir <- make_packages(
    foo1 = { f <- function() print("hello!") ; d <- 1:10 },
    foo2 = { f <- function() print("hello again!") ; d <- 11:20 }
  )
  
  expect_output(foo1::f(), "hello!", fixed = TRUE)  
  expect_output(foo2::f(), "hello again!", fixed = TRUE)
  expect_equal(foo1::d, 1:10)
  expect_equal(foo2::d, 11:20)

})
