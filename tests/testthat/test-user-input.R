context("User input")
testthat::test_that("only dataframes are accepted", {
  testthat::expect_error(biplotly(list(d = c(1, 2), e = c(3, 4)), "42"))
})