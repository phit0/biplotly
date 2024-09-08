
testthat::test_that("only dataframes are accepted", {
  testthat::expect_error(triplotly(list(d = c(1, 2), e = c(3, 4)), "42"))
})

test_data <- data.frame(
  "A" = rep(1:2, 5),
  "B" = rnorm(10),
  "C" = runif(10),
  "D" = rt(10, 1)
)

test_that("Factors can be submitted as index and String", {
  expect_true("trp" %in% class(trp$new(test_data, factors = "B")))
  expect_true("trp" %in% class(trp$new(test_data, factors = 1)))
})

test_that("Message for dropping empty columns is issued", {
  expect_message(
    trp$new(test_data %>% 
                  tibble::add_column("E" = rep(NA, 10)), factors = "B"),
    "empty and will be removed"
  )
})

test_that("Error for inexistent column name is issued", {
  
          expect_error(
            trp$new(test_data, factors = "Z"), 
            "either empty or do not match the data header..."
          )
          expect_error(
            trp$new(test_data, factors = "E"), 
            "either empty or do not match the data header..."
          )
})

test_that("Message for dropping rows with empty values is issued", 
          {
            test_data[3,3] <- NA
            expect_message(
              trp$new(test_data, factors = "B"),
              "The following columns contain most missing values:"
            )
          }
)

test_that("Message for constant column detection is issued", {
  expect_message(
    trp$new(
      test_data %>% tibble::add_column("F" = 99),
      factors = "A"
    ),
    "are constant and will be removed"
  )
}
)

