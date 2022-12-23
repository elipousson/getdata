test_that("as_date_range works", {
  expect_equal(
    as_date_range("2022-01-01", days = 10),
    as_date_range(c("2022-01-01", "2022-01-11"), days = 10)
  )

  expect_equal(
    as_date_range(year = 2022),
    as_date_range(c("2022-01-01", "2022-12-31"))
  )
})

test_that("date_range_query works", {
  expect_equal(
    date_range_query(c("2022-01-01", "2022-01-31")),
    "(date >= '2022-01-01') AND (date <= '2022-01-31')"
  )
})
