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

  expect_equal(
    date_range_query(c("2022-01-01", "2022-01-31"), .col = "created_date"),
    "(created_date >= '2022-01-01') AND (created_date <= '2022-01-31')"
  )
})

test_that("between_date_range works", {
  expect_equal(
    between_date_range(c("2022-01-01", "2022-01-31")),
    "(date BETWEEN DATE '2022-01-01' AND DATE '2022-01-31')"
  )

  expect_equal(
    between_date_range(c("2022-01-01", "2022-01-31"), .col = "created_date"),
    "(created_date BETWEEN DATE '2022-01-01' AND DATE '2022-01-31')"
  )
})

test_that("check_date_range works", {
  expect_null(
    check_date_range(c("2022-06-01", "2022-06-30"))
  )

  expect_no_error(
    check_date_range(
      c("2022-06-01", "2022-06-30"),
      limits = c("2022-01-01", "2022-12-31")
    )
  )

  expect_error(
    check_date_range(
      c("2022-06-01", "2022-06-30"),
      limits = c("2022-07-01", "2022-12-31")
    )
  )

  expect_error(
    check_date_range(
      c("2022-06-01", "2022-12-31"),
      limits = c("2022-01-01", "2022-11-30")
    )
  )
})
