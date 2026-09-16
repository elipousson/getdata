test_that("set_pkg_options works", {
  withr::local_options(list(getdata.test_opt = NULL))

  expect_message(
    set_pkg_options(test_opt = 123),
    "Updated options"
  )
  expect_equal(getOption("getdata.test_opt"), 123)

  expect_message(
    set_pkg_options(test_opt = 999),
    "conflict"
  )
  expect_equal(getOption("getdata.test_opt"), 123)

  expect_message(
    set_pkg_options(test_opt = 999, overwrite = TRUE),
    "Replacing"
  )
  expect_equal(getOption("getdata.test_opt"), 999)
})

test_that("set_pkg_options respects .pkg argument", {
  withr::local_options(list(otherpkg.test_opt = NULL))

  expect_message(
    set_pkg_options(test_opt = "value", .pkg = "otherpkg"),
    "Updated options for.*otherpkg"
  )
  expect_equal(getOption("otherpkg.test_opt"), "value")
})
