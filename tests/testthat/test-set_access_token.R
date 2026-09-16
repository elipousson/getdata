test_that("get_access_token returns a directly supplied token", {
  expect_equal(
    get_access_token(token = "abc123", type = "ANY_TOKEN"),
    "abc123"
  )

  # type is optional when token is supplied directly
  expect_equal(
    get_access_token(token = "abc123"),
    "abc123"
  )
})

test_that("get_access_token reads from an environment variable", {
  withr::local_envvar(c("TEST_GETDATA_TOKEN" = "value123"))

  expect_equal(
    get_access_token(type = "TEST_GETDATA_TOKEN"),
    "value123"
  )
})

test_that("get_access_token falls back to TOKEN when type is not supplied", {
  withr::local_envvar(c("TOKEN" = "default_token_value"))

  expect_equal(
    get_access_token(),
    "default_token_value"
  )
})

test_that("get_access_token errors when the token can't be found", {
  withr::local_envvar(c("TEST_GETDATA_MISSING_TOKEN" = ""))

  expect_error(
    get_access_token(type = "TEST_GETDATA_MISSING_TOKEN")
  )
})

test_that("set_access_token installs a token to .Renviron", {
  tmp_home <- withr::local_tempdir()
  withr::local_envvar(c(HOME = tmp_home))

  suppressMessages(
    set_access_token(
      "abc123",
      type = "TEST_GETDATA_INSTALL_TOKEN",
      install = TRUE,
      quiet = TRUE
    )
  )

  renviron <- file.path(tmp_home, ".Renviron")

  expect_true(file.exists(renviron))
  expect_match(
    readLines(renviron),
    'TEST_GETDATA_INSTALL_TOKEN="abc123"',
    fixed = TRUE
  )
})

test_that("set_access_token installs to TOKEN when type is not supplied", {
  tmp_home <- withr::local_tempdir()
  withr::local_envvar(c(HOME = tmp_home))

  suppressMessages(
    set_access_token("abc123", install = TRUE, quiet = TRUE)
  )

  renviron <- file.path(tmp_home, ".Renviron")

  expect_true(file.exists(renviron))
  expect_match(
    readLines(renviron),
    'TOKEN="abc123"',
    fixed = TRUE
  )
})

test_that("set_access_token errors when overwriting without permission", {
  tmp_home <- withr::local_tempdir()
  withr::local_envvar(c(HOME = tmp_home))

  suppressMessages(
    set_access_token(
      "abc123",
      type = "TEST_GETDATA_OVERWRITE_TOKEN",
      install = TRUE,
      quiet = TRUE
    )
  )

  expect_error(
    suppressMessages(
      set_access_token(
        "xyz789",
        type = "TEST_GETDATA_OVERWRITE_TOKEN",
        install = TRUE,
        quiet = TRUE
      )
    )
  )
})
