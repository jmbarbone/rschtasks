test_that("schtasks_()", {
  skip_on_cran()
})

test_that("schtasks_query()", {
  skip_on_cran()

  # Returns tibble

  # Doesn't return tibble
})

test_that("check days", {
  ts <- TaskScheduler$new(days = 1, schedule = "monthly")
  private(ts, "days")
  private(ts, "check_days")

  ts <- TaskScheduler$new(days = "friday")
  private(ts, "days")



  # TODO upate with private()
  # expect_identical(check_days("1", "monthly"), "1")
  # expect_identical(check_days(1, "monthly"), "1")
  # expect_error(check_days(1))
  #
  # expect_identical(check_days("friday"), "FRI")
  # expect_error(check_days("frid"))
  #
  # expect_identical(check_days("*"), "*")
  # expect_identical(check_days(NULL), NULL)
})
