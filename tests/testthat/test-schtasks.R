test_that("schtasks_()", {
  skip_on_cran()
})

test_that("schtasks_query()", {
  skip_on_cran()

  # Returns tibble

  # Doesn't return tibble
})

test_that("check days", {

  task <- TaskScheduler$new(
    task_name = "",
    days = 1,
    schedule = "monthly",
    exec = FALSE
  )

  expect_identical(private(task, "days"), 1)
  expect_identical(private(task, "schedule"), "MONTHLY")
  expect_s3_class(task$create(), c("TaskScheduler", "R6"))

  task$result
  task$system_call
  task <- TaskScheduler$new(days = "friday")
  private(task, "days")



  # TODO update with private()
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
