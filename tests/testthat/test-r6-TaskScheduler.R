test_that("TaskScheduler works", {
  # initializes fine with default values
  expect_error(TaskScheduler$new(), NA)
})
