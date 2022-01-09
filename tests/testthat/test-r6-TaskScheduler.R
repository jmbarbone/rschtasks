test_that("TaskScheduler works", {
  # initializes fine with default values
  expect_s3_class(TaskScheduler$new(), c("TaskScheduler", "R6"))
})
