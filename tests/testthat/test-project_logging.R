context("project_logging")

test_that("project_logging works as expected", {

  skip_if(conn_test$token == "")

  x1 <- project_logging(conn_test)
  expect_s3_class(x1, "tbl_df")
  expect_named(x1, c("timestamp", "username", "action", "details"))

  x2 <- project_logging(conn_test, type = "export")
  expect_true(all(grepl("^Data Export", x2$action)))

  x3 <- project_logging(conn_test, type = "record_add")
  expect_true(all(grepl("^Created Record", x3$action)))

  x4 <- project_logging(conn_test, type = "record", record = "0002")
  expect_true(all(grepl("0002", x4$action)))

  x5 <- project_logging(conn_test, type = "record")

  dt_min <- "2021-03-01 13:30"
  x6 <- project_logging(conn_test, type = "record", time_start = dt_min)
  expect_gte(min(lubridate::ymd_hm(x6$timestamp)), lubridate::ymd_hm(dt_min))
  expect_equal(max(lubridate::ymd_hm(x6$timestamp)), max(lubridate::ymd_hm(x5$timestamp)))

  dt_max <- "2021-03-19 19:05"
  x7 <- project_logging(conn_test, type = "record", time_end = dt_max)
  expect_lte(max(lubridate::ymd_hm(x7$timestamp)), lubridate::ymd_hm(dt_max))
  expect_equal(min(lubridate::ymd_hm(x7$timestamp)), min(lubridate::ymd_hm(x5$timestamp)))

  x8 <- project_logging(conn_test, type = "record", time_start = dt_min, time_end = dt_max)
  expect_gte(min(lubridate::ymd_hm(x8$timestamp)), lubridate::ymd_hm(dt_min))
  expect_lte(max(lubridate::ymd_hm(x8$timestamp)), lubridate::ymd_hm(dt_max))
})


test_that("project_logging fails gracefully", {

  expect_error(project_logging(conn_test, type = "blah"))
  expect_error(project_logging(conn_test, user = c("p-barks", "other")))
  expect_error(project_logging(conn_test, record = c("0001", "0002")))
})
