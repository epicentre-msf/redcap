context("project_logging")

test_that("project_logging works as expected", {

  skip_if(conn_test$token == "")

  x1 <- project_logging(conn_test)
  expect_s3_class(x1, "tbl_df")
  expect_true(all(c("timestamp", "username", "action", "details") %in% names(x1)))

  x2 <- project_logging(conn_test, type = "export")
  expect_true(all(grepl("export", x2$action, ignore.case = TRUE)))

  x3 <- project_logging(conn_test, type = "record_add")
  expect_true(all(grepl("^Create(d)? Record", x3$action, ignore.case = TRUE)))

  x4 <- project_logging(conn_test, type = "record", record = "0002")
  expect_true(all(grepl("0002", x4$action)))

  x5 <- project_logging(conn_test, type = "record")
  dt_min <- "2021-03-01 13:30"
  x6 <- project_logging(conn_test, type = "record", time_start = dt_min)
  expect_gte(min(x6$timestamp), lubridate::ymd_hm(dt_min))
  expect_equal(max(x6$timestamp), max(x5$timestamp))

  dt_max <- "2021-03-19 19:05"
  x7 <- project_logging(conn_test, type = "record", time_end = dt_max)
  expect_lte(max(x7$timestamp), lubridate::ymd_hm(dt_max))
  expect_equal(min(x7$timestamp), min(x5$timestamp))

  x8 <- project_logging(conn_test, type = "record", time_start = dt_min, time_end = dt_max)
  expect_gte(min(x8$timestamp), lubridate::ymd_hm(dt_min))
  expect_lte(max(x8$timestamp), lubridate::ymd_hm(dt_max))

  x9 <- project_logging(conn_test, record = "0002", timestamp_to_posix = FALSE)
  expect_type(x9$timestamp, "character")
})


test_that("project_logging fails gracefully", {

  expect_error(project_logging(conn_test, type = "blah"))
  expect_error(project_logging(conn_test, user = c("p-barks", "other")))
  expect_error(project_logging(conn_test, record = c("0001", "0002")))
})
