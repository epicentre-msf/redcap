context("reclass")

test_that("reclass works as expected", {

  skip_if(conn_test$token == "")

  ## specify subsets to fetch
  form_focal <- "followup"
  event_focal <- "unscheduled_follow_arm_1"

  ## get dictionary for focal form
  dict <- meta_dictionary(conn_test, forms = form_focal)

  ## expect that reclass can convert time/dates from numeric back to their default
  # requires fn_dates = lubridate::as_date
  x1 <- fetch_records(
    conn = conn_test,
    forms = form_focal,
    events = event_focal,
    times_chron = TRUE
  )

  x1_num <- x1
  vars_date_time <- dict$field_name[grepl("date_|^time$", dict$validation)]
  for (j in vars_date_time) { x1_num[[j]] <- as.numeric(x1_num[[j]]) }

  x1_reclass <- reclass(x1_num, dict, fn_dates = lubridate::as_date, fn_dates_args = list())
  expect_equal(x1, x1_reclass)

  ## expect that reclass can retain dates/datetimes as class character
  x2 <- fetch_records(
    conn = conn_test,
    forms = form_focal,
    events = event_focal,
    fn_dates = as.character,
    fn_dates_args = list(),
    fn_datetimes = as.character,
    fn_datetimes_args = list()
  )

  vars_date <- dict$field_name[grepl("date", dict$validation)]
  expect_equal(unique(vapply(x2[,vars_date], class, "")), "character")
})

