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
  vars_dt <- dict$field_name[grepl("date|time", dict$validation)]
  for (j in vars_dt) { x1_num[[j]] <- as.numeric(x1_num[[j]]) }

  x1_reclass <- reclass(x1_num, dict, fn_dates = lubridate::as_date)
  expect_identical(x1, x1_reclass)
})

