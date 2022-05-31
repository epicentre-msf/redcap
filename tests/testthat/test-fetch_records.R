context("fetch_records")

test_that("fetch_records works as expected", {

  skip_if(conn_test$token == "")

  ## specify subsets to fetch
  form_focal <- "followup"
  event_focal <- "unscheduled_follow_arm_1"

  ## get dictionary for focal form
  dict <- meta_dictionary(conn_test, forms = form_focal)
  dict <- dict[!dict$field_type %in% "descriptive",]

  ## test with T/F args set to FALSE
  x1 <- fetch_records(
    conn = conn_test,
    forms = form_focal,
    events = event_focal,
    id_field = FALSE,
    rm_empty = FALSE,
    value_labs = FALSE,
    header_labs = FALSE,
    checkbox_labs = FALSE,
    use_factors = FALSE,
    times_chron = FALSE,
    dag = FALSE,
    double_resolve = FALSE
  )

  expect_is(x1, "tbl_df")
  expect_is(x1$follow_form_dt, "POSIXct")
  expect_is(x1$follow_date, "Date")
  expect_is(x1$follow_time, "character")
  expect_is(x1$follow_visit_type, "character")
  expect_true(!"record_id" %in% names(x1))
  expect_true(all(dict$field_name %in% names(x1)))
  expect_true(all(x1$follow_sae_which___1 %in% c("0", "1")))
  expect_true(!"redcap_data_access_group" %in% names(x1))


  ## test with T/F args set to TRUE (except header_labs)
  x2 <- fetch_records(
    conn = conn_test,
    forms = form_focal,
    events = event_focal,
    id_field = TRUE,
    rm_empty = TRUE,
    value_labs = TRUE,
    header_labs = FALSE,
    checkbox_labs = TRUE,
    use_factors = TRUE,
    times_chron = TRUE,
    dag = TRUE
  )

  expect_equal(nrow(x1), nrow(x2))
  expect_is(x2$follow_time, "times")
  expect_is(x2$follow_visit_type, "factor")
  expect_true(all(x2$redcap_event_name %in% "Unscheduled Followup"))
  expect_true(all(dict$field_name %in% names(x2)))
  expect_true(all(x2$follow_sae_which___1 %in% c("Participant was hospitalized", NA)))


  ## test header_labs = TRUE, and value_labs TRUE + checkbox_labs FALSE
  x3 <- fetch_records(
    conn = conn_test,
    forms = form_focal,
    events = event_focal,
    value_labs = TRUE,
    header_labs = TRUE,
    checkbox_labs = FALSE,
    use_factors = FALSE,
    dag = FALSE
  )

  expect_true(all(dict$field_label %in% names(x3)))
  check_var <- "Which serious adverse event occurred? (choice=Participant was hospitalized)"
  expect_true(all(x3[[check_var]] %in% c("Unchecked", "Checked")))


  ## test argument records
  records_focal <- c("0001", "0003")

  x4 <- fetch_records(
    conn = conn_test,
    forms = form_focal,
    records = records_focal
  )

  expect_setequal(x4$record_id, records_focal)

  ## test argument records_omit
  x5 <- fetch_records(
    conn = conn_test,
    forms = form_focal,
    records_omit = records_focal
  )

  expect_true(!any(records_focal %in% x5$record_id))
})


test_that("fetch_records fails gracefully", {

  expect_error(fetch_records(conn_fake))
  skip_if(conn_test$token == "")
  expect_error(fetch_records(conn_test, forms = "blah_blah"))
})

