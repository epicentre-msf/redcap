test_that("fetch_database works as expected", {

  skip_if(is.null(conn_test$token))


  ## specify subsets to fetch
  forms_focal <- c("enrolment", "followup")
  records_focal <- c("0001", "0002")


  ## test basic functionality
  x1 <- fetch_database(
    conn = conn_test,
    forms = forms_focal,
    records = records_focal
  )

  expect_is(x1, "list")
  expect_is(x1[[1]], "tbl_df")
  expect_equal(length(x1), length(forms_focal))
  expect_true(all(sapply(x1, function(x) "record_id" %in% names(x))))


  ## test argument names_fn
  recode_names <- function(x) { toupper(x) }

  x2 <- fetch_database(
    conn = conn_test,
    forms = forms_focal,
    records = records_focal,
    names_fn = recode_names
  )

  expect_setequal(names(x2), recode_names(forms_focal))


  ## test argument fns
  rm_ids  <- function(x) { x[!x$record_id %in% "0002", , drop = FALSE] }
  add_col <- function(x) { x$test <- "TEST"; x }

  x3 <- fetch_database(
    conn = conn_test,
    forms = forms_focal,
    records = records_focal,
    fns = list(rm_ids, add_col)
  )

  expect_true(all(sapply(x3, function(x) "test" %in% names(x))))
  expect_true(all(sapply(x3, function(x) !"0002" %in% x$record_id)))
})


test_that("fetch_database fails gracefully", {

  expect_error(fetch_database(conn_fake))
  skip_if(is.null(conn_test$token))
  expect_error(fetch_database(conn_test, forms = "blah_blah"))
})
