context("generate_queries")

test_that("generate_queries works as expected", {

  skip_if(conn_test$token == "")

  q1 <- generate_queries(conn_test, query_types = "both")
  expect_is(q1, "tbl_df")
  expect_setequal(q1$form_name, meta_forms(conn_test)[[1]])

  # test arg forms
  q2 <- generate_queries(conn_test, forms = "eligibility")
  expect_setequal(q2$form_name, "eligibility")

  # test arg query_types
  q1_types <- vapply(q1$description, function(x) strsplit(x, "\\:")[[1]][1], "")
  expect_setequal(q1_types, c("Missing", "Not missing"))

  q3 <- generate_queries(conn_test, query_types = "missing")
  expect_true(all(grepl("^Missing", q3$description)))

  q4 <- generate_queries(conn_test, query_types = "not missing")
  expect_true(all(grepl("^Not missing", q4$description)))
})


test_that("fetch_database fails gracefully", {

  expect_error(generate_queries(conn_fake))
  skip_if(conn_test$token == "")
  expect_error(generate_queries(conn_test, forms = "blah_blah"))
})
