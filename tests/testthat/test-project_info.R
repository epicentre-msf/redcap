context("project_info")

test_that("project_info works as expected", {

  skip_if(conn_test$token == "")

  m1 <- project_info(conn_test)
  expect_is(m1, "tbl_df")
  expect_true(all(c("project_id", "project_title", "creation_time", "in_production") %in% names(m1)))

})


test_that("project_info fails gracefully", {

  expect_error(project_info(conn_fake))
})

