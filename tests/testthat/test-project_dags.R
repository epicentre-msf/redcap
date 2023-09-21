context("project_dags")

test_that("project_dags works as expected", {

  skip_if(conn_test$token == "")

  m1 <- project_dags(conn_test)
  expect_is(m1, "tbl_df")
  expect_true(all(c("data_access_group_name", "unique_group_name") %in% names(m1)))

})


test_that("project_dags fails gracefully", {

  expect_error(project_dags(conn_fake))
})

