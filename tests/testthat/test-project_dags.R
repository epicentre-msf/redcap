context("project_dags")

test_that("project_dags works as expected", {

  skip_if(conn_test$token == "")

  m1 <- project_dags(conn_test)
  expect_is(m1, "tbl_df")
  expect_equal(names(m1), c("data_access_group_name", "unique_group_name"))

})


test_that("project_dags fails gracefully", {

  expect_error(project_dags(conn_fake))
})

