context("redcap_version")

test_that("redcap_version works as expected", {

  skip_if(conn_test$token == "")

  x <- redcap_version(conn_test)
  expect_is(x, "character")
  expect_length(x, 1)
})


test_that("redcap_version fails gracefully", {

  expect_error(redcap_version(conn_fake))
})
