context("utils")

test_that("utils work as expected", {

  # format_1dp
  expect_equal(format_1dp(1.362), "1.4")
  expect_equal(format_1dp("1.362"), "1.4")
  expect_equal(format_1dp(1L), "1.0")

  # format_2dp
  expect_equal(format_2dp(1.362), "1.36")
  expect_equal(format_2dp("1.362"), "1.36")
  expect_equal(format_2dp(1L), "1.00")

  # wrap_parens
  expect_equal(wrap_parens("x + y"), "(x + y)")
  expect_equal(wrap_parens("(x + y)"), "(x + y)")
  expect_equal(wrap_parens("(x + y) + (x + y)"), "((x + y) + (x + y))")
  expect_equal(wrap_parens(NA_character_), NA_character_)
})


