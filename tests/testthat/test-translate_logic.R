context("translate_logic")

test_that("translate_logic works as expected", {

  df_radio <- dplyr::tribble(
    ~field_name, ~value, ~label,
    "consent", "0", "No",
    "consent", "1", "Yes",
    "head_household", "0", "No",
    "head_household", "1", "Yes",
    "type___88", "0", "Unchecked",
    "type___88", "1", "Checked",
    "fruit___app", "0", "Unchecked",
    "fruit___app", "1", "Checked",
    "fruit___ban", "0", "Unchecked",
    "fruit___ban", "1", "Checked",
    "signature", "0", "No",
    "signature", "1", "Yes",
    "relation", "0", "No",
    "relation", "1", "Father/Mother",
    "relation", "2", "Elderly sibling",
    "relation", "3", "Uncle/Aunt",
    "relation", "88", "Other"
  )

  redcap_logic <- c(
    "[head_household]='1'",
    "[head_household]=1 and [consent]=0 and [consent]<>\"\"",
    "[type(88)] = '1'",
    "[signature]='1' and [age] < 18",
    "[relation]=88",
    "[fruit(APP)]='1'"
  )

  redcap_logic_tr <- c(
    "head_household == \"Yes\"",
    "head_household == \"Yes\" & consent == \"No\" & !is.na(consent)",
    "type___88 == \"Checked\"",
    "signature == \"Yes\" & age < 18",
    "relation == \"Other\"",
    "fruit___app == \"Checked\""
  )

  expect_equal(
    translate_logic(
      redcap_logic,
      use_value_labs = TRUE,
      use_header_labs = FALSE,
      use_is_na = TRUE,
      use_in = FALSE,
      meta_factors = df_radio
    ),
    redcap_logic_tr
  )

})


test_that("translate_logic fails gracefully", {

  x <- "[var] = '1'"
  expect_error(translate_logic(x, use_value_labs = TRUE, meta_factors = NULL))
  expect_error(translate_logic(x, use_in = TRUE, meta_factors = NULL))
  expect_error(translate_logic(x, use_header_labs = TRUE, meta_dictionary = NULL))

  # test expression that is not parsable
  x <- "[var] != '1'" # expecting <> instead of !=
  expect_error(translate_logic(x, use_value_labs = FALSE, use_in = FALSE, on_error = "fail"))
  expect_warning(translate_logic(x, use_value_labs = FALSE, use_in = FALSE, on_error = "warn"))
  expect_true(is.na(translate_logic(x, use_value_labs = FALSE, use_in = FALSE, on_error = "ignore")))
})
