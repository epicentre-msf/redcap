context("project_xml")

test_that("project_xml works as expected", {

  skip_if(conn_test$token == "")

  x1 <- project_xml(conn_test)
  expect_s3_class(x1, "xml_document")

  x1_parse <- parse_xml(x1)
  expect_s3_class(x1_parse, "tbl_df")
  expect_named(x1_parse, c("record_id", "form", "redcap_event", "redcap_repeat_instance", "field", "value"))

  x2 <- project_xml(conn_test, meta_only = TRUE)
  expect_length(xml2::xml_find_all(x2, "//d1:SubjectData"), 0)

  records_focal <- c("0001", "0003")
  x3 <- project_xml(conn_test, records = records_focal)
  expect_setequal(parse_xml(x3)$record_id, records_focal)

  fields_focal <- c("record_id", "enr_signed_date", "follow_date")
  x4 <- project_xml(conn_test, fields = fields_focal)
  expect_setequal(parse_xml(x4)$field, fields_focal)

  events_focal <- c("enrollment_arm_1", "unscheduled_follow_arm_1")
  x5 <- project_xml(conn_test, events = events_focal)
  expect_setequal(parse_xml(x5)$redcap_event, events_focal)
})


test_that("project_xml fails gracefully", {

  expect_error(project_xml(conn_test, fields = c("record_id", "fake_field")))
  expect_error(project_logging(conn_test, events = c("fake_event")))
})
