test_that("social_contacts module returns the expected results", {
  expect_snapshot(social_contacts_ui("example"))
})
