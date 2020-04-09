test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



test_that("test that all page headers are within their bounds defined by the excel sheet they originated from", {




})




testthat::expect(
  ok = all(header$Page >= header$Startpage & header$Page <= header$Endpage),
  failure_message = "Not all of the header pages are in their right place.")
















