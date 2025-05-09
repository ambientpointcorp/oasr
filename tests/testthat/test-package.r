context("Package Structure and Data")

test_that("package data exists", {
  # Check if sample_abs_data exists when loaded
  data(sample_abs_data, package = "oasR", envir = environment())
  expect_true(exists("sample_abs_data"))
  
  # Check structure if it exists
  if (exists("sample_abs_data")) {
    expect_is(sample_abs_data, "data.frame")
    expect_true(all(c("asset_id", "balance", "wac", "wam") %in% names(sample_abs_data)))
  }
  
  # Check if sample_deal_structure exists
  data(sample_deal_structure, package = "oasR", envir = environment())
  expect_true(exists("sample_deal_structure"))
  
  # Check structure if it exists
  if (exists("sample_deal_structure")) {
    expect_is(sample_deal_structure, "list")
    expect_true(all(c("bonds", "waterfall") %in% names(sample_deal_structure)))
  }
})

test_that("package namespace is properly structured", {
  # Verify that key functions are exported
  expect_true("calculate_oas" %in% getNamespaceExports("oasR"))
  expect_true("prepayment_model" %in% getNamespaceExports("oasR"))
  expect_true("project_cashflows" %in% getNamespaceExports("oasR"))
  expect_true("simulate_rates" %in% getNamespaceExports("oasR"))
  
  # Check if important internal functions exist
  expect_true(exists("create_sample_abs_data", envir = asNamespace("oasR")))
  expect_true(exists("create_sample_deal_structure", envir = asNamespace("oasR")))
})

test_that("S3 methods are properly registered", {
  # Check if print and summary methods for oas_result are registered
  expect_true(isS3method("print", "oas_result"))
  expect_true(isS3method("summary", "oas_result"))
  expect_true(isS3method("plot", "oas_result"))
  
  # Check if print and plot methods for oas_sensitivity are registered
  expect_true(isS3method("print", "oas_sensitivity"))
  expect_true(isS3method("plot", "oas_sensitivity"))
  
  # Check if plot method for oas_surface is registered
  expect_true(isS3method("plot", "oas_surface"))
})
