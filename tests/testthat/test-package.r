
test_that("package data exists", {
  # Check if sample_abs_data exists when loaded
  data(sample_abs_data, package = "oasr", envir = environment())
  expect_true(exists("sample_abs_data"))
  
  # Check structure if it exists
  if (exists("sample_abs_data")) {
    expect_s3_class(sample_abs_data, "data.frame")
    expect_true(all(c("asset_id", "balance", "wac", "term") %in% names(sample_abs_data)))
  }
  
  # Check if sample_deal_structure exists
  data(sample_deal_structure, package = "oasr", envir = environment())
  expect_true(exists("sample_deal_structure"))
  
  # Check structure if it exists
  if (exists("sample_deal_structure")) {
    expect_type(sample_deal_structure, "list")
    expect_true(all(c("bonds", "waterfall") %in% names(sample_deal_structure)))
  }
})

test_that("package namespace is properly structured", {
  # Verify that key functions are exported
  expect_true("calculate_oas" %in% getNamespaceExports("oasr"))
  expect_true("prepayment_model" %in% getNamespaceExports("oasr"))
  expect_true("project_cashflows" %in% getNamespaceExports("oasr"))
  expect_true("simulate_rates" %in% getNamespaceExports("oasr"))
  
  # Check if important internal functions exist
  expect_true(exists("create_sample_abs_data", envir = asNamespace("oasr")))
  expect_true(exists("create_sample_deal_structure", envir = asNamespace("oasr")))
})

test_that("S3 methods are properly registered", {
  # getS3method() rather than isS3method(): the latter reports FALSE under
  # devtools::load_all() even when the method is registered and dispatches.
  expect_false(is.null(getS3method("print", "oas_result", optional = TRUE)))
  expect_false(is.null(getS3method("summary", "oas_result", optional = TRUE)))
  expect_false(is.null(getS3method("plot", "oas_result", optional = TRUE)))
  
  # Check if print and plot methods for oas_sensitivity are registered
  expect_false(is.null(getS3method("print", "oas_sensitivity", optional = TRUE)))
  expect_false(is.null(getS3method("plot", "oas_sensitivity", optional = TRUE)))
  
  # Check if plot method for oas_surface is registered
  expect_false(is.null(getS3method("plot", "oas_surface", optional = TRUE)))
})
