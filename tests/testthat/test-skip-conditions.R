# Test file to verify skip conditions in test-1-simple-tests.R

test_that("skip_on_cran() is called in simple model tests", {
  # Mock the skip_on_cran function to capture calls
  skip_calls <- 0
  
  # Create a mock environment that simulates CRAN
  withr::with_envvar(
    c("NOT_CRAN" = ""),
    {
      # Override skip_on_cran to count calls instead of actually skipping
      with_mocked_bindings(
        skip_on_cran = function() {
          skip_calls <<- skip_calls + 1
        },
        skip_on_ci = function() {
          # Do nothing for this test
        },
        {
          # Source the test file to check if skip_on_cran is called
          tryCatch({
            source("test-1-simple-tests.R", local = TRUE)
          }, error = function(e) {
            # Expected to fail due to missing dependencies, but skip_on_cran should still be called
          })
        }
      )
    }
  )
  
  # Verify that skip_on_cran was called at least once (should be 4 times for the 4 test cases)
  expect_true(skip_calls >= 1, info = "skip_on_cran() should be called in the test cases")
})

test_that("skip_on_ci() is called in simple model tests", {
  # Mock the skip_on_ci function to capture calls
  skip_calls <- 0
  
  # Create a mock environment that simulates CI
  withr::with_envvar(
    c("CI" = "true"),
    {
      # Override skip_on_ci to count calls instead of actually skipping
      with_mocked_bindings(
        skip_on_ci = function() {
          skip_calls <<- skip_calls + 1
        },
        skip_on_cran = function() {
          # Do nothing for this test
        },
        {
          # Source the test file to check if skip_on_ci is called
          tryCatch({
            source("test-1-simple-tests.R", local = TRUE)
          }, error = function(e) {
            # Expected to fail due to missing dependencies, but skip_on_ci should still be called
          })
        }
      )
    }
  )
  
  # Verify that skip_on_ci was called at least once (should be 4 times for the 4 test cases)
  expect_true(skip_calls >= 1, info = "skip_on_ci() should be called in the test cases")
})

test_that("skip functions work correctly on CRAN environment", {
  # Test that skip_on_cran actually skips when on CRAN
  withr::with_envvar(
    c("NOT_CRAN" = ""),
    {
      expect_error(
        {
          skip_on_cran()
          stop("This should not be reached on CRAN")
        },
        class = "skip"
      )
    }
  )
})

test_that("skip functions work correctly on CI environment", {
  # Test that skip_on_ci actually skips when on CI
  withr::with_envvar(
    c("CI" = "true"),
    {
      expect_error(
        {
          skip_on_ci()
          stop("This should not be reached on CI")
        },
        class = "skip"
      )
    }
  )
})