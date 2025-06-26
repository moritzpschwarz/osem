# Test skip behavior and conditions

test_that("skip_on_cran behaves correctly in different environments", {
  # Test when NOT_CRAN is not set (simulates CRAN environment)
  withr::with_envvar(
    c("NOT_CRAN" = ""),
    {
      expect_error(
        skip_on_cran(),
        class = "skip",
        info = "skip_on_cran() should throw skip error on CRAN"
      )
    }
  )
  
  # Test when NOT_CRAN is set to "true" (simulates local/non-CRAN environment)
  withr::with_envvar(
    c("NOT_CRAN" = "true"),
    {
      # Should not throw an error
      expect_silent(skip_on_cran())
    }
  )
})

test_that("skip_on_ci behaves correctly in different environments", {
  # Test when CI is set to "true" (simulates CI environment)
  withr::with_envvar(
    c("CI" = "true"),
    {
      expect_error(
        skip_on_ci(),
        class = "skip",
        info = "skip_on_ci() should throw skip error on CI"
      )
    }
  )
  
  # Test when CI is not set (simulates local environment)
  withr::with_envvar(
    c("CI" = ""),
    {
      # Should not throw an error
      expect_silent(skip_on_ci())
    }
  )
})

test_that("model tests would be skipped on CRAN", {
  # Simulate CRAN environment
  withr::with_envvar(
    c("NOT_CRAN" = ""),
    {
      # Create a mock test function that includes skip_on_cran
      mock_test <- function() {
        skip_on_cran()
        # This would be the actual test code
        return("test executed")
      }
      
      # Expect the test to be skipped
      expect_error(mock_test(), class = "skip")
    }
  )
})

test_that("model tests would be skipped on CI", {
  # Simulate CI environment
  withr::with_envvar(
    c("CI" = "true"),
    {
      # Create a mock test function that includes skip_on_ci
      mock_test <- function() {
        skip_on_ci()
        # This would be the actual test code
        return("test executed")
      }
      
      # Expect the test to be skipped
      expect_error(mock_test(), class = "skip")
    }
  )
})

test_that("skip functions work together correctly", {
  # Both skip conditions should work independently
  # When both are present, either can trigger a skip
  
  # Test with CRAN environment (skip_on_cran should trigger)
  withr::with_envvar(
    c("NOT_CRAN" = "", "CI" = "false"),
    {
      mock_test_both <- function() {
        skip_on_cran()
        skip_on_ci()
        return("test executed")
      }
      
      expect_error(mock_test_both(), class = "skip")
    }
  )
  
  # Test with CI environment (skip_on_ci should trigger)
  withr::with_envvar(
    c("NOT_CRAN" = "true", "CI" = "true"),
    {
      mock_test_both <- function() {
        skip_on_cran()
        skip_on_ci()
        return("test executed")
      }
      
      expect_error(mock_test_both(), class = "skip")
    }
  )
  
  # Test with neither environment (should not skip)
  withr::with_envvar(
    c("NOT_CRAN" = "true", "CI" = "false"),
    {
      mock_test_both <- function() {
        skip_on_cran()
        skip_on_ci()
        return("test executed")
      }
      
      expect_equal(mock_test_both(), "test executed")
    }
  )
})