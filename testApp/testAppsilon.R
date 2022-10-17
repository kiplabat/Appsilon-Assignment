library(shinytest)
library(testthat)

# Initialize the test run with the correct parameters
# Loading timeout is necessarily longer to accommodate app size
app <- ShinyDriver$new("~/app.R",loadTimeout = 40000)

# Now start the testing function
test_that("App Works correctly",{
  # Give the app time to "breath" before sending a test
  Sys.sleep(4)
  
  # Test that: if input the SPECIES NAME in the selectizeInput, we will get its TOTAL NUMBER
  app$setInputs(sciName = "Calopteryx virgo - Beautiful Demoiselle")
  
  # Then the OUPUT should always be
  output <- app$getValue(name = "totNum")
  
  # What is the correct output?
  expect_equal(output, "- 138 -")  
})

app$stop()