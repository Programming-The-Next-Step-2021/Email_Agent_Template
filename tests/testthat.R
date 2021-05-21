library(testthat)
library(shinytest)
recordTest("R/simple-app/")

app <- ShinyDriver$new("R/simple-app/")

vals <- app$getAllValues()

str(vals)


library(testthat)
library(Email_Agent_Template)

context('core Email Template functionality')

test_check("Email_Agent_Template")

test_that('functionality of load data function',{
  expect_identical(vals$export, read.csv("expected_output.csv"))
})


##if a certain input is given, function should produce a certain ##
##excel file which is save to the dropbox account##



