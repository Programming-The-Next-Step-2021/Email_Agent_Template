library(shiny)
library(testthat)
library(shinytest)

app <- ShinyDriver$new("~/UVA/programming next step/Email_Agent_Template/tests/app")

vals <- app$getAllValues()

str(vals)



library(EmailTemplate) 

context('core Email Template functionality')

test_that('functionality of load data function',{
  expect_identical(vals$export, read.csv("~/UVA/programming next step/Email_Agent_Template/expected_output.csv"))
})





