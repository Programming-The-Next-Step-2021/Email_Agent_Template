library(shiny)
library(testthat)
library(shinytest)

#testing shiny app
context('core Email Template functionality')

#open shiny app
app <- ShinyDriver$new("~/UVA/programming next step/Email_Agent_Template/tests/app")

#get all values
vals <- app$getAllValues()

str(vals)


test_that('functionality of load data function',{
  expect_identical(vals$export, read.csv("~/UVA/programming next step/Email_Agent_Template/expected_output.csv"))
})


#stop shiny app
app$stop

