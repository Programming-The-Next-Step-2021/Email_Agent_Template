library(shiny)
library(testthat)
library(shinytest)

#widget name - add_btn
#widget name - freetext
#widget name - selector



#open shiny app
app <- ShinyDriver$new("~/UVA/programming next step/Email_Agent_Template/tests/app")


test_that("choices(),adding choices works", {
  
  
  
  #set input for freetext
  app$setInputs(freetext = "hello")
  
  #set input for add_btn
  app$click(name = add_btn)
  
  #set input for selector
  app$setInputs(selector = "hello")

  #get input for selector
  choice <- app$getValue(name = selector)
  
  #test
  expect_equal(choice, "hello" )
})

#stop shiny app
app$stop