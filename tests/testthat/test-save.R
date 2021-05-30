library(shiny)
library(testthat)
library(shinytest)


## What I want to test?
### I extracted a code snippet of my server code and saved it in R/save.R
### I named the function save_file() 
## the function is supposed to save the input data in a data frame and writes this data frame into a csv. file.
## If we test the function using a path to an empty csv file 
##it should create the same excel file as the user_inputs1.csv excel file 
##(the file that is created when saving the default inputs of the sidebar panel)



test_that("save_file() saves inputs", {
  
  #create and save an empty excel file 
  path_csv <- tempfile()
  write.csv(path_csv,row.names = FALSE)
  expect_identical(save_file(path_csv),read.csv("~/UVA/programming next step/Email_Agent_Template/R/user_inputs1.csv"))

})
