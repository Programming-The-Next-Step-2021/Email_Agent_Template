library(shiny)


ui <- fluidPage(
  titlePanel(titel="Email Template Creator"),
  sidebarLayout(
    sidebarPanel("select the topics"),
    mainPanel("this is the email template, output is displayed here")
  )
)