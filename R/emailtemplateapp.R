library(shiny)
library(tidyverse)

#requirements
  #select response type 
  #select topic
  #select ending
  #select greeting 
  #add logo
  #custom design
  #ability to save pre-witten content 

ui <- fluidPage(
  #Application title
  theme = bslib::bs_theme(bootswatch = "darkly"),
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      /* Change font of header text */
      h2 {
        font-family: 'Yusei Magic', sans-serif;
      }")),
  tags$link(rel = "stylesheet",type = "button/css", href="bootstrap.min.css")),
  titlePanel("Custom Email Templates"),
  sidebarLayout(
    sidebarPanel(
      selectInput("type","Choose a response type:",
                  c("first_response"="first",
                    "reply"="reply",
                    "post_phone"="phone",
                    "update"="update")),
      selectInput("topic","Choose a topic:",
                  list('post delivery/user phase'= list("damage","productcomplaint","invoice","return"),
                       'delivery phase'= list("where is my parcel","returned by courier"))),
      selectInput("ending","Choose an ending:",
                  list('Monday'= list("morning","afternoon","evening"),
                       'Tuesday'= list("morning","afternoon","evening"),
                       'Wednesday'= list("morning","afternoon","evening"),
                       'Thursday'= list("morning","afternoon","evening"),
                       'Friday'= list("morning","afternoon","evening"),
                       'Saturday'= list("morning","afternoon","evening"),
                       'Sunday'= list("morning","afternoon","evening"))),
      textInput("Greeting","Type a greeting:","kind regards,"),
      textInput("Name","Type your Name:"),
      submitButton("Update Template", icon("refresh"))
      
    ),
    mainPanel(
      textOutput("type"),
      textOutput("topic"),
      textOutput("ending"),
      textOutput("greeting"),
      textOutput("name")
    ),
  )
)

server <- function(input, output){
  output$type<-renderText({input$type})
  output$topic<-renderText({input$topic})
  output$ending<-renderText({input$ending})
  output$greeting<-renderText({input$Greeting})
  output$name<-renderText({input$Name})
}
shinyApp(ui=ui,server = server)
