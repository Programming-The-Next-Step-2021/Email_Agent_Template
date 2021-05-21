library(shiny)
library(tidyverse)
library(rdrop2)

# Define the fields we want to save from the form
fields <- c("type", "topic","ending", "greeting","name")


drop_auth()
token <- drop_auth()
saveRDS(token, file = "token.rds")

# in any drop_* function, pass `dtoken = token
# Tokens are valid until revoked.

outputDir <- "Email Templates"

saveData <- function(data) {
  
  data <- t(data)
  
  # Create a unique file name
  
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  
  # Write the data to a temporary file locally
  
  filePath <- file.path(tempdir(), fileName)
  
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  
  drop_upload(filePath, path = outputDir, dtoken = token)
  
}

loadData <- function() {
  
  # Read all the files into a list
  filesInfo <- drop_dir(outputDir, dtoken = token)
  
  filePaths <- filesInfo$path_display
  
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}

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
                  list(`Monday`= list("morning","afternoon","evening"),
                       `Tuesday`= list("morning","afternoon","evening"),
                       `Wednesday`= list("morning","afternoon","evening"),
                       `Thursday`= list("morning","afternoon","evening"),
                       `Friday`= list("morning","afternoon","evening"),
                       `Saturday`= list("morning","afternoon","evening"),
                       `Sunday`= list("morning","afternoon","evening"))
                  ),
      
      textInput("Greeting","Type a greeting:","kind regards,"),
      textInput("Name","Type your Name:"),
      actionButton("submit", "Submit")
     
    ),
    
    mainPanel(
      textOutput("type"),
      textOutput("topic"),
      textOutput("ending"),
      textOutput("greeting"),
      textOutput("name"),
      
    ),
  )
)

server <- function(input, output, session){
  
  output$type<-renderText({input$type})
  
  output$topic<-renderText({input$topic})
  
  output$ending<-renderText({input$ending})
  
  output$greeting<-renderText({input$Greeting})
  
  output$name<-renderText({input$Name})
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$responses <- DT::renderDataTable({
    input$submit
    loadData()
  }) 
 
}
shinyApp(ui=ui,server = server)
