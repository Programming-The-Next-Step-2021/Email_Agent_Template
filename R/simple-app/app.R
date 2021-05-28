library(shiny)
library(tidyverse)
library(rdrop2) ##only if I need to connect app to my dropbox account-or maybe also for the user guide##
library(readr) ##only if I need to read in a text file##

#######To DO######

##add copy to clipboard button

##add an info popover for the guide panel so users know they will need to open web browser - will all webbrowsers work or just google?

##add the option to create on sidepanel inputs---

##change main r and add the exports etc.


# Define the fields we want to save from the form
fields <- c("type", "topic","Day", "Time", "greeting","name")



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
                  c("first_response"= "first",
                    "reply"="reply",
                    "post_phone"="phone",
                    "update"="update")),
      
      selectInput("topic","Choose a topic:",
                  list('post delivery/user phase'= list("damage","productcomplaint","invoice","return"),
                       'delivery phase'= list("where is my parcel","returned by courier"))),
      
      selectInput("Day","Choose an ending:",
                  c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')),
      
      selectInput("Time","choose time of the day:",
                  c("morning","afternoon","evening")),
      
      textInput("Greeting","Type a greeting:","kind regards,"),
      textInput("Name","Type your Name:"),
      
      actionButton("submit", "Submit"),
      textInput(inputId = 'inputsLocation', label = 'Inputs Location', value = "~/UVA/programming next step/Email_Agent_Template/R/simple-app/user_inputs1.csv"),
      actionButton('load_inputs', 'Load inputs'),
      actionButton('save_inputs', 'Save inputs')
    ),
    
    mainPanel(
      tabsetPanel(type ="tabs",
        tabPanel("User Guide",
                 
                 tags$iframe(style="height:450px; width:100%; scrolling=yes",
                             src="https://www.dropbox.com/s/6us8lvwrpvyqqxa/userguide.pdf?raw=1")),
        tabPanel("Summary",
                 textInput(inputId = 'inputsLocation2', label = 'Inputs Location2', value = "~/UVA/programming next step/Email_Agent_Template/R/simple-app/type.csv"),
                 
                 conditionalPanel(
                   condition = "input.type == 'first'",

                   textInput('first_response','first')),
                 conditionalPanel(
                   condition = "input.type == 'reply'",
                 textInput('reply','reply')),
                 conditionalPanel(
                   condition = "input.type == 'phone'",
                   textInput('phone','phone')),
                 conditionalPanel(
                   condition = "input.type == 'update'",
                   textInput('update','update')),
                 conditionalPanel(
                   condition = "input.topic == 'damage'",
                   
                   textAreaInput('damage','damage')),
                 
                 actionButton('load_inputs2', 'Load inputs2'),
                 actionButton('save_inputs2', 'Save inputs2'),
                 
                 ),
                 
        tabPanel("Template",
                
                 tags$p(textOutput("summary")),
                 tags$p(textOutput("topsummary")),
                 tags$p(textOutput("ending")),
                 tags$p(textOutput("Greeting")),
                 tags$p(textOutput("Name")),
                 tags$p(""),
                 tags$p(""),
                 tags$p("")
                 
                 )
      )
      
    ),
  )
)

server <- function(input, output, session){
  
  output$type<-renderText({input$type})
  
  output$topic<-renderText({input$topic})
  
  
  
  
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
      
  })
  
  
output$summary <- renderText({
  if(input$type=="reply"){
    input$reply
  }else if(input$type=="first"){
    input$first_response
  }else if(input$type=="phone"){
    input$phone
  }else if(input$type=="update"){
    input$update
  }
  })

output$topsummary <- renderText({
  if(input$topic=="damage"){
    input$damage
  }
})

output$ending<-renderText(paste0("I wish you a beautiful"," ",{input$Day}," ",{input$Time},"!"))

output$Greeting <- renderText({
  input$Greeting
})

output$Name <- renderText({
  input$Name
})
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$responses <- DT::renderDataTable({
    input$submit
    loadData()
  }) 
  
  observeEvent(input$load_inputs, {
    # Load inputs
    uploaded_inputs <- read.csv(input$inputsLocation)
    # Update each input
    for(i in 1:nrow(uploaded_inputs)){
      updateTextInput(session,
                      inputId = uploaded_inputs$inputId[i],
                      value = uploaded_inputs$value[i])
    }
  })
  
  observeEvent(input$save_inputs, {
    # Define inputs to save
    inputs_to_save <- c('topic','type','Day','Time','Greeting', 'Name')
    # Declare inputs
    inputs <- NULL
    # Append all inputs before saving to folder
    for(input.i in inputs_to_save){
      inputs <- append(inputs, input[[input.i]])
    }
    # Inputs data.frame
    inputs_data_frame <- data.frame(inputId = inputs_to_save, value = inputs)
    # Save Inputs
    write.csv(inputs_data_frame, file = input$inputsLocation, row.names = FALSE)
  }) 
  
  observeEvent(input$load_inputs2, {
    # Load inputs
    uploaded_inputs <- read.csv(input$inputsLocation2)
    # Update each input
    for(i in 1:nrow(uploaded_inputs)){
      updateTextInput(session,
                      inputId = uploaded_inputs$inputId[i],
                      value = uploaded_inputs$value[i])
    }
  })
  
  observeEvent(input$save_inputs2, {
    # Define inputs to save
    inputs_to_save <- c('first_response','reply','phone','update','damage')
    # Declare inputs
    inputs <- NULL
    # Append all inputs before saving to folder
    for(input.i in inputs_to_save){
      inputs <- append(inputs, input[[input.i]])
    }
    # Inputs data.frame
    inputs_data_frame <- data.frame(inputId = inputs_to_save, value = inputs)
    # Save Inputs
    write.csv(inputs_data_frame, file = input$inputsLocation2, row.names = FALSE)
  }) 
  
  
}
shinyApp(ui=ui,server = server)
