

#######To DO######

##add copy to clipboard button

##add an info popover for the guide panel so users know they will need to open web browser - will all webbrowsers work or just google?

##how can I create dynamic select input choices? so that user can pick their own topics...

##change main r and add the exports etc.

##is there an easier way than adding each topic as a conditional panel?


# Define the fields we want to save from the form




#' @export

EmailTemplate <- function(){
  
  require(shiny)
  require(tidyverse)
  
  fields <- c("type", "topic","Day", "Time", "greeting","name")
  
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
        
        #predefined types of responses
        selectInput("type","Choose a response type:",
                    c("first_response"= "first",
                      "reply"="reply",
                      "post_phone"="phone",
                      "update"="update")),
        
        #dynamic types of responses with 4 default options
        ##need to save new options for later usage
        ##option to delete options again
        uiOutput("selector_ui"),
        
        ##user can add another choice if needed
        textInput(
          "freetext",
          "Enter new choice if not present"
        ),
        
        ##to add the extra choice
        actionButton("add_btn", "Add to choices"),
        
        ##predfinied topics
        selectInput("topic","Choose a topic:",
                    list('post delivery/user phase'= list("damage","productcomplaint","invoice","return"),
                         'delivery phase'= list("where is my parcel","returned by courier"))),
        
        
        ##user can select days of the week
        selectInput("Day","Choose an ending:",
                    c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')),
        
        ##user can selec time of the day 
        selectInput("Time","choose time of the day:",
                    c("morning","afternoon","evening")),
        
        ##dynamic greeting with default value - user can pick another value and save it for next time
        textInput("Greeting","Type a greeting:","kind regards,"),
        
        ##dynamic input field, user can save and load values 
        textInput("Name","Type your Name:"),
        
        
        
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
  
  
  default_choices <- c("first_response", "reply","phone", "update")
  

  server <- function(input, output, session){
    
    output$type<-renderText({input$type})
    
    output$topic<-renderText({input$topic})
    
    
    
    type <- reactiveVal(default_choices)
    
    observeEvent(input$add_btn, {
      if (input$freetext == "") {
        showModal(
          modalDialog(
            title = "No new label entered!",
            "Please enter a label in the free text field",
            easyClose = FALSE
          )
        )
        return(NULL)
      } else {
        # add the free text file to the reactive values
        new_choices <- c(type(), input$freetext)
        type(new_choices)
      }
    })
    
    output$selector_ui <- renderUI({
      selectInput(
        "selector",
        "Choose label",
        choices = type(),
        selected = NULL,
        multiple = FALSE
      )
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
    
    output$ending<-renderText(paste0(
      "I wish you a beautiful"," ",{input$Day}," ",{input$Time},"!"))
    
    output$Greeting <- renderText({
      input$Greeting
    })
    
    output$Name <- renderText({
      input$Name
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
}

