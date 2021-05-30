

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
  
  default_choices <- c("first_response", "reply","phone", "update")
  
  default_topics <- c("damage","productcomplaint","return","where is my parcel")
  
  ui <- fluidPage(
    
    #implementing bootstrap css dark mode
    theme = bslib::bs_theme(bootswatch = "darkly"),
    
    #adding custom css via tags$head 
    tags$head(
      
      tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      /* Change font of header text */
      h2 {
        font-family: 'Yusei Magic', sans-serif;
      }")),
      
      tags$style(HTML('#add_btn{font-size:small}')),
      tags$style(HTML('#add_btn2{font-size:small}')),
      tags$style(HTML('#add_btn{background-color:#B8B8AA}')),
      tags$style(HTML('#add_btn2{background-color:#B8B8AA}')),
      tags$style(HTML('#load_inputs{font-size:small}')),
      tags$style(HTML('#save_inputs{font-size:small}')),
      tags$style(HTML('#load_inputs{background-color:#595D88}')),
      tags$style(HTML('#save_inputs{background-color:#DB7543}')),
      tags$style(HTML('#load_inputs2{font-size:small}')),
      tags$style(HTML('#save_inputs2{font-size:small}')),
      tags$style(HTML('#load_inputs2{background-color:#595D88}')),
      tags$style(HTML('#save_inputs2{background-color:#DB7543}'))
    ),
    
    #Application title
    titlePanel("Custom Email Templates"),
    
    
    sidebarLayout(
      
      sidebarPanel(
        
        
        tags$b(textInput(inputId = 'inputsLocation', label = 'Inputs Location', value = "~/UVA/programming next step/Email_Agent_Template/R/user_inputs1.csv")),
        
        tags$p(""),
        
        tags$b(textInput(
          "Salutation",
          "Salutation",
          "Dear...,"
        )),
        
        
        #dynamic types of responses with 4 default options
        ##need to save new options for later usage
        ##option to delete options again
        selectInput("selector",
                    "Choose opening line",
                    choices = default_choices,
                    selected = NULL,
                    multiple = FALSE),
        
        ##user can add another choice if needed
        textInput(
          "freetext",
          "Enter new choice if not present"
        ),
        
        ##to add the extra choice
        
        actionButton("add_btn", "Add to choices"),
        
        tags$p(""),
        
        selectInput("selector2",
                    "Choose topic",
                    choices = default_topics,
                    selected = NULL,
                    multiple = FALSE),
        
        
        ##user can add another choice if needed
        textInput(
          "freetext2",
          "Enter new choice if not present"
        ),
        
        ##to add the extra choice
        actionButton("add_btn2", "Add to choices"),
        
        tags$p(""),
        
        ##user can select days of the week
        selectInput("Day","Choose day of week:",
                    c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')),
        
        ##user can selec time of the day 
        selectInput("Time","Choose time of day:",
                    c("morning","afternoon","evening")),
        
        ##dynamic greeting with default value - user can pick another value and save it for next time
        textInput("Greeting","Closing line:","kind regards,"),
        
        ##dynamic input field, user can save and load values 
        textInput("Name","Insert your Name:"),
        
        ## action buttons to save and load inputs
        actionButton('load_inputs', 'Load inputs'),
        actionButton('save_inputs', 'Save inputs'),
        
        
      ),
      
      mainPanel(
        tabsetPanel(type ="tabs",
                    tabPanel("User Guide",
                             
                             tags$iframe(style="height:450px; width:100%; scrolling=yes",
                                         src="https://www.dropbox.com/s/6us8lvwrpvyqqxa/userguide.pdf?raw=1")),
                    tabPanel("Content Editor",
                             
                             tags$p(""),
                             
                             textInput(inputId = 'inputsLocation2', label = 'Inputs Location2', value = "~/UVA/programming next step/Email_Agent_Template/R/type.csv"),
                             
                             tags$div(id = 'placeholder'),
                             
                             conditionalPanel(
                               condition = "input.selector == 'first_response'",
                               
                               textInput('first_response','first')),
                             conditionalPanel(
                               condition = "input.selector == 'reply'",
                               textInput('reply','reply')),
                             conditionalPanel(
                               condition = "input.selector == 'phone'",
                               textInput('phone','phone')),
                             conditionalPanel(
                               condition = "input.selector == 'update'",
                               textInput('update','update')),
                             conditionalPanel(
                               condition = "input.selector2 == 'damage'",
                               
                               textAreaInput('damage','damage')),
                             conditionalPanel(
                               condition = "input.selector2 == 'productcomplaint'",
                               
                               textAreaInput('productcomplaint','productcomplaint')),
                             conditionalPanel(
                               condition = "input.selector2 == 'return'",
                               
                               textAreaInput('return','return')),
                             conditionalPanel(
                               condition = "input.selector2 == 'where is my parcel'",
                               
                               textAreaInput('parcel','where is my parcel')),
                             
                             actionButton('load_inputs2', 'Load inputs2'),
                             actionButton('save_inputs2', 'Save inputs2'),
                             
                    ),
                    
                    tabPanel("Template",
                             
                             tags$p(textOutput("Salutation")),
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
  
  
  
  
  dataitems <- c(default_choices, default_topics)
  
  server <- function(input, output, session){
    
    output$selector<-renderText({input$selector})
    
    output$selector2<-renderText({input$selector2})
    
    
    
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
        
        updateSelectInput( session,
                           inputId = "selector",
                           "Choose opening line",
                           choices = c(default_choices,input$freetext),
                           selected = NULL
        )
      }
    })
    
    
    
    
    topic <- reactiveVal(default_topics)
    
    observeEvent(input$add_btn2, {
      if (input$freetext2 == "") {
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
        new_topics <- c(topic(), input$freetext2)
        updateSelectInput( session,
                           inputId = "selector2",
                           "Choose topic",
                           choices = c(default_topics,input$freetext2),
                           selected = NULL
        )
      }
    })
    
    
    
    output$Salutation <- renderText({
      input$Salutation
    }) 
    
    
    output$summary <- renderText({
      if(input$selector=="reply"){
        input$reply
      }else if(input$selector=="first"){
        input$first_response
      }else if(input$selector=="phone"){
        input$phone
      }else if(input$selector=="update"){
        input$update
      }
    })
    
    output$topsummary <- renderText({
      if(input$selector2=="damage"){
        input$damage
      }
      else if(input$selector2=="productcomplaint"){
        input$productcomplaint
      }
      else if(input$selector2=="return"){
        input$return
      }
      else if(input$selector2=="where is my parcel"){
        input$parcel
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
      
      inputs_to_save <- c('Salutation','selector','selector2','Day','Time','Greeting', 'Name')
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
      inputs_to_save <- c('first_response','reply','phone','update','damage',"productcomplaint","return","parcel")
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

