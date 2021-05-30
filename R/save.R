
save_file <- function(path){
  path <- path
  inputs_to_save <- c('Salutation','selector',
                      'selector2','Day','Time','Greeting', 'Name')
  
  input <- c('Dear,...','first_response','damage','Monday','morning','kind regards','')
  
  inputs<-NULL
  
  # Append all inputs before saving to folder
  for(input.i in inputs_to_save){
    inputs <- append(inputs, input[input.i])
  }
  
  inputs_data_frame <- data.frame(inputId = inputs_to_save, 
                                             value = inputs)
             
             write.csv(inputs_data_frame, 
                       file = path, row.names = FALSE)
}

