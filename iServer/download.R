##
# handle dowmload options
##
output$download_file_selector <- renderUI({
  selectInput("dataset", "Choose a dataset:", 
              choices = names(port_info())[-1])
})

output$download_data <- downloadHandler(
  
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    paste0(input$dataset, ".csv")
  },
  
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file) {
    
    # Write to a file specified by the 'file' argument
    write.csv(port_info()[[input$dataset]], file, row.names = FALSE)
  }
)