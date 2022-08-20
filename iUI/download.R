##
# A download page
##
download_tp <- tabPanel(
  "Downloads",
  
  tabsetPanel(
    tabPanel(
      "Download Page",
      
      fluidRow(
        column(
          12,
          tags$div(
            shypka.ddiv(
              uiOutput("download_file_selector")
            )
          ),
          tags$div(
            shypka.ddiv(
              downloadButton(class = "btn-primary", 'download_data', 'Download')
            )
          )
        )
      )  
    )  # end of nonforex security
    
    # second tabPanel can start here
    
  )
)