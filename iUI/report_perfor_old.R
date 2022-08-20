##
# Performance
##
perfor_tp <- tabPanel(
  "Performance",
  
  tabsetPanel(
    
    # return table
    tabPanel(
      "Cumulative Return",
      
      fluidRow(
        column(
          12,
          tags$br(),
          tags$div(
            shypka.ddiv(tags$h5(
              style = "padding:4px",
              textOutput("last_update_time_perfor_table")
            ), color = "#ffe4e1"),    #misty rose
            DT::dataTableOutput("perfor_table")
          )
        )
      )
    ),
    
    # return curve
    tabPanel(
      "Account Value History",
      
      tabsetPanel(
        tabPanel(
          "YTD",
          tags$br(),
          fluidRow(
            column(
              12,
              plotOutput("perfor_graph_ytd")
            )
          )
          
        ),
        tabPanel(
          "One Year From Now",
          tags$br(),
          fluidRow(
            column(
              12,
              plotOutput("perfor_graph_yfn")
            )
          )
          
        ),
        tabPanel(
          "Since Inception",
          tags$br(),
          fluidRow(
            column(
              12,
              plotOutput("perfor_graph_sinc")
            )
          )
          
        )
      )
    
    )
    # End
  )
  
)