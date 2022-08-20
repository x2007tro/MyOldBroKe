##
# Trade hist tabpanel
account_activity_tp <- tabPanel(
  "Activity",
  
  tabsetPanel(
    tabPanel(
      "Past Trades",
      tags$br(),
      fluidRow(
        column(
          12,
          DT::dataTableOutput("past_trades")
        )
      )
    ),
    tabPanel(
      "Realized Profit",
      tags$br(),
      fluidRow(
        column(
          12,
          DT::dataTableOutput("real_profit")
        )
      )
    ),
    tabPanel(
      "Log",
      tags$br(),
      fluidRow(
        column(
          12,
          DT::dataTableOutput("err_log")
        )
      )
    ),
    tabPanel(
      "Past Messages",
      tags$br(),
      fluidRow(
        column(
          12,
          DT::dataTableOutput("past_messages")     
        )
      )
    )
  )
  
)