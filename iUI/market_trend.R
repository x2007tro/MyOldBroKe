##
# Market trend tab
market_trend_tp <- tabPanel(
  "Market Trend",
  
  tabsetPanel(
    # Equity curve
    
    tabPanel(
      "Equity",
      tags$br(),
      plotOutput("equity_mkt"),
      tags$div(style="float:right; padding:0px, margin:0px, height:50%",
               selectInput("eq_perf_period", NULL, choices = c("5D", "1M", "3M", "6M", "1Y", "3Y", "5Y", "YTD"), selected = "1Y", width = blotter_field_default_width)
      )      
    ),
    # End
    
    # Bond
    tabPanel(
      "Treasury bond",
      tags$br(),
      plotOutput("tbond_mkt"),
      tags$div(style="float:right; padding:0px, margin:0px, height:50%",
               selectInput("tb_perf_period", NULL, choices = c("5D", "1M", "3M", "6M", "1Y", "3Y", "5Y", "YTD"), selected = "1Y", width = blotter_field_default_width)
      )
    ),
    # End
    
    # Treasury yield curve
    tabPanel(
      "Corporate bond",
      tags$br(),
      plotOutput("cbond_mkt"),
      tags$div(style="float:right; padding:0px, margin:0px, height:50%",
               selectInput("cb_perf_period", NULL, choices = c("5D", "1M", "3M", "6M", "1Y", "3Y", "5Y", "YTD"), selected = "1Y", width = blotter_field_default_width)
      )
    )
    # End
  )
  
)