##
# Watchlist tabpanal
watchlist_tp <- tabPanel(
  "Watchlist",
  
  tabsetPanel(
    tabPanel(
      "Watchlist",
      
      fluidRow(
        column(
          6,
          tags$h4("Watchlist"),
          DT::dataTableOutput("wl_stwl_tbl")
        ),
        column(
          6,
          #tags$h4(tags$b("Watchlist"), style="float:left"),
          tags$h4("Search Bar"),
          tags$div(style="float:left; padding:0px; margin:0px; height:100%",
                   textInput("ticker_search", NULL, value = "AAPL-USD", width = blotter_field_default_width)
          ),
          tags$div(style="float:left; padding:0px; margin:0px; height:100%",
                   actionButton("ticker_search_submit", "Get quote", width = blotter_field_default_width)
          ),
          
          fluidRow(
            column(
              12,
              tags$h4("Most Recent Quote"),
              tags$div(style="padding:0px, margin:0px, height:100%",
                       DT::dataTableOutput("prev_day_quote"))
            )
          )
          
        )
      )
    ), # end of tabpanel
    
    tabPanel(
      "Price History",
      
      fluidRow(
        column(
          12,
          #tags$h5("Latest information"),
          tags$br(),
          #tags$h5("Historical Performance (1 Year)"),
          tags$div(style="padding:0px, margin:0px, height:100%",
                   plotOutput("hist_prc"))
        )
      )
    ), # end of price history tab
    
    tabPanel(
      "Cumulative Return",
      
      fluidRow(
        column(
          12,
          #tags$h5("Latest information"),
          #tags$h5("Historical Performance (1 Year)"),
          tags$br(),
          tags$div(style="padding:0px, margin:0px, height:100%",
                   plotOutput("hist_return"))
        ))
    ) # end of cumulative return tab
    
  ) # end of tabsetpanel
)