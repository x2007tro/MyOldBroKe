##
# Market news tab
market_news_tp <- tabPanel(
  "Market News",
  
  tabsetPanel(
   
    tabPanel(
      "Market News",
      
      fluidRow(
        column(12, id = "news",
               tags$div(class = "nano_block",
                        tags$h4("News", style="float:left"),
                        tags$div(style="float:right; padding:0px, margin:0px, height:100%",
                                 actionButton("ticker_news_submit", "Get news", width = blotter_field_default_width)
                        ),
                        tags$div(style="float:right; padding:0px, margin:0px, height:100%",
                                 textInput("ticker_news", NULL, value = "", width = blotter_field_default_width)
                        )
               )
        )
      ) # End
      
    ) # end of panel
    
  ) # end of tabsetpanel
  
)