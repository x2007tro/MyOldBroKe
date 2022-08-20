##
# Economic indicators tab
ei_tp <- tabPanel(
  "Economic Indicators",
  
  tabsetPanel(
    
    tabPanel(
      "General",
      column(econ_indi_panel_default_width,
             DT::dataTableOutput("gei_dt"))
    ),
    # End
    
    tabPanel(
      "Leading",
      column(econ_indi_panel_default_width,
             DT::dataTableOutput("lei_dt"))
    ),
    # End
    
    tabPanel(
      "Coincident",
      column(econ_indi_panel_default_width,
             DT::dataTableOutput("coi_dt"))
    ),
    # End
    
    tabPanel(
      "Lagging",
      column(econ_indi_panel_default_width,
             DT::dataTableOutput("lai_dt"))
    ),
    # End
    
    # Introduction to Economic Indicators
    tabPanel(
      "What are Economic Indicators?",
      column(
        12,
        br(),
        tags$div("An economic indicator is a statistic about an economic activity.
                  Economic indicators allow analysis of economic performance and predictions of future performance.
                  The indicators provided by this website are also referred to as The Conference Board Economic Indicators"),
        br(),
        tags$div("Useful links:"),
        tags$ul(
          tags$li(tags$a(href="https://www.investopedia.com/university/conferenceboard/", "The Conference Board Economic Indicators from Investopedia")),
          tags$li(tags$a(href="https://www.conference-board.org/", "The Conference Board Official Website"))
        ))
    )
    # End
    
  )
  
)