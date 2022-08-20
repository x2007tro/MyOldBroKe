##
# Configuration tab
conf_tp <- tabPanel(
  "Configuration",
  
  tabsetPanel(
   
    tabPanel(
      "Configuration",
      
      tags$h5("Options"),
      tags$div(actionButton("config_open", "Start TWS", width = blotter_field_default_width)),
      tags$div(actionButton("config_close", "End TWS", width = blotter_field_default_width))
      #tags$div("Paper/Live"),
      #tags$div("Foreign Exchange order")
      
    ) # end of panel
    
  ) # end of tabsetpanel
  
)