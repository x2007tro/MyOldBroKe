##
# Forex blotter tabPanel
forex_blotter_tp <- tabPanel(
  "Forex",
  
  ##
  # Main panel
  tabsetPanel(
    tabPanel(
      "Blotter",
      tags$br(),
      tags$div(
        style="display:block",
        fluidRow(
          column(
            12,
            tags$div(class = "blotter_fields_wide", selectInput("tgt_curr", "Target Currency", choices = tradable_curr, selectize = TRUE, multiple = FALSE, width = blotter_field_default_width_wide)),
            tags$div(class = "blotter_fields_wide", numericInput("tgt_val", "Target Value", 0, width = blotter_field_default_width_wide)),
            tags$div(class = "blotter_fields_wide", selectInput("req_curr", "Required Currency", choices = "CAD", selectize = TRUE, multiple = FALSE, width = blotter_field_default_width_wide)),
            tags$div(class = "blotter_fields_wide", textInput("req_val", "Required Value", value = "0", width = blotter_field_default_width_wide)),
            tags$div(class = "blotter_fields", style = "padding-top:20px", checkboxInput('forex_trade_transmit', "Transmit", value = FALSE, width = blotter_field_default_width)),
            tags$div(
              class = "blotter_fields_wide", style = "padding-top:20px", 
              actionButton(class="btn-primary", "request_forex", "Request", width = blotter_field_default_width)
            ),
            tags$div(
              class = "blotter_fields_wide", style = "padding-top:20px", 
              actionButton(class="btn-primary", "trade_forex", "Trade", width = blotter_field_default_width)
            )
          ))
        
      ),
      
      tags$br(),
      tags$div(style = "border:1px gray solid"),
      
      ##
      # Message div
      fluidRow(
        column(
          12,
          tags$div(
            tags$h4(tags$b("Message")),
            tags$br(),
            tags$div(style="display:block", textOutput("forex_trade_msg", inline = FALSE))
          )
        )
      )
    ),
    
    ##
    # Contract Details tabPanel
    tabPanel(
      "Contract Details",
      DT::dataTableOutput("forex_cd")
    )
    
  )
)