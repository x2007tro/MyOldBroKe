##
# Option blotter tabPanel
option_blotter_tp <- tabPanel(
  "Vanilla Option",
  
  tabsetPanel(
    ##
    # 1st panel
    tabPanel(
      "Blotter",
      
      tags$br(),
      
      fluidRow(
        column(
          12,
          tags$div(
            style="display:block",
            # tags$div(class = "blotter_fields", "IB Symbol"),
            # tags$div(class = "blotter_fields", "Right"),
            # tags$div(class = "blotter_fields_wide", "Expiry"),
            # tags$div(class = "blotter_fields", "Strike"),
            # tags$div(class = "blotter_fields", "Currency"),
            # tags$div(class = "blotter_fields", "Side"),
            # tags$div(class = "blotter_fields", "Shares"),
            # tags$div(class = "blotter_fields", "Type"),
            # tags$div(class = "blotter_fields", "Limit Price"),
            # tags$div(class = "blotter_fields", "Multiplier"),
            # tags$div(class = "blotter_fields", "Trade Value"),
            # tags$div(class = "blotter_fields", "Transmit"),
            
            lapply(1:opt_max_blotter_size, function(i){
              fluidRow(
                column(
                  12,
                  tags$div(
                    style="display:block", uiOutput(paste0('opt_trade_item', i), inline = FALSE))
                ))
            })
          )            
        )
      ), # end of equity div
      
      tags$div(style = "border:1px gray solid"),
      
      ##
      # cancel order div
      fluidRow(
        # Cancel order column
        column(
          12, 
          tags$div(
            fluidRow(column(
              12,
              shypka.ddiv(tags$h4(tags$b("Active Orders"))),   # salmon
              tags$div(textOutput("opt_current_active_trades")),
              tags$br(),
              tags$div(
                id = "cancel_all_trades", 
                actionButton(class = "btn-primary", "opt_cancel_all_trades", "Cancel All", width = blotter_field_default_width)
              )
            ))
          )
        )
      ), # end of cancel order div
      
      tags$br(),
      tags$div(style = "border:1px gray solid"),
      tags$br(),
      
      ##
      # Message div
      fluidRow(
        column(
          12, 
          tags$div(
            tags$h4(tags$b("Message")),
            lapply(1:opt_max_message_count, function(i){
              tags$div(style="display:block", textOutput(paste0('opt_message', i), inline = FALSE))
            })
          )
        )
      )
    ),  # end of blotter panel
    
    ##
    # Contract Details tabPanel
    tabPanel(
      "Contract Details",
      DT::dataTableOutput("opt_cd")
    ),
    
    ## 
    # History tabPanel
    tabPanel(
      "Option Chain History",
      DT::dataTableOutput("opt_yoc")
    ),
    
    tabPanel(
      "Config",
      
      fluidRow(
        column(
          12,
          tags$br(),
          shypka.ddiv(
            selectInput("opt_blotter_size_selector", tags$b("Slot Size"), choices = 1:10, selected = 1, width = blotter_field_default_width)
          ),
          shypka.ddiv(
            selectInput("opt_yoc_ndays", tags$b("How Far Away (days)"), choices = 1:90, selected = 5, width = blotter_field_default_width)
          ),
          shypka.ddiv(
            selectInput("opt_yoc_qry_lmt", tags$b("Option Query Row Limit"), choices = c(10, 50, 100, 200, 500, 1000), selected = 50, width = blotter_field_default_width)
          )
        )
      )
      
    )
  )
)