#
# Update section portfolio
#
autoUpdate <- reactiveTimer(refresh_time)

port_info <- reactive({
  autoUpdate()
  port_info <- UtilGetPortfolio()
  UtilPostCurrHoldings(port_info, db_obj)
  return(port_info)
})

output$last_update_time_nonforex <- renderText({
  update_datetime <- port_info()$update_datetime
  paste0("Last updated: ", format(update_datetime, "%Y-%m-%d %H:%M:%S"))
})

output$last_update_time_sector <- renderText({
  update_datetime <- port_info()$update_datetime
  paste0("Last updated: ", format(update_datetime, "%Y-%m-%d %H:%M:%S"))
})

output$last_update_time_country <- renderText({
  update_datetime <- port_info()$update_datetime
  paste0("Last updated: ", format(update_datetime, "%Y-%m-%d %H:%M:%S"))
})

output$last_update_time_forex <- renderText({
  update_datetime <- port_info()$update_datetime
  paste0("Last updated: ", format(update_datetime, "%Y-%m-%d %H:%M:%S"))
})

output$last_update_time_cashbal <- renderText({
  update_datetime <- port_info()$update_datetime
  paste0("Last updated: ", format(update_datetime, "%Y-%m-%d %H:%M:%S"))
})


output$portfolio_holding_nonforex <- DT::renderDataTable({
  
  withProgress(message = 'Getting porfolio holdings ...', {
    tbl2dis <- port_info()$holdings_nonforex
  })
  
  # add data to 
  
  DT::datatable(
    tbl2dis, 
    options = list(
      pageLength = 50,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    ) 
  ) %>% 
    DT::formatStyle(
      'Security Type',
      fontWeight = "bold",
      color = "gray",
      backgroundColor = DT::styleEqual(
        unique(tbl2dis$`Security Type`),
        brewed_colors[1:length(unique(tbl2dis$`Security Type`))]
      )
    ) %>%
    DT::formatCurrency("Position", currency = "", digits = 0) %>% 
    DT::formatCurrency(c("Cost", "Market Price", "Market Value", "Unrealized Profit", "Realized Profit"), currency = "$", digits = 2) %>% 
    DT::formatPercentage("Unrealized Change%", 2) %>% 
    DT::formatStyle(
      c("Unrealized Profit", "Unrealized Change%", "Realized Profit"),
      fontWeight = "bold",
      #color = "white",
      color = DT::styleInterval(
        0,
        c("#fa8072","#9acd32")    # salmon and yellowgreen
      )
    )
})

output$portfolio_instrument <- DT::renderDataTable({
  tbl2dis <- UtilGertPortfExpo(db_obj)
  DT::datatable(
    tbl2dis$by_inst, 
    options = list(
      pageLength = 10,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    ) 
  ) %>% 
    DT::formatCurrency(c("Value"), currency = "$", digits = 2) %>% 
    DT::formatPercentage("Weight", 2)
})

output$portfolio_assetcat <- DT::renderDataTable({
  tbl2dis <- UtilGertPortfExpo(db_obj)
  DT::datatable(
    tbl2dis$by_acat, 
    options = list(
      pageLength = 10,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    ) 
  ) %>% 
    DT::formatCurrency(c("Value"), currency = "$", digits = 2) %>% 
    DT::formatPercentage("Weight", 2)
})

output$portfolio_assetcla <- DT::renderDataTable({
  tbl2dis <- UtilGertPortfExpo(db_obj)
  DT::datatable(
    tbl2dis$by_acla, 
    options = list(
      pageLength = 10,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    ) 
  ) %>% 
    DT::formatCurrency(c("Value"), currency = "$", digits = 2) %>% 
    DT::formatPercentage("Weight", 2)
})

output$portfolio_sector <- DT::renderDataTable({
  tbl2dis <- UtilGertPortfExpo(db_obj)
  DT::datatable(
    tbl2dis$by_sect, 
    options = list(
      pageLength = 12,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    ) 
  ) %>% 
    DT::formatCurrency(c("Value"), currency = "$", digits = 2) %>% 
    DT::formatPercentage("Weight", 2)
})

output$portfolio_style <- DT::renderDataTable({
  tbl2dis <- UtilGertPortfExpo(db_obj)
  DT::datatable(
    tbl2dis$by_styl, 
    options = list(
      pageLength = 10,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    ) 
  ) %>% 
    DT::formatCurrency(c("Value"), currency = "$", digits = 2) %>% 
    DT::formatPercentage("Weight", 2)
})

output$portfolio_country <- DT::renderDataTable({
  tbl2dis <- UtilGertPortfExpo(db_obj)
  DT::datatable(
    tbl2dis$by_ctry, 
    options = list(
      pageLength = 20,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    ) 
  ) %>% 
    DT::formatCurrency(c("Value"), currency = "$", digits = 2) %>% 
    DT::formatPercentage("Weight", 2)
})

output$portfolio_holding_forex <- DT::renderDataTable({
  tbl2dis <- port_info()$holdings_forex
  DT::datatable(
    tbl2dis, 
    options = list(
      pageLength = 20,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    ) 
  ) %>% 
    DT::formatStyle(
      'Security Type',
      fontWeight = "bold",
      color = "gray",
      backgroundColor = DT::styleEqual(
        unique(tbl2dis$`Security Type`),
        brewed_colors[1:length(unique(tbl2dis$`Security Type`))]
      )
    ) %>%
    DT::formatCurrency("Position", currency = "", digits = 0) %>% 
    DT::formatCurrency(c("Cost", "Market Price"), currency = "", digits = 5) %>% 
    DT::formatCurrency(c("Market Value", "Unrealized Profit", "Realized Profit"), currency = "$", digits = 2) %>% 
    DT::formatPercentage("Unrealized Change%", 2) %>% 
    DT::formatStyle(
      c("Unrealized Profit", "Realized Profit", "Unrealized Change%"),
      fontWeight = "bold",
      #color = "white",
      color = DT::styleInterval(
        0,
        c("#fa8072","#9acd32")    # salmon and yellowgreen
      )
    )
})

output$portfolio_cash_balance <- DT::renderDataTable({
  tbl2dis <- port_info()$cash_balance
  DT::datatable(
    tbl2dis, 
    options = list(
      pageLength = 20,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    ) 
  ) %>% 
    DT::formatStyle(
      'Currency',
      fontWeight = "bold",
      color = "gray",
      backgroundColor = DT::styleEqual(
        unique(tbl2dis$Currency),
        brewed_colors[1:length(unique(tbl2dis$Currency))]
      )
    ) %>%
    DT::formatCurrency("Exchange Rate", currency = "", digits = 4) %>% 
    DT::formatCurrency(c("Balance", "CAD Balance"), currency = "$", digits = 2)
})

#
# Handle add to trade list
#
observeEvent(input$add_trade_list_submit, {
  
  selected_rows <- input$portfolio_holding_nonforex_rows_selected
  if(!is.null(selected_rows)){
    for(i in 1:length(selected_rows)){
      my_row <- selected_rows[i]
      
      ##
      # Retrieve values
      holdings <- port_info()$holdings_nonforex
      ticker <- holdings[my_row,"Symbol"]
      right <- holdings[my_row,"Right"]
      expiry <- holdings[my_row,"Expiry"]
      strike <- holdings[my_row,"Strike"]
      currency <- holdings[my_row,"Currency"]
      exchange <- holdings[my_row,"Exchange"]
      security_type <- holdings[my_row,"Security Type"]
      position <- holdings[my_row,"Position"]
      prc <- holdings[my_row,"Market Price"]
      
      if(security_type == "STK"){
        # increase blotter size
        eq_blotter_size_tracker <<- eq_blotter_size_tracker + 1
        
        output[[paste0('eq_trade_item', eq_blotter_size_tracker)]] <- renderUI({
          list(
            tags$div(class = "blotter_fields", textInput(paste0('eq_ticker',eq_blotter_size_tracker), "Symbol", value = ticker, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", selectInput(paste0('eq_exch',eq_blotter_size_tracker), "Exchange", choices = exchange, selected = exchange, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('eq_currency',eq_blotter_size_tracker), "Currency", choices = tradable_curr, selected = currency, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('eq_side',eq_blotter_size_tracker), "Side", choices = c("Buy", "Sell"), selected = "Sell", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput(paste0('eq_shares',eq_blotter_size_tracker), "Shares", value = position, min = 0, max = 1000,  width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('eq_type',eq_blotter_size_tracker), "Type", choices = c("Lmt", "Mkt"), selected = "Mkt", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput(paste0('eq_limit_price',eq_blotter_size_tracker), "Limit Price", value = prc, min = 0, max = 1000, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", textInput(paste0('eq_trade_value',eq_blotter_size_tracker), "Trade Value", value = "0", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", style = "padding-top:20px", checkboxInput(paste0('eq_transmit',eq_blotter_size_tracker), "Transmit", value = FALSE, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", style = "padding-top:20px", actionButton(class = "btn-primary", paste0('eq_reqc',eq_blotter_size_tracker), "Request", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", style = "padding-top:20px", actionButton(class = "btn-primary", paste0('eq_trade',eq_blotter_size_tracker), "Trade", width = blotter_field_default_width))
          )
        })
      } else if (security_type == "OPT"){
        # increase blotter size
        opt_blotter_size_tracker <<- opt_blotter_size_tracker + 1
        
        output[[paste0('opt_trade_item',opt_blotter_size_tracker)]] <- renderUI({
          list(
            tags$div(class = "blotter_fields", textInput(paste0('opt_ticker',opt_blotter_size_tracker), "Symbol", value = ticker, width = blotter_field_default_width, placeholder = "AAPL")),
            tags$div(class = "blotter_fields", selectInput(paste0('opt_right',opt_blotter_size_tracker), "Right", choices = c("C", "P"), selected = right, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", selectInput(paste0('opt_expiry',opt_blotter_size_tracker), "Expiry", choices = expiry, selected = expiry, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('opt_strike',opt_blotter_size_tracker), "Strike", choices = strike, selected = strike, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('opt_currency',opt_blotter_size_tracker), "Currency", choices = c("CAD","USD"), selected = currency, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('opt_side',opt_blotter_size_tracker), "Side", choices = c("Buy", "Sell"), selected = "Sell", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput(paste0('opt_shares',opt_blotter_size_tracker), "Shares", value = position, min = 0, max = 1000,  width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('opt_type',opt_blotter_size_tracker), "Type", choices = c("Lmt", "Mkt"), selected = "Mkt", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput(paste0('opt_limit_price',opt_blotter_size_tracker), "Limit Price", value = prc/100, min = 0, max = 1000,  width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput(paste0('opt_multiplier',opt_blotter_size_tracker), "Multiplier", value = 100, min = 100, max = 100,  width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", textInput(paste0('opt_trade_value',opt_blotter_size_tracker), "Trade Value", value = "0", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", style = "padding-top:20px", checkboxInput(paste0('opt_transmit',opt_blotter_size_tracker), "Transmit", value = FALSE, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", style = "padding-top:20px", actionButton(class = "btn-primary", paste0('opt_reqc',opt_blotter_size_tracker), "Request", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", style = "padding-top:20px", actionButton(class = "btn-primary", paste0('opt_trade',opt_blotter_size_tracker), "Trade", width = blotter_field_default_width))
          )
        })
      } else if (security_type == "FUT"){
        # increase blotter size
        fut_blotter_size_tracker <<- fut_blotter_size_tracker + 1
        
        output[[paste0('fut_trade_item',fut_blotter_size_tracker)]] <- renderUI({
          list(
            tags$div(class = "blotter_fields", textInput(paste0('fut_ticker',fut_blotter_size_tracker), "Symbol", value = ticker, width = blotter_field_default_width, placeholder = "AAPL")),
            tags$div(class = "blotter_fields_wide", selectInput(paste0('fut_expiry',fut_blotter_size_tracker), "Expiry", choices = expiry, selected = expiry, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('fut_currency',fut_blotter_size_tracker), "Currency", choices = currency, selected = currency, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('fut_side',fut_blotter_size_tracker), "Side", choices = c("Buy", "Sell"), selected = "Sell", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput(paste0('fut_shares',fut_blotter_size_tracker), "Shares", value = position, min = 0, max = 1000,  width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('fut_type',fut_blotter_size_tracker), "Type", choices = c("Lmt", "Mkt"), selected = "Mkt", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput(paste0('fut_limit_price',fut_blotter_size_tracker), "Limit Price", value = prc/100, min = 0, max = 1000,  width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput(paste0('fut_multiplier',fut_blotter_size_tracker), "Multiplier", value = 100, min = 100, max = 100,  width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", textInput(paste0('fut_trade_value',fut_blotter_size_tracker), "Trade Value", value = "0", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", style = "padding-top:20px", checkboxInput(paste0('fut_transmit',fut_blotter_size_tracker), "Transmit", value = FALSE, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", style = "padding-top:20px", actionButton(class = "btn-primary", paste0('fut_reqc',fut_blotter_size_tracker), "Request", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", style = "padding-top:20px", actionButton(class = "btn-primary", paste0('fut_trade',fut_blotter_size_tracker), "Trade", width = blotter_field_default_width))
          )
        })
      } else {
        print("Invalid Trade!")
      }
      
    }
  } 
})