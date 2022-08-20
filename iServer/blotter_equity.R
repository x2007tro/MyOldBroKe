#
# Handling dynamic trade items 
#
observeEvent(input$eq_blotter_size_selector,{
  #
  # Clear current value
  #
  lapply(1:eq_blotter_size_tracker, function(i){
    output[[paste0('eq_trade_item',i)]] <- renderUI({
      tags$div()
    })
  })
  
  #
  # Update new value
  # 
  eq_blotter_size_tracker <<- as.numeric(input$eq_blotter_size_selector)
  lapply(1:eq_blotter_size_tracker, function(i){
    output[[paste0('eq_trade_item',i)]] <- renderUI({
      list(
        tags$div(class = "blotter_fields", textInput(paste0('eq_ticker',i), "Symbol", value = "", width = blotter_field_default_width, placeholder = "AAPL")),
        tags$div(class = "blotter_fields_wide", selectInput(paste0('eq_exch',i), "Exchange", choices = c("TSE", "NASDAQ", "NYSE"), width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", selectInput(paste0('eq_currency',i), "Currency", choices = c("CAD","USD"), width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", selectInput(paste0('eq_side',i), "Side", choices = c("Buy", "Sell"), width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", numericInput(paste0('eq_shares',i), "Shares", value = 0, min = 0, max = 1000,  width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", selectInput(paste0('eq_type',i), "Type", choices = c("Lmt", "Mkt"), width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", numericInput(paste0('eq_limit_price',i), "Limit Price", value = 1, min = 0, max = 1000,  width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", textInput(paste0('eq_trade_value',i), "Trade Value", value = "0", width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", style = "padding-top:20px", checkboxInput(paste0('eq_transmit',i), "Transmit", value = FALSE, width = blotter_field_default_width)),
        tags$div(class = "blotter_fields_wide", style = "padding-top:20px", actionButton(class = "btn-primary", paste0('eq_reqc',i), "Request", width = blotter_field_default_width)),
        tags$div(class = "blotter_fields_wide", style = "padding-top:20px", actionButton(class = "btn-primary", paste0('eq_trade',i), "Trade", width = blotter_field_default_width))
      )
    })
  })
})

#
# Automatically calculate trade_value
#
lapply(1:eq_max_blotter_size, function(i){
  observeEvent({ 
    input[[paste0('eq_shares',i)]]
    input[[paste0('eq_limit_price',i)]]
  }, {
    updateTextInput(session, paste0('eq_trade_value',i), value=input[[paste0('eq_shares',i)]]*input[[paste0('eq_limit_price',i)]])
  })
})

#
# Cancel all trades
#
observeEvent(input$eq_cancel_all_trades, {
  UtilCancelAllTrades()
  # Update active orders
  output$eq_current_active_trades <- renderText({
    res <- paste(active_trade_ids, " ,")
    res <- substr(res, 1, nchar(res)-2)
  })
})

#
# Handling equity contract request
#
lapply(1:eq_max_blotter_size, function(i){
  observeEvent(input[[paste0("eq_reqc",i)]],{
    
    withProgress(message = 'Retrieving contract details ...', {
      res <- UtilGetContractDetails(sym = input[[paste0('eq_ticker',i)]], sec_type = "equity")
    })
    
    # Render contract details
    output$eq_cd <- DT::renderDataTable({
      DT::datatable(
        res, 
        options = list(
          pageLength = 20,
          orderClasses = TRUE,
          searching = TRUE,
          paging = TRUE
        ) 
      ) %>%
        DT::formatStyle(
          c("Currency"),
          fontWeight = "bold",
          #color = "white",
          backgroundColor = DT::styleEqual(
            unique(res$Currency),
            brewed_colors[1:length(unique(res$Currency))]
          )
        ) %>% 
        DT::formatStyle(
          c("PriExchange"),
          fontWeight = "bold",
          #color = "white",
          color = DT::styleEqual(
            unique(res$PriExchange),
            brewed_colors[1:length(unique(res$PriExchange))]
          )
        )
    })
    
    # update eq trade blotter
    if(ncol(res) != 1){
      updateSelectInput(session, paste0('eq_exch',i), choices = res$PriExchange)
      updateSelectInput(session, paste0('eq_currency',i), choices = res$Currency) 
    }
    
  })
})
  
#
# Handling trade order submit
#
lapply(1:eq_max_blotter_size, function(i){
  observeEvent(input[[paste0("eq_trade",i)]],{
    
    blotter <- data.frame(Symbol = input[[paste0('eq_ticker',i)]],
                          Right = "",
                          Expiry = "",
                          Strike = "",
                          Exchange = input[[paste0('eq_exch',i)]],
                          Action = input[[paste0('eq_side',i)]],
                          Quantity = input[[paste0('eq_shares',i)]],
                          OrderType = input[[paste0('eq_type',i)]],
                          LimitPrice = input[[paste0('eq_limit_price',i)]],
                          `Security Type` = "STK",       # this should match the security name provided by IB
                          Currency = input[[paste0('eq_currency',i)]],
                          TradeSwitch = input[[paste0('eq_transmit',i)]],
                          stringsAsFactors = FALSE,
                          check.names = FALSE)
    
    withProgress(message = 'Trading in progress ...', {
      res <- UtilTradeWithIB(blotter)
      msg <- res$msg_rec
      trd <- res$trade_rec
    })
    
    ## 
    # Write message to db
    WriteDataToSS(db_obj, trd, "MyBroKe_TradeHistory", apd = TRUE)
    WriteDataToSS(db_obj, msg, "MyBroKe_TradeMessage", apd = TRUE)
    
    ifelse(eq_message_count_trader %% eq_max_message_count == 0, 
           msg_id <- eq_max_message_count,
           msg_id <- eq_message_count_trader %% eq_max_message_count)
    output[[paste0('eq_message', msg_id)]] <- renderText({
      msg$Msg
    })
    eq_message_count_trader <<- eq_message_count_trader + 1
    
    # Update active orders
    output$eq_current_active_trades <- renderText({
      res <- paste0(active_trade_ids, collapse = ", ")
      res <- paste0(" ", res)
    })
  })
})