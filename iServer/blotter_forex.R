#
# handling required currency
#
observeEvent({ 
  input$tgt_curr
}, {
  tgt_c <- input$tgt_curr
  if(tgt_c != "CAD"){
    updateSelectInput(session, "req_curr", choices = "CAD")
  } else {
    updateSelectInput(session, "req_curr", choices = tradable_curr[!(tradable_curr %in% "CAD")])
  }
})

#
# handling required value
#
observeEvent({ 
  input$tgt_curr
  input$req_curr
  input$tgt_val
}, {
  tgt_c <- input$tgt_curr
  tgt_v <- input$tgt_val
  req_c <- input$req_curr
  cb <- port_info()$cash_balance
  
  if(tgt_c != "CAD"){
    exch_rate <- cb[cb$Currency == tgt_c,"Exchange Rate"]
    updateTextInput(session, "req_val", value = round(tgt_v * exch_rate,0))
  } else {
    exch_rate <- cb[cb$Currency == req_c,"Exchange Rate"]
    updateTextInput(session, "req_val", value = round(tgt_v / exch_rate,0))
  }
})

#
# Handling equity contract request
#
observeEvent(input$request_forex, {
  
  withProgress(message = 'Retrieving contract details ...', {
    res <- UtilGetContractDetails(sym = input$tgt_curr, sec_type = "forex")
  })
  
  # Render contract details
  output$forex_cd <- DT::renderDataTable({
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
        color = DT::styleEqual(
          unique(res$Currency),
          brewed_colors[1:length(unique(res$Currency))]
        )
      )
  })
  
})

#
# handling forex trade
#
observeEvent(input$trade_forex, {
  blotter <- data.frame(Symbol = input$tgt_curr,
                        Right = "",
                        Expiry = "",
                        Strike = 0,
                        Exchange = "",
                        Action = "exchange",
                        Quantity = input$tgt_val,
                        OrderType = "",
                        LimitPrice = 0,
                        `Security Type` = "FOREX",
                        Currency = input$req_curr,
                        TradeSwitch = input$forex_trade_transmit,
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
  
  output$forex_trade_msg <- renderText({
    msg$Msg
  })
  
})