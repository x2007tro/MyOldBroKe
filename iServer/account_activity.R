#
# Auto refresh tables
#
autoUpdate <- reactiveTimer(refresh_time)

sql_tbls <- reactive({
  autoUpdate()
  
  trade_hist_qry <- paste0("SELECT * FROM `WebappAdmin`.`MyBroKe_TradeHistory` WHERE
                           `ApplicationStatus` = '", ts_static$ts_app_status,
                           "' AND `TradeMode` = '", ts_static$ts_trade_mode,"' ORDER BY `Date` DESC")
  
  trade_msg_qry <- paste0("SELECT * FROM `WebappAdmin`.`MyBroKe_TradeMessage` WHERE
                           `ApplicationStatus` = '", ts_static$ts_app_status,
                           "' AND `TradeMode` = '", ts_static$ts_trade_mode,"' ORDER BY `Date` DESC")
  
  err_log_qry <- paste0("SELECT * FROM `WebappAdmin`.`MyBroKe_ErrorLog` WHERE
                        `ApplicationStatus` = '", ts_static$ts_app_status,
                        "' AND `TradeMode` = '", ts_static$ts_trade_mode,
                        "' AND `Type` <> 'Info' ORDER BY `Timestamp` DESC")
  
  profit_hist_qry <- paste0("SELECT * FROM `WebappAdmin`.`100_710_RealizedProfitHistory` WHERE
                           `Application Status` = '", ts_static$ts_app_status,
                           "' AND `Trade Mode` = '", ts_static$ts_trade_mode,"' ORDER BY `Market Date` DESC")
  res <- list(
    trade_hist = GetQueryResFromSS(db_obj, trade_hist_qry),
    trade_msg = GetQueryResFromSS(db_obj, trade_msg_qry),
    err_log = GetQueryResFromSS(db_obj, err_log_qry),
    rprofit = GetQueryResFromSS(db_obj, profit_hist_qry)
  )
})

#
# handling past trades
#
output$past_trades <- DT::renderDataTable({
  trades <- sql_tbls()$trade_hist
  DT::datatable(
    trades, 
    options = list(
      pageLength = 20,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    )
  )
})

#
# realized profit
#
output$real_profit <- DT::renderDataTable({
  rpft <- sql_tbls()$rprofit
  DT::datatable(
    rpft,
    options = list(
      pageLength = 20,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    )
  ) %>% 
    DT::formatStyle(
      "Security Type",
      fontWeight = "bold",
      color = "gray",
      backgroundColor = DT::styleEqual(
        unique(rpft$`Security Type`),
        brewed_colors[1:length(unique(rpft$`Security Type`))]
      )
    ) %>% 
    DT::formatStyle(
      "Currency",
      fontWeight = "bold",
      color = "gray",
      backgroundColor = DT::styleEqual(
        unique(rpft$Currency),
        brewed_colors[1:length(unique(rpft$Currency))]
      )
    ) %>% 
    DT::formatCurrency(c("Realized Profit"), currency = "$", digits = 2) %>% 
    DT::formatStyle(
      c("Realized Profit"),
      fontWeight = "bold",
      #color = "white",
      color = DT::styleInterval(
        0,
        c("#fa8072","#9acd32")    # salmon and yellowgreen
      )
    )
})

#
# Err log
#
output$err_log <- DT::renderDataTable({
  log <- sql_tbls()$err_log
  DT::datatable(
    log,
    options = list(
      pageLength = 20,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    )
  ) %>% 
    DT::formatStyle(
    "Type",
    fontWeight = "bold",
    color = "gray",
    backgroundColor = DT::styleEqual(
      unique(log$Type),
      brewed_colors[1:length(unique(log$Type))]
    )
  )
})

#
# handling past messages
#
output$past_messages <- DT::renderDataTable({
  messages <- sql_tbls()$trade_msg
  DT::datatable(
    messages, 
    options = list(
      pageLength = 20,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    )
  )
})
