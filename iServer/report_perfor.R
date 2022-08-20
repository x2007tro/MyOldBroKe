##
# Performance
##
autoUpdate <- reactiveTimer(perfor_refresh_time)

perfor_data <- reactive({
  autoUpdate()
  withProgress(message = 'Getting portfolio performance data ...', {
    dataset <- UtilGetPortfPerfor()
  })
})


perfor_data_oa <- reactive({
  autoUpdate()
  withProgress(message = 'Getting other account performance data ...', {
    dataset2 <- UtilGetPortfPerfor_OA()
  })
})

output$last_update_time_perfor_table <- renderText({
  update_datetime <- perfor_data()$update_datetime
  paste0("Last updated: ", format(update_datetime, "%Y-%m-%d %H:%M:%S"))
})

output$perfor_table <- DT::renderDataTable({
  rets <- perfor_data()$table
  DT::datatable(
    rets, 
    options = list(
      pageLength = 10,
      orderClasses = FALSE,
      searching = TRUE,
      paging = FALSE
    )
  ) %>%
    DT::formatPercentage('TWRR', 2) %>% 
    DT::formatPercentage('MWRR', 2)
})

output$perfor_table_oa <- DT::renderDataTable({
  rets <- perfor_data_oa()
  DT::datatable(
    rets, 
    options = list(
      pageLength = 10,
      orderClasses = FALSE,
      searching = TRUE,
      paging = FALSE
    )
  ) %>%
    DT::formatPercentage('TWRR', 2) %>% 
    DT::formatPercentage('MWRR', 2) %>% 
    DT::formatRound('Account.Value', digits = 0) %>% 
    DT::formatRound('Profit', digits = 0)
  
})

output$perfor_graph_ytd <- renderPlot({
  rets <- perfor_data()$graph$ytd
  ggplot(rets, aes(x = MarketDate, y = Value, fill = Cashflow)) +
    geom_bar(position="stack", stat="identity") +
    ggthemes::theme_wsj() +
    #scale_fill_manual(name = "Cashflow", values = user_colors) +
    ggtitle("Account Value", subtitle = paste0("YTD since ", format(rets$MarketDate[1], "%b %Y"))) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
    #scale_y_continuous(labels = function(x) scales::label_comma(x)) +
    theme(legend.position = "bottom")
})

output$perfor_graph_yfn <- renderPlot({
  rets <- perfor_data()$graph$yfn
  ggplot(rets, aes(x = MarketDate, y = Value, fill = Cashflow)) +
    geom_bar(position="stack", stat="identity") +
    ggthemes::theme_wsj() +
    #scale_fill_manual(name = "Cashflow", values = user_colors) +
    ggtitle("Account Value", subtitle = paste0("Year from now since ", format(rets$MarketDate[1], "%b %Y"))) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
    #scale_y_continuous(labels = function(x) scales::label_comma(x)) +
    theme(legend.position = "bottom")
})

output$perfor_graph_sinc <- renderPlot({
  rets <- perfor_data()$graph$sinc
  ggplot(rets, aes(x = MarketDate, y = Value, fill = Cashflow)) +
    geom_bar(position="stack", stat="identity") +
    ggthemes::theme_wsj() +
    #scale_fill_manual(name = "Cashflow", values = user_colors) +
    ggtitle("Account Value", subtitle = paste0("Since inception ", format(rets$MarketDate[1], "%b %Y"))) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 month") + 
    #scale_y_continuous(labels = function(x) scales::label_comma(x)) +
    theme(legend.position = "bottom")
})

observeEvent(input$ptoa_input_confirm, {
  
  withProgress(message = 'Adding current account value to DB ...', {
    # Inactivate last account end value
    my_sql <- paste0("UPDATE `MyBroKe_FundTransferHistoryOA` SET `Active`=0 WHERE `Account` = '", input$ptoa_input_account,"' and `Method` = 'Account Value' and `Period` = 'End'")
    GetQueryResFromSS(db_obj, my_sql)
    
    # Reinsert new account end value
    new_rec <- data.frame(
      datadate = input$ptoa_input_mktdate,
      Account = input$ptoa_input_account,
      Method = 'Account Value',
      Period = 'End',
      Amount = input$ptoa_input_endav,
      Active = 1,
      EntryDatetime = Sys.time(),
      stringsAsFactors = F
    )
    WriteDataToSS(db_obj, new_rec, 'MyBroKe_FundTransferHistoryOA', apd = T)
    
    # wait for refresh
  })
  
})
