#
# handling watchlist request
#
observeEvent(input$ticker_search_submit, {
  tik <- isolate(input$ticker_search)
  output$prev_day_quote <- DT::renderDataTable({
    withProgress(message = 'Getting most recent quote ...', {
      prc <- UtilGetStockLastestPrice(tik)
      opt <- as.data.frame(t(c(format(index(prc)[1], "%Y-%m-%d"), round(prc[1,],2))))
      colnames(opt) <- c("Date", colnames(prc))
    })
    
    DT::datatable(opt, options = list(dom = "t"))
  })
  
  output$hist_prc <- renderPlot({
    withProgress(message = 'Getting historical stock price ...', {
      hist_prc <- UtilGetStockHistPrcAndRet(tik)$prc
    })
    
    UtilPlotMarketPrice(hist_prc, input$ticker_search, "1Y")
  })
  
  output$hist_return <- renderPlot({
    withProgress(message = 'Getting historical stock return ...', {
      hist_ret <- UtilGetStockHistPrcAndRet(tik)$cumret
    })
    
    UtilPlotMarketReturn(hist_ret, input$ticker_search, "1Y")
  })
})

output$wl_stwl_tbl <- DT::renderDataTable({
  ReadDataFromSS(db_obj, 'MyBroKe_STWatchlist') %>% 
    dplyr::select(Symbol, ER, Description, Action, Horizon, Reason)
})