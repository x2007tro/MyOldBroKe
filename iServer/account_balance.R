#
# handling account info
#
output$account_recon <- DT::renderDataTable({
  tbl2dis <- port_info()$ts_acc_recon
  DT::datatable(
    tbl2dis, 
    options = list(
      pageLength = 10,
      orderClasses = FALSE,
      searching = TRUE,
      paging = FALSE
    ) 
  ) %>% 
    DT::formatCurrency(c("Balance", "CAD Balance"), currency = "$", digits = 0) %>% 
    DT::formatCurrency(c("Exchange Rate"), currency = "", digits = 5) %>% 
    DT::formatStyle(
      "Security Type",
      fontWeight = "bold",
      color = "gray",
      backgroundColor = DT::styleEqual(
        unique(tbl2dis$`Security Type`),
        brewed_colors[1:length(unique(tbl2dis$`Security Type`))]
      )
    )
})

output$raw_metrics <- DT::renderDataTable({
  tbl2dis <- port_info()$port_into
  DT::datatable(
    tbl2dis, 
    options = list(
      pageLength = 10,
      orderClasses = FALSE,
      searching = TRUE,
      paging = FALSE
    ) 
  ) %>% 
    DT::formatCurrency("Value", currency = "$", digits = 0) %>% 
    DT::formatStyle(
      "Value",
      fontWeight = "bold",
      backgroundcolor = "#9acd32"
    )
})