#
# handling economic indicators
#
eiAutoUpdate <- reactiveTimer(ei_refresh_time)

ei_data <- reactive({
  eiAutoUpdate()
  withProgress(message = 'Getting economic indicators data ...', {
    ei_data <- UtilGetEconIndicators(ei_fred, ei_quandl)
  })
  
})

lapply(1:length(econ_indi_tab_names), function(i){
  ei_name <- econ_indi_tab_names[i]
  output[[ei_name]] <- DT::renderDataTable({
    ei_tbl <- ei_data()[[ei_name]]
    DT::datatable(
      ei_tbl, 
      options = list(
        pageLength = 10,
        orderClasses = FALSE,
        searching = TRUE,
        paging = FALSE
      )
    ) %>%
      DT::formatStyle(
        'Unit',
        color = "gray",
        backgroundColor = DT::styleEqual(
          unique(ei_tbl$Unit),
          RColorBrewer::brewer.pal(n = 12, name = "Set3")[1:length(unique(ei_tbl$Unit))]
        )
      )
  })
})