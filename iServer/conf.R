#
# Handl configuration
#
observeEvent(input$config_open, {
  OpenCloseConn("open")
})

observeEvent(input$config_close, {
  OpenCloseConn("close")
})