##
# Source server and ui components
##
require(shiny)
require(magrittr)
require(ggplot2)
source("helper/utils.R")
source("helper/retcalchelper.R")
source("helper/dbhelper.R")
source("global.R", local = FALSE)
source("iUI/main.R")
source("iServer/main.R")

##
# Launch shiny app
##
shinyApp(
  ui = mainUI,
  server = mainServer
)