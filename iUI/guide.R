##
# Guide page
guide_tp <- tabPanel(
  "Guide",
  
  fluidRow(
    column(
      12,
      
      tags$h2("Site Map"),
      br(),
      
      ##
      # Block 1
      tags$h4("Account"),
      tags$ul(
        tags$li("Holding: this tab contains the information for all holdings (securities, forex, and cash)."),
        tags$li("Balance: this tab contains the account balance."),
        tags$li("Watchlist: this tab is used to retrieve price and return history for given equity."),
        tags$li("Account Activity: this tab displays the past trades and trade messages."),
        tags$li("Downloads: this tab supports downloading certain tables from the site")
      ),
      br(), br(),
      
      ##
      # Block 2
      tags$h4("Report"),
      tags$ul(
        tags$li("Performance: this tab plots the historical return for the account")
      ),
      br(), br(),
      
      ##
      # Block 3
      tags$h4("Trade"),
      tags$ul(
        tags$li("General: all tabs in this section handles trading"),
        tags$li("How to trade:"),
        tags$li("1. Fill in Symbol (Target Currency for Forex),"),
        tags$li("2. Click Request. It will display all available contracts in Contracts details tab."),
        tags$li("3. Fill in all remaining fields based on contract details."),
        tags$li("4. Check the transmit box. Unchecked transimit will result in trade not implementing."),
        tags$li("5. Click trade."),
        tags$li("6. After the trade, a message will be displayed to indicate the results."),
        tags$li("7. Note that for forex, the message will always say the trade is not done, even if the trade is done.")
      ),
      br(), br(),
      
      ##
      # Block 4
      tags$h4("Market"),
      tags$ul(
        tags$li("Market trend: this tab shows the performance for certain ETFs that represent the market."),
        tags$li("Economic indicators: this tab shows the values for economic indicators.")
      )
    )
  )
  
)