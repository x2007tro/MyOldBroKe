# ts_static$TSUpdateAccountDetail()
# 
# #
# # Test account details
# #
# acc_info <- IBrokers::reqAccountUpdates(ts_static$ts_conn)
# acc1 <- acc_info[[1]]
# acc2 <- acc_info[[2]]
# 
# TSProcessAccBal <- function(acc_bal){
#   ##
#   # convert info from list to data.frame
#   res <- lapply(1:length(acc_bal), function(i, acc_bal1){
#     tmp <- acc_bal1[[i]]
#     
#     tmp_df <- data.frame(
#       Metric = names(acc_bal1)[i],
#       Value = as.numeric(tmp['value']),
#       Currency = tmp['currency'],
#       stringsAsFactors = FALSE
#     ) %>% dplyr::filter(!is.na(Value))
#     return(tmp_df)
#   }, acc_bal)
#   res1 <- dplyr::bind_rows(res)
#   return(res1)
# }
# port_info <- ProcessAccBal(acc1)
# 
# TSProcessHolding <- function(holding){
#   ##
#   # convert info from list to data.frame
#   res <- lapply(1:length(holding), function(i, holding1){
#     tmp <- holding1[[i]]
#     contract <- tmp$contract
#     pv <- tmp$portfolioValue
#     
#     tmp2 <- cbind.data.frame(
#       data.frame(t(unlist(contract)), stringsAsFactors = FALSE),
#       data.frame(t(unlist(pv)), stringsAsFactors = FALSE)
#     ) %>% dplyr::mutate(
#       multiplier = ifelse(is.na(as.numeric(multiplier)), 1, as.numeric(multiplier))
#     ) %>% dplyr::mutate(
#       position = as.numeric(position),
#       marketPrice = as.numeric(marketPrice)*multiplier,
#       marketValue = as.numeric(marketValue),
#       averageCost = as.numeric(averageCost),
#       unrealizedPNL = as.numeric(unrealizedPNL),
#       realizedPNL = as.numeric(realizedPNL)
#     )
#     
#     return(tmp2)
#   }, holding)
#   res1 <- dplyr::bind_rows(res)
#   return(res1)
# }
# holding <- ProcessHolding(acc2)
# 
# #
# # Buy forex contract
# #
# cont <- twsCurrency(
#   symbol = 'AUD',
#   currency = 'CAD'
# )
# det <- IBrokers::reqContractDetails(ts_static$ts_conn, cont)
# trd_res <- placeOrder(ts_static$ts_conn,
#                       cont,
#                       twsOrder(orderId = reqIds(ts_static$ts_conn),
#                                action = 'buy',
#                                totalQuantity = 100,
#                                orderType = "MKT",
#                                transmit = TRUE,
#                                #goodTillDate = gtd,
#                                outsideRTH = "1"))
# 
# ##
# #  Test buy equity
# ##
# stk_blotter <- data.frame(
#   Symbol = "RY",
#   Right = "",
#   Expiry = "20181221",
#   Strike = "",
#   Exchange = "",
#   Action = "Buy",
#   Quantity = 10,
#   OrderType = "Mkt",
#   LimitPrice = 20,
#   `Security Type` = "FUT",
#   Currency = "USD",
#   TradeSwitch = TRUE,
#   stringsAsFactors = FALSE,
#   check.names = FALSE
# )
# res <- UtilTradeWithIB(stk_blotter)
# 
# ##
# # Test buy forex
# ##
# forex_blotter <- data.frame(
#   LocalTicker = "CAD",
#   Right = "",
#   Expiry = "",
#   Strike = "",
#   Exchange = "",
#   Action = "",
#   Quantity = 100,
#   OrderType = "Mkt",
#   LimitPrice = 20,
#   SecurityType = "FOREX",
#   Currency = "AUD",
#   TradeSwitch = TRUE,
#   stringsAsFactors = FALSE
# )
# res <- UtilTradeWithIB(forex_blotter)
#
# # Test cancel all trades
# res <- UtilCancelAllTrades()
#
# # Test get Econ indicators
#x <- UtilGetEconIndicators(ei_fred, ei_quandl)#
#x <- UtilGetMarketReturn(watchlist)
#x <- UtilGetStockLastestPrice("AAPL-USD")
#x <- UtilGetStockHistPrcAndRet("AAPL-USD")
#UtilPlotMarketReturn(x$cumret, "AAPL-USD", "1Y")
#UtilPlotMarketPrice(x$prc, "AAPL-USD", "1Y")
#x <- UtilGetPortfPerfor_OA()
