##
# Utility functions for shiny trading portal
##

#
# Get portfolio data
# 
UtilGetPortfolio <- function(){
  ts_tmp <- IBTradingSession$new(11, platform, acct)
  ts_tmp$
    TSSetTransmit(FALSE)$                       #Prevert trade from actually happening
    TSUpdateAccountDetail()$
    TSCloseTradingSession()
  
  return(list(update_datetime = Sys.time(),
              holdings_nonforex = ts_tmp$ts_port_holdings_nonforex %>% dplyr::mutate(`Unrealized Change%` = (`Market Price` - Cost)/abs(Cost)),
              holdings_forex = ts_tmp$ts_port_holdings_forex %>% dplyr::mutate(`Unrealized Change%` = (`Market Price` - Cost)/abs(Cost)),
              port_into = ts_tmp$ts_port_info,
              cash_balance = ts_tmp$ts_cash_balance,
              ts_acc_recon = ts_tmp$ts_acc_recon %>% dplyr::select(-`Market Datetime`)))
}

#
# Find current holding
#
UtilFindCurrentHolding <- function(ticker, curr, sec_type){
  port <- UtilGetPortfolio()$holdings_nonforex
  if(nrow(port) == 0){
    pos <- 0
  } else {
    holding <- port %>% dplyr::filter(Symbol == ticker & Currency == curr & `Security Type` == sec_type)
    
    if(nrow(holding) == 0){
      pos <- 0
    } else {
      pos <- holding[,"Position"]
    }
  }
  return(pos)
}

#
# Find current holding
#
UtilFindCashBalance <- function(currency){
  port <- UtilGetPortfolio()$cash_balance
  if(nrow(port) == 0){
    pos <- 0
  } else {
    cash <- port %>% dplyr::filter(Currency == currency)
    
    if(nrow(cash) == 0){
      pos <- 0
    } else {
      pos <- cash[,"Balance"]
    }
  }
  return(pos)
}

#
# Retrieve contract details
#
UtilGetContractDetails <- function(sym, cur = "", sec_type){
  ts_tmp <- IBTradingSession$new(10, platform, acct)
  res <- ts_tmp$TSGetContractDetails(sym, cur, sec_type)
  ts_tmp$TSCloseTradingSession()
  return(res)
}

#
# Trade equity functions
#
UtilTradeWithIB <- function(blotter){
  for(i in 1:nrow(blotter)){
    ticker <- blotter[i,"Symbol"]
    curr <- blotter[i,"Currency"]
    sec_type <- blotter[i,"Security Type"]
    side <- blotter[i,"Action"]
    order_type <- blotter[,"OrderType"]
    trade_shares <- blotter[i,"Quantity"]
    transmit <- blotter[i,"TradeSwitch"]
    
    if(sec_type == "FOREX"){
      transmit <- blotter[i,"TradeSwitch"]
      tgt_curr <- blotter[,"Symbol"]
      tgt_value <- blotter[,"Quantity"]
      curr_balance <- UtilFindCashBalance(tgt_curr)
      
      expected_balance <- curr_balance + tgt_value
      
      # ts_static <<- TradingSession(22, platform, acct)
      ts_static$
        TSSetTransmit(transmit)$
        TSSetPrelimTradeList(blotter[i,])$
        TSGenFnlTradeList()
      
      ts_static$
        TSExecuteAllTrades()
      
      curr_trd_id <- ts_static$ts_trade_ids[length(ts_static$ts_trade_ids)]
      actual_balance <- UtilFindCashBalance(tgt_curr)
      
      if(actual_balance >= expected_balance) {
        res <- "Successful"
        flag <- 1
      } else {
        res <- "Failed"
        flag <- 0
      }
    } else {
      
      #
      # Check the current position
      #
      curr_holding <- UtilFindCurrentHolding(ticker, curr, sec_type)
      if(side == "Buy"){
        expected_after_holding <- curr_holding + trade_shares
      } else {
        expected_after_holding <- curr_holding - trade_shares
      }
      
      #
      # Trade
      #
      # ts_static <<- TradingSession(22, platform, acct)
      ts_static$
        TSSetTransmit(transmit)$
        TSSetPrelimTradeList(blotter[i,])$
        TSGenFnlTradeList()
      
      ts_static$TSExecuteAllTrades()
      
      curr_trd_id <- ts_static$ts_trade_ids[length(ts_static$ts_trade_ids)]
      print(curr_trd_id)
      err_msg <- ts_static$ts_last_trade_message[length(ts_static$ts_trade_ids)]
      
      #
      # Run a loop to check if the trade is sucessful
      #
      flag <- 0
      while(i <= trade_time_limit){
        actual_after_holding <- UtilFindCurrentHolding(ticker, curr, sec_type)
        ifelse(actual_after_holding == expected_after_holding, flag <- 1, flag <- 0)
        
        if(flag == 1){
          break
        } else {
          i <- i + 1
          Sys.sleep(1)
        }
      }
    }
    
    #
    # Output results
    #
    trade_res <- blotter
    trade_date <- format(Sys.Date(), "%Y-%m-%d")
    trade_time <- format(Sys.time(), "%H:%M:%S")
    if(flag == 1){
      trade_res$Date <- trade_date
      trade_res$Time <- trade_time
      trade_res$Result <- "Success"
      trade_res$TradeID <- curr_trd_id
      trade_res$TradeMode <- ts_static$ts_account_type
      trade_res$ApplicationStatus <- ts_static$ts_app_status
      trade_res <- trade_res[,c(ncol(trade_res),1:(ncol(trade_res)-1))]
      
      msg <- data.frame(Date = trade_date,
                        Time = trade_time,
                        TradeMode = acct,
                        ApplicationStatus = ts_static$ts_app_status,
                        Msg = paste0(sec_type, " ", order_type, " trade (",curr_trd_id, ") ", ticker, " - ", curr," is successfully traded (", side, ") at ",
                                     trade_date, " ", trade_time),
                        stringsAsFactors = FALSE)
    } else {
      if(curr_trd_id != -1){
        active_trade_ids <<- c(active_trade_ids, curr_trd_id)   # Update background active trades
      }
      trade_res$Date <- trade_date
      trade_res$Time <- trade_time
      trade_res$Result <- "Failed"
      trade_res$TradeID <- curr_trd_id
      trade_res$TradeMode <- ts_static$ts_account_type
      trade_res$ApplicationStatus <- ts_static$ts_app_status
      trade_res <- trade_res[,c(ncol(trade_res),1:(ncol(trade_res)-1))]
      
      msg <- data.frame(Date = trade_date,
                        Time = trade_time,
                        TradeMode = acct,
                        ApplicationStatus = ts_static$ts_app_status,
                        Msg = paste0(sec_type, " ", order_type, " trade (",curr_trd_id, ") ", ticker, " - ", curr," is not successfully traded (", side, ") at ",
                                     trade_date, " ", trade_time),
                        stringsAsFactors = FALSE)
    }
    
  }
  return(list(trade_rec = trade_res, msg_rec = msg))
}

#
# Cancel all trades
#
UtilCancelAllTrades <- function(){
  # Cancel all trades
  ts_static$TSCancelAllTrades()
  active_trade_ids <<- c()
  
  # Re-open ts Static
  # TSCloseTradingSession(ts_static)
  # ts_static <<- TradingSession(22, platform, acct)
}

#
# Download etf historical price and calculate return
#
UtilGetMarketReturn <- function(watchlist){
  #
  # Setup
  #
  start.date <- as.Date("2013-01-01") 
  end.date <- Sys.Date()
  ei.etf.keys <- paste("$", watchlist$Symbol, sep="")
  
  #
  # Retrieve quotes
  #
  fshd1 <- FinancialSecurityHistoricalData(id = 1, hist_startdate = start.date,
                                           hist_enddate = end.date)
  fshd1 <- FSHDSetWatchlist(fshd1, watchlist)
  fshd1 <- FSHDObtainAllHistPrcs(fshd1)
  ei.etf <- fshd1$FSHD_hist_cumret
  colnames(ei.etf) <- paste0(watchlist$Comments, " (", watchlist$Symbol, ")")
  
  return(ei.etf)
}

UtilGetStockHistPrcAndRet <- function(ticker_w_crncy){
  #
  # Setup
  #
  start.date <- as.Date("2013-01-01")
  end.date <- Sys.Date()
  
  pos <- regexpr("-", ticker_w_crncy)[1]
  if(pos == -1){
    ticker <- ticker_w_crncy
    currency <- "USD"
  } else {
    ticker <- substr(ticker_w_crncy, 1, pos-1)
    currency <- substr(ticker_w_crncy, pos+1, nchar(ticker_w_crncy))
  }
  
  watchlist <- data.frame(Symbol = ticker,
                          Currency = currency,
                          `Security Type` = 'STK',
                          Comments = 'None',
                          stringsAsFactors = FALSE,
                          check.names = FALSE)
  
  #
  # Retrieve quotes
  #
  fshd1 <- FinancialSecurityHistoricalData(id = 1, hist_startdate = start.date,
                                           hist_enddate = end.date)
  fshd1 <- FSHDSetWatchlist(fshd1, watchlist)
  fshd1 <- FSHDObtainAllHistPrcs(fshd1)
  
  prc <- fshd1$FSHD_hist_prc
  colnames(prc) <- ticker_w_crncy
  
  cumret <- fshd1$FSHD_hist_cumret
  colnames(cumret) <- ticker_w_crncy
  
  return(list(
    prc = prc,
    cumret = cumret
  ))
}

#
# Plot etf return data
#
UtilPlotMarketPrice <- function(master_plot_data, market, period){
  
  if(market == "Equity"){
    plot_data_prelim <- master_plot_data[,1:3]
  } else if (market == "Tbond"){
    plot_data_prelim <- master_plot_data[,4:7]
  } else if (market == "Cbond"){
    plot_data_prelim <- master_plot_data[,8:10]
  } else {
    # do nothing
    plot_data_prelim <- master_plot_data
  }
  
  # Filter based on period
  if(period == "5D"){
    offset <- 5
  } else if(period == "1M"){
    offset <- 252/12
  } else if(period == "3M"){
    offset <- 252/12 * 3
  } else if(period == "6M"){
    offset <- 252/12 * 6
  } else if(period == "1Y"){
    offset <- 252
  } else if(period == "3Y"){
    offset <- 252*3
  } else if(period == "5Y"){
    offset <- 252*5
  } else if(period == "YTD"){
    offset <- 252
  } else {
    offset <- 0
  }
  
  base <- plot_data_prelim[(nrow(plot_data_prelim)-offset+1):nrow(plot_data_prelim),]
  
  # Transform data to dataframe
  plot_data_prelim_df <- data.frame(Period = index(base),
                                    Value = base,
                                    stringsAsFactors = FALSE)
  colnames(plot_data_prelim_df) <- c("Period",colnames(plot_data_prelim))
  plot_data_final <- tidyr::gather(plot_data_prelim_df, Security, CumRet, -Period)
  
  YearMonthDay <- function(x) format(x, "%Y-%m-%d")
  my_plot <- ggplot(plot_data_final, aes(x = Period, y = CumRet, color = Security)) +
    geom_point() + 
    geom_line() +
    #scale_x_date(date_breaks = "1 day", labels = YearMonthDay) +
    ggtitle(paste0("Price History for ", market, " Market")) +
    labs(caption = paste0("Plot produced on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))) +
    theme_pka()
  
  return(my_plot)
}

#
# Plot etf return data
#
UtilPlotMarketReturn <- function(master_plot_data, market, period){
  
  if(market == "Equity"){
    plot_data_prelim <- master_plot_data[,1:3]
  } else if (market == "Tbond"){
    plot_data_prelim <- master_plot_data[,4:6]
  } else if (market == "Cbond"){
    plot_data_prelim <- master_plot_data[,7:9]
  } else {
    # do nothing
    plot_data_prelim <- master_plot_data
  }
  
  # Filter based on period
  if(period == "5D"){
    offset <- 5
  } else if(period == "1M"){
    offset <- 252/12
  } else if(period == "3M"){
    offset <- 252/12 * 3
  } else if(period == "6M"){
    offset <- 252/12 * 6
  } else if(period == "1Y"){
    offset <- 252
  } else if(period == "3Y"){
    offset <- 252*3
  } else if(period == "5Y"){
    offset <- 252*5
  } else if(period == "YTD"){
    offset <- 252
  } else {
    offset <- 0
  }
  
  base <- plot_data_prelim[nrow(plot_data_prelim)-offset-1,]
  fac <- 1/as.vector((1+base))
  last_rec <- nrow(plot_data_prelim)
  plot_data_prelim <- plot_data_prelim[(last_rec-offset):last_rec,]
  plot_data_prelim_mtx <- (1+plot_data_prelim) %*% diag(fac, nrow = length(fac)) - 1
  
  # Transform data to dataframe
  plot_data_prelim_df <- data.frame(Period = index(plot_data_prelim),
                                    Value = plot_data_prelim_mtx,
                                    stringsAsFactors = FALSE)
  colnames(plot_data_prelim_df) <- c("Period",colnames(plot_data_prelim))
  plot_data_final <- tidyr::gather(plot_data_prelim_df, Security, CumRet, -Period)
  
  YearMonthDay <- function(x) format(x, "%Y-%m-%d")
  my_plot <- ggplot(plot_data_final, aes(x = Period, y = CumRet, color = Security)) +
    geom_point() + 
    geom_line() +
    #scale_x_date(date_breaks = "1 day", labels = YearMonthDay) +
    ggtitle(paste0("Cumulative Return for ", market, " Market")) +
    labs(caption = paste0("Plot produced on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))) +
    theme_pka()
  
  return(my_plot)
}

#
# Get lastest quote
#
UtilGetStockLastestPrice <- function(ticker_w_crncy){
  
  pos <- regexpr("-", ticker_w_crncy)[1]
  if(pos == -1){
    ticker <- ticker_w_crncy
  } else {
    ticker <- substr(ticker_w_crncy, 1, pos-1)
    currency <- substr(ticker_w_crncy, pos+1, nchar(ticker_w_crncy))
    if(currency == "CAD"){
      ticker <- paste0(ticker,".TO")
    } 
  }
  
  lprc_prelim <- getSymbols(ticker, auto.assign = FALSE)
  lprc_final <- lprc_prelim[nrow(lprc_prelim),1:5]
  colnames(lprc_final) <- c("Open", "High", "Low", "Close", "Volume")
  
  return(lprc_final)
}

#
# Economic Indicators functions
#
UtilGetEconIndicators <- function(ei_fred, ei_quandl){
  
  #
  # Setup run parameters
  #
  end.date <- Sys.Date() - days(day(Sys.Date())) + 1
  mth.seq <- rev(seq(from = end.date, length = 12, by="-1 month")) 
  start.date <- mth.seq[1]
  
  ###########################################################################
  ############################# Monthly Data ################################
  ###########################################################################
  ei1 <- EconomicIndicators(id = 1, fred_items = ei_fred, quandl_items = ei_quandl,
                            hist_startdate = start.date, hist_enddate = end.date)
  # 
  # Download economic items
  ei1 <- EIDownloadAllFredItems(ei1)
  ei1 <- EIDownloadAllQuandlItems(ei1)
  
  # 
  # Data aggregation for monthly data
  #
  ei.mthly <- merge.xts(ei1$EI_fred_data,
                        ei1$EI_quandl_data,
                        all= TRUE)
  
  #
  # Format monthly data
  #
  res <- lapply(1:length(mth.seq), function(j, mth.seq){
    
    bom <- mth.seq[j]
    yr <- year(bom)
    mh <- month(bom)
    
    if(mh == 12){
      eom <- as.Date(paste(yr+1, "-", "01", "-01", sep="")) - 1
    } else {
      eom <- as.Date(paste(yr, "-", mh+1, "-01", sep="")) - 1
    }
    prd <- paste(bom, eom, sep="/")
    
    sub.data <- ei.mthly[prd]
    month.mean <- colMeans(sub.data, na.rm = TRUE)
    month.mean <- sapply(month.mean, function(x){ format(round(x, 2), nsmall=2, big.mark=",") })
    res <- as.data.frame(month.mean)
    colnames(res) <- names(res)
    
    return(res)
  }, mth.seq)
  pd.mthly.output <- dplyr::bind_cols(res)
  pd.mthly.output[is.na(pd.mthly.output)] <- ""
  colnames(pd.mthly.output) <- format(mth.seq, "%b %Y")
  pd.mthly.output$Key <- c(names(ei_fred), names(ei_quandl))
  
  #
  # Merge Data
  #
  gei_data <- merge.data.frame(gei_lookup, pd.mthly.output, by="Key")[,-(1:4)]
  lei_data <- merge.data.frame(lei_lookup, pd.mthly.output, by="Key")[,-(1:4)]
  coi_data <- merge.data.frame(coi_lookup, pd.mthly.output, by="Key")[,-(1:4)]
  lai_data <- merge.data.frame(lai_lookup, pd.mthly.output, by="Key")[,-(1:4)]
  
  #
  # Return data
  #
  return(list(gei_dt = gei_data,
              lei_dt = lei_data,
              coi_dt = coi_data,
              lai_dt = lai_data))
  
}

##
# New - 20220809
##
UtilGetPortfPerfor <- function(){
  
  ##
  # raw get data from DB
  #
  qry_str <- "SELECT NewMarketDate, `CAD Balance` FROM WebappAdmin.100_020_AccountReconciliationHistory where `security type` = 'NetLiquidation' and currency = 'CAD'"
  dataset <- GetQueryResFromSS(db_obj, qry_str)
  prcs_df <- dataset %>% 
    dplyr::mutate(MarketDate = as.Date(NewMarketDate), CADBalance = `CAD Balance`) %>% 
    dplyr::select(MarketDate, CADBalance)
  
  ##
  # contribution and distribution adjustment
  #
  trans <- ReadDataFromSS(db_obj, "MyBroKe_FundTransferHistory") %>% 
    dplyr::filter(Active == 1) %>% 
    dplyr::mutate(MarketDate = as.Date(datadate)) %>% 
    dplyr::select(MarketDate, Method, Amount) 
  
  full_trans <- data.frame(
    MarketDate = seq(min(prcs_df$MarketDate), max(prcs_df$MarketDate), by = 'day')
  ) %>% 
    dplyr::left_join(prcs_df, by = c('MarketDate')) %>% 
    tidyr::fill(CADBalance, .direction = "down")

  ##
  # date manipulation
  mkt_dates <- prcs_df$MarketDate
  ldate <- max(mkt_dates)
  fdate <- min(mkt_dates)
  
  wk_beg <- lubridate::floor_date(ldate, unit = "week")
  mth_beg <- lubridate::floor_date(ldate, unit = "month")
  qtr_beg <- lubridate::floor_date(ldate, unit = "quarter")
  hyr_beg <- lubridate::floor_date(ldate, unit = "halfyear")
  yr_beg <- lubridate::floor_date(ldate, unit = "year")
  yrfn_beg <- ldate - 365
  
  wk_beg2 <- mkt_dates[max(which(mkt_dates <= wk_beg))]
  mth_beg2 <- mkt_dates[min(which(mkt_dates >= mth_beg))]
  qtr_beg2 <- mkt_dates[min(which(mkt_dates >= qtr_beg))]
  hyr_beg2 <- mkt_dates[min(which(mkt_dates >= hyr_beg))]
  yr_beg2 <- mkt_dates[min(which(mkt_dates >= yr_beg))]
  yrfn_beg2 <- mkt_dates[min(which(mkt_dates >= yrfn_beg))]
  
  ##
  # table output
  twrr_wk <- retcalchelper_TWRR(full_trans, trans, wk_beg2, ldate)
  twrr_mth <- retcalchelper_TWRR(full_trans, trans, mth_beg2, ldate)
  twrr_qtr <- retcalchelper_TWRR(full_trans, trans, qtr_beg2, ldate)
  twrr_hyr <- retcalchelper_TWRR(full_trans, trans, hyr_beg2, ldate)
  twrr_yr <- retcalchelper_TWRR(full_trans, trans, yr_beg2, ldate)
  twrr_yrfn <- retcalchelper_TWRR(full_trans, trans, yrfn_beg2, ldate)
  twrr_all <- retcalchelper_TWRR(full_trans, trans, fdate, ldate)
  
  mwrr_wk <- retcalchelper_MWRR(full_trans, trans, wk_beg2, ldate)
  mwrr_mth <- retcalchelper_MWRR(full_trans, trans, mth_beg2, ldate)
  mwrr_qtr <- retcalchelper_MWRR(full_trans, trans, qtr_beg2, ldate)
  mwrr_hyr <- retcalchelper_MWRR(full_trans, trans, hyr_beg2, ldate)
  mwrr_yr <- retcalchelper_MWRR(full_trans, trans, yr_beg2, ldate)
  mwrr_yrfn <- retcalchelper_MWRR(full_trans, trans, yrfn_beg2, ldate)
  mwrr_all <- retcalchelper_MWRR(full_trans, trans, fdate, ldate)
  
  rets_tbl <- data.frame(
    `Time Fame` = c("Week2Date", "Month2Date", "Quarter2Date", "HalfYear2Date", "Year2Date", "1 Year From Now", "Since Inception"),
    `Start Date` = c(wk_beg2, mth_beg2, qtr_beg2, hyr_beg2, yr_beg2, yrfn_beg2, fdate),
    `End Date` = rep(ldate, 7),
    `TWRR` = c(twrr_wk, twrr_mth, twrr_qtr, twrr_hyr, twrr_yr, twrr_yrfn, twrr_all),
    `MWRR` = c(mwrr_wk, mwrr_mth, mwrr_qtr, mwrr_hyr, mwrr_yr, mwrr_yrfn, mwrr_all)
  )

  ##
  # graph output
  yr_cret <- retcalchelper_avhist(full_trans, trans, yr_beg, ldate)
  yrfn_cret <- retcalchelper_avhist(full_trans, trans, yrfn_beg, ldate)
  sinc_cret <- retcalchelper_avhist(full_trans, trans, fdate, ldate)
  
  results <- list(
    update_datetime = Sys.time(),
    table = rets_tbl,
    graph = list(
      ytd = yr_cret,
      yfn = yrfn_cret,
      sinc = sinc_cret
    )
  )
  
  return(results)
}

##
# New - 20220809
##
UtilGetPortfPerfor_OA <- function(){
  
  ##
  # load the dataset
  #
  trans <- ReadDataFromSS(db_obj, "MyBroKe_FundTransferHistoryOA") %>% 
    dplyr::filter(Active == 1) %>% 
    dplyr::mutate(MarketDate = as.Date(datadate)) %>% 
    dplyr::mutate(RevAmount = ifelse(Method == 'Account Value' & Period == 'End', -Amount, Amount)) 
  
  uniq_acts <- unique(trans$Account)
  tmp <- lapply(1:length(uniq_acts), function(i){
    
    curr_acct <- uniq_acts[i]
    curr_trans <- trans %>% 
      dplyr::filter(Account == curr_acct) %>% 
      dplyr::arrange(MarketDate)
    
    avdf <- curr_trans %>% 
      dplyr::filter(Method == 'Account Value' & Period == 'End')
    av <- mean(avdf$Amount)
    mwrr <- xirr(curr_trans$RevAmount, curr_trans$MarketDate)
    
    rets_tbl <- data.frame(
      `Account` = c(curr_acct),
      `Start Date` = c(min(curr_trans$MarketDate)),
      `End Date` = c(max(curr_trans$MarketDate)),
      `Account Value` = av,
      `Profit` = av - av/(1 + mwrr),
      `TWRR` = c(mwrr),
      `MWRR` = c(mwrr)
    )
    
    return(rets_tbl)
  })
  results <- dplyr::bind_rows(tmp)
  
  return(results)
}

#
# Add current holdings to DB
# 2020/09/30
#
UtilPostCurrHoldings <- function(porfobj, dbobj){
  tmp <- porfobj
  
  # get info
  hd <- tmp$holdings_nonforex
  cb <- tmp$cash_balance
  
  # get exchange rate
  er <- cb %>% 
    dplyr::select(Currency, `Exchange Rate`)
  
  # get portf holding first
  portf <- hd %>% 
    dplyr::select(`Market Date`, `Symbol`, `Security Type`, Currency, `Market Value`) %>% 
    dplyr::left_join(er, by = "Currency") %>% 
    dplyr::mutate(
      `CAD Market Value` = `Market Value` * `Exchange Rate`,
      `Trade Mode` = acct,
      `Application Status` = ts_static$ts_app_status,
      Active = 1
    )
  
  # remove duplicate data
  sql_str <- paste0("UPDATE MyBroKe_PortfolioHoldings SET Active = 0 WHERE `Trade Mode` = '", acct, "' and `Application Status` = '", ts_static$ts_app_status, "'")
  print(sql_str)
  GetQueryResFromSS(dbobj, sql_str)
  
  # add data
  WriteDataToSS(dbobj, portf, "MyBroKe_PortfolioHoldings", apd = TRUE)
  
  return(portf)
}

#
#
#
#
UtilGertPortfExpo <- function(dbobj){
  
  ##
  # Get portfolio holding
  portfh <- ReadDataFromSS(db_obj, "MyBroKe_PortfolioHoldings")
  
  ##
  # Asset sector
  tmp <- ReadDataFromSS(db_obj, "MyBroKe_AssetSector")
  sector <- tmp %>% 
    dplyr::filter(`Active` == 1) %>% 
    dplyr::select(Symbol, Currency, Sector, Weight) 
  
  ##
  # Asset country
  tmp <- ReadDataFromSS(db_obj, "MyBroKe_AssetCountry")
  country <- tmp %>% 
    dplyr::filter(`Active` == 1) %>% 
    dplyr::select(Symbol, Currency, Country, Weight) 
  
  ##
  # other character
  tmp <- ReadDataFromSS(db_obj, "MyBroKe_AssetChar")
  char <- tmp %>% 
    dplyr::filter(`Active` == 1) %>% 
    dplyr::select(Symbol, Currency, Instrument, AssetCategory, AssetClass, Style)
  
  ##
  # further processing
  portfh2 <- portfh %>% 
    dplyr::filter(`Active` == 1 & `Trade.Mode` == acct & `Application.Status` == ts_static$ts_app_status) %>% 
    dplyr::select(Symbol, Currency, `Security.Type`, `CAD.Market.Value`) %>% 
    dplyr::left_join(char, by = c("Symbol", "Currency")) %>% 
    dplyr::mutate(Instrument = paste0(`Security.Type`, "-", Instrument))
  portfv <- sum(portfh2$`CAD.Market.Value`)
  
  ##
  # process character
  by_inst <- portfh2 %>% dplyr::group_by(Instrument) %>% dplyr::summarise(Value = sum(`CAD.Market.Value`)) %>% dplyr::mutate(Weight = Value/portfv) %>% dplyr::arrange(desc(Weight))
  by_acat <- portfh2 %>% dplyr::group_by(AssetCategory) %>% dplyr::summarise(Value = sum(`CAD.Market.Value`)) %>% dplyr::mutate(Weight = Value/portfv) %>% dplyr::arrange(desc(Weight))
  by_acla <- portfh2 %>% dplyr::group_by(AssetClass) %>% dplyr::summarise(Value = sum(`CAD.Market.Value`)) %>% dplyr::mutate(Weight = Value/portfv) %>% dplyr::arrange(desc(Weight))
  by_styl <- portfh2 %>% dplyr::group_by(Style) %>% dplyr::summarise(Value = sum(`CAD.Market.Value`)) %>% dplyr::mutate(Weight = Value/portfv) %>% dplyr::arrange(desc(Weight))
  
  ##
  # process sector
  by_sect <- portfh2 %>% 
    dplyr::filter(AssetCategory == 'Equity') %>% 
    dplyr::left_join(sector, by = c("Symbol", "Currency")) %>% 
    dplyr::mutate(
      Sector2 = ifelse(is.na(Sector), "Other", Sector),
      Weight2 = ifelse(is.na(Weight), 100, Weight)
    ) %>% 
    dplyr::mutate(`CAD Market Value2` = `CAD.Market.Value` * Weight2 / 100) %>% 
    dplyr::group_by(Sector2) %>% 
    dplyr::summarise(`Value` = sum(`CAD Market Value2`)) %>% 
    dplyr::mutate(`Weight` = `Value`/portfv) %>% 
    dplyr::arrange(desc(Weight))
  
  ##
  # process country
  by_ctry <- portfh2 %>% 
    dplyr::filter(AssetCategory == 'Equity') %>% 
    dplyr::left_join(country, by = c("Symbol", "Currency")) %>% 
    dplyr::mutate(
      Country2 = ifelse(is.na(Country), "Other", Country),
      Weight2 = ifelse(is.na(Weight), 100, Weight)
    ) %>% 
    dplyr::mutate(`CAD Market Value2` = `CAD.Market.Value` * Weight2 / 100) %>% 
    dplyr::group_by(Country2) %>% 
    dplyr::summarise(`Value` = sum(`CAD Market Value2`)) %>% 
    dplyr::mutate(`Weight` = `Value`/portfv) %>% 
    dplyr::arrange(desc(Weight))
  
  res <- list(
    by_inst = by_inst,
    by_acat = by_acat,
    by_acla = by_acla,
    by_styl = by_styl,
    by_sect = by_sect,
    by_ctry = by_ctry
  )
  return(res)
}

#
# Retrieve current asset sector
# 2020/10/01
# No longer used
#
UtilGetPortfSectorDistrib_OLD <- function(){
  # get asset sector
  tmp <- ReadDataFromSS(db_obj, "MyBroKe_AssetSector")
  asec <- tmp %>% 
    dplyr::filter(`Active` == 1) %>% 
    dplyr::select(Symbol, Currency, Sector, Weight) %>% 
    dplyr::arrange(desc(Weight))
  
  # get portfolio current assets
  tmp <- ReadDataFromSS(db_obj, "MyBroKe_PortfolioHoldings")
  sec_wgt <- tmp %>% 
    dplyr::filter(`Active` == 1 & `Security.Type` != "OPT" & `Trade.Mode` == acct & `Application.Status` == ts_static$ts_app_status) %>% 
    dplyr::select(Symbol, Currency, `CAD.Market.Value`) %>% 
    dplyr::left_join(asec, by = c("Symbol", "Currency")) %>% 
    dplyr::mutate(
      Sector2 = ifelse(is.na(Sector), "Other", Sector),
      Weight2 = ifelse(is.na(Weight), 100, Weight)
    ) %>% 
    dplyr::mutate(`CAD Market Value2` = `CAD.Market.Value` * Weight2 / 100) %>% 
    dplyr::group_by(Sector2) %>% 
    dplyr::summarise(`Value` = sum(`CAD Market Value2`)) %>% 
    dplyr::mutate(`Weight` = `Value`/sum(`Value`)) %>% 
    dplyr::arrange(desc(Weight))
  
  #WriteDataToSS(db_obj, sec_wgt, "temp", apd = TRUE)
  
  return(sec_wgt)
  
}

#
# Retrieve current asset sector
# 2020/10/01
# No longer used
#
UtilGetPortfCountryDistrib_OLD <- function(){
  # get asset sector
  tmp <- ReadDataFromSS(db_obj, "MyBroKe_AssetCountry")
  asec <- tmp %>% 
    dplyr::filter(`Active` == 1) %>% 
    dplyr::select(Symbol, Currency, Country, Weight) %>% 
    dplyr::arrange(desc(Weight))
  
  # get portfolio current assets
  tmp <- ReadDataFromSS(db_obj, "MyBroKe_PortfolioHoldings")
  ctry_wgt <- tmp %>% 
    dplyr::filter(`Active` == 1 & `Security.Type` != "OPT" & `Trade.Mode` == acct & `Application.Status` == ts_static$ts_app_status) %>% 
    dplyr::select(Symbol, Currency, `CAD.Market.Value`) %>% 
    dplyr::left_join(asec, by = c("Symbol", "Currency")) %>% 
    dplyr::mutate(
      Country2 = ifelse(is.na(Country), "Other", Country),
      Weight2 = ifelse(is.na(Weight), 100, Weight)
    ) %>% 
    dplyr::mutate(`CAD Market Value2` = `CAD.Market.Value` * Weight2 / 100) %>% 
    dplyr::group_by(Country2) %>% 
    dplyr::summarise(`Value` = sum(`CAD Market Value2`)) %>% 
    dplyr::mutate(`Weight` = `Value`/sum(`Value`)) %>% 
    dplyr::arrange(desc(Weight))
  
  #WriteDataToSS(db_obj, sec_wgt, "temp", apd = TRUE)
  
  return(ctry_wgt)
  
}



#
# Manual open & close connection
#
OpenCloseConn <- function(dirc = c("open", "close")){
  d <- match.arg(dirc)
  if(d == "open"){
    if(!ts_static$TSIsConnected()){
      ts_static <<- IBTradingSession$new(22, platform, acct)
    }
  } else {
    if(ts_static$TSIsConnected()){
      ts_static$TSCloseTradingSession()
    }
  }
}
