
#~ Title: XIRR Excel function simulation
#~ 
#~ Reference 1: XIRR manual - http://office.microsoft.com/en-gb/excel-help/xirr-HP005209341.aspx
#~ Reference 2: How to calculate IRR manually - http://www.s-anand.net/Calculating_IRR_manually.html
#~ 
#~ Step 1: enter zeroes (0) against dates that do not have any cash outflow or inflows.
#~ Step 1bis: calculate IRR for these cash flow values using normal IRR function.
#~ Step 1tris: or using an iteractive approach as bisection method to find the NPV zeroes.
#~ Step 2: multiply this value of IRR by 365 to get annual IRR (since, these are daily cash flows).
#~ Step 3: refine using the formula =( 1+ R / 365) ^ 365 - 1), where R is the the value obtained in Step2.
#~

# 
sppv <- function (i, n) {
  return((1 + i/100)^(-n))
}

# Net Present Value
npv <- function(x, i) {
  npv = c()
  for (k in 1:length(i)) {
    pvs = x * sppv(i[k], 1:length(x))
    npv = c(npv, sum(pvs))
  }
  return(npv)
}

# Internal rate of return for non-periodic cash flow
# Input: cashflow - vector of numeric
#           dates - vector of dates
# Output: irr - internal rate of return - range 0,1
#
xirr <- function (cashflow, dates) {
  if (length(cashflow) != length(dates)) {
    stop("length(cashflow) != length(dates)")
  }
  
  cashflow_adj <- c(cashflow[1])
  i=1
  while (i<length(cashflow)) {
    if(dates[i+1]==dates[i]){
      cashflow_adj[length(cashflow_adj)]=cashflow_adj[length(cashflow_adj)]+cashflow[i+1]
    }
    else{
      interval <- as.integer(dates[i+1] - dates[i])
      cashflow_adj <- c(cashflow_adj, rep(0, interval-1), cashflow[i+1])
    }
    i=i+1
  }
  
  # Bisection method finding the rate to zero npv
  left = -10
  right = 10
  epsilon = 1e-8
  while (abs(right-left) > 2*epsilon) {
    midpoint = (right+left)/2
    if (npv(cashflow_adj, left) * npv(cashflow_adj, midpoint) > 0) {
      left = midpoint
    } else {
      right = midpoint
    }
  }

  # Irr for daily cashflow (not in percentage format)
  irr = (right+left) / 2 / 100  
  
  ##
  # Determine annualized return
  #
  
  # Irr for daily cashflow multiplied by 365 to get yearly return
  irr1 <- irr * 365 
  # Annualized yield (return) reflecting compounding effect of daily returns
  irr1 <- (1 + irr1 / 365) ^ 365 - 1
  
  ##
  # Determine actual return
  ndays <- length(seq(from=dates[1], to=dates[length(dates)], by='day')) -1
  irr2 <- (1 + irr) ^ ndays - 1
  
  return(irr2)
}

##
# Calculate MWRR
retcalchelper_MWRR <- function(acct_bal, cf, fdate, ldate){
  
  acct_bal <- acct_bal %>% 
    dplyr::filter(MarketDate == fdate | MarketDate == ldate) %>% 
    dplyr::mutate(Cashflow = ifelse(MarketDate == ldate, -CADBalance, CADBalance)) %>% 
    dplyr::select(MarketDate, Cashflow)
  cf2 <- cf %>% 
    dplyr::rename(Cashflow = Amount) %>% 
    dplyr::select(MarketDate, Cashflow) %>% 
    dplyr::filter(MarketDate >= fdate & MarketDate <= ldate)
  
  comb_cf <- rbind.data.frame(acct_bal, cf2) %>% 
    dplyr::arrange(MarketDate)
  
  mwrr <- xirr(comb_cf$Cashflow, comb_cf$MarketDate)
  
  return(mwrr)
}

##
# Calculate TWRR
retcalchelper_TWRR <- function(acct_bal, cf, fdate, ldate){
  
  acct_bal <- acct_bal %>% 
    dplyr::rename(Cashflow1 = CADBalance) %>% 
    dplyr::filter(MarketDate >= fdate & MarketDate <= ldate)
  cf2 <- cf %>% 
    dplyr::select(MarketDate, Amount) %>% 
    dplyr::rename(Cashflow2 = Amount)
  
  # calculate TWRR
  comb_cf <- acct_bal %>% 
    dplyr::left_join(cf2, by = 'MarketDate')
  cf2_timing <- comb_cf$MarketDate[!is.na(comb_cf$Cashflow2)]
  cf2_timing <- c(cf2_timing, ldate)
  
  tmp <- 1
  for(i in 1:length(cf2_timing)){
    t <- cf2_timing[i]
    if(i == 1){
      tgt_dates <- c(fdate, t)
      subset_cf <- comb_cf %>% 
        dplyr::filter(MarketDate %in% tgt_dates)
      tgt_df <- data.frame(
        MarketDate = tgt_dates,
        Cashflow = c(subset_cf$Cashflow1[1], -subset_cf$Cashflow1[2])
      )
    } else {
      tgt_dates <- c(cf2_timing[i-1], t)
      subset_cf <- comb_cf %>% 
        dplyr::filter(MarketDate %in% tgt_dates)
      tgt_df <- data.frame(
        MarketDate = tgt_dates,
        Cashflow = c(subset_cf$Cashflow1[1] + subset_cf$Cashflow2[1], -subset_cf$Cashflow1[2])
      )
    }
    #cret <- tgt_df$Cashflow[2]/tgt_df$Cashflow[1]
    cret <- xirr(tgt_df$Cashflow, tgt_df$MarketDate) + 1
    tmp <- cret * tmp
  }
  twrr <- tmp - 1
  
  return(twrr)
}

##
# plot account value
retcalchelper_avhist <- function(acct_bal, cf, fdate, ldate){
  # manipulation
  acct_bal <- acct_bal %>% 
    dplyr::rename(MarketValue = CADBalance)
    
  cf2 <- cf %>% 
    tidyr::spread(key = Method, value = Amount) %>% 
    dplyr::mutate(MarketDate = lubridate::ceiling_date(MarketDate, unit = 'month')) %>% 
    dplyr::group_by(MarketDate) %>% 
    dplyr::summarise(Deposit = sum(Deposit, na.rm = T), Withdrawal = sum(Withdrawal, na.rm = T))
  
  # join dataset and convert to xts
  acct_bal <- acct_bal %>% 
    dplyr::left_join(cf2, by = 'MarketDate')
  acct_bal_xts <- xts::xts(acct_bal[,-1], acct_bal[,1])
  acct_bal_xts[is.na(acct_bal_xts)] <- 0
  
  # get month end data points
  eom_pts <-  xts::endpoints(acct_bal_xts, on = "month")
  rev_eom_pts <- eom_pts[-c(1, length(eom_pts))]
  rev_eom_pts <- rev_eom_pts + 1
  rev_eom_pts <- c(1, rev_eom_pts, eom_pts[length(eom_pts)])
  acct_bal_xts_mthly <- acct_bal_xts[rev_eom_pts,]
  
  # convert data to data.frame
  acct_bal_df_mthly <- data.frame(
    MarketDate = zoo::index(acct_bal_xts_mthly),
    MarketValue = acct_bal_xts_mthly$MarketValue,
    Deposit = acct_bal_xts_mthly$Deposit,
    Withdrawal = acct_bal_xts_mthly$Withdrawal
  ) %>% 
    tidyr::gather(key = 'Cashflow', value = 'Value', -MarketDate) %>% 
    dplyr::filter(MarketDate >= fdate & MarketDate <= ldate)
  
  return(acct_bal_df_mthly)
}

