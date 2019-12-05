library(dplyr)
library(tidyr)

source("readMinutelyData.R")

updateWithJumps <- function(sp500_clean_df) {
# for minutely frequency
  hoursinday = 24
  minsinhour = 60
  min_interval = 1
  nobs_day = hoursinday*minsinhour/min_interval
  K = ceiling(sqrt(252*nobs_day))
  sp500_clean_df <-na.omit(sp500_clean_df)
  sp500_clean_df$vol_hat <- NaN

  start_j = 2+1
  max_i =length(sp500_clean_df$Close.Price)-1
  max_start_j = max_i-K+2

  for(index in seq(start_j, max_start_j, 1)){
    start_i = index+K+2
    j <- seq(index, (start_i-1), 1)

    sp500_clean_df$vol_hat[index] <- sqrt(1/(K-2)*sum(abs(log(sp500_clean_df$Close.Price[j]/sp500_clean_df$Close.Price[j-1]))*abs(log(sp500_clean_df$Close.Price[j-1]/sp500_clean_df$Close.Price[j-2]))))

    }
  sp500_clean_df$vol_hat[1:2] <- sp500_clean_df$vol_hat[3]
  sp500_clean_df$vol_hat[is.na(sp500_clean_df$vol_hat)] <- sp500_clean_df$vol_hat[max_start_j-2]
  sp500_clean_df$vol_hat[is.nan(sp500_clean_df$vol_hat)] <- sp500_clean_df$vol_hat[max_start_j-2]


  sp500_clean_df <- sp500_clean_df%>%mutate(Jump_stat = abs(log(Close.Price/lag(Close.Price))/vol_hat))

  sp500_clean_df = sp500_clean_df[-1,]

  mu_no_jump = 0
  sigma_no_jump = sqrt(1/(.7979^2))
  jump_threshold = mu_no_jump +2*sigma_no_jump
  sp500_clean_df$JUMP = ifelse(sp500_clean_df$Jump_stat<jump_threshold, 0, 1)
  
  return(sp500_clean_df)

}

sp500_clean_df <- updateWithJumps(market_df)





