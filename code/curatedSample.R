library(tidyverse)
library(tidytext)
library(stringr)
library(caret)
library(tm)
library(zoo)
library(Rmisc)

source("data_processing.R")
source("identify_jumps.R")
source("getNormalizedTweets.R")


prep2MergeStocks <- function(stock_df, ticker){
  stock_df <- stock_df[ order(stock_df$DateTime , decreasing = TRUE ),]
  row.names(stock_df) <- 1:nrow(stock_df)
  return(stock_df)
}

fullJoinTime <- function(data1_df, data2_df){
  data2_df <- normalizeAllTweets(data2_df)
  joined_df <- data1_df %>% full_join(data2_df, by = "DateTime")
  joined_df <- joined_df %>% filter(as.Date(DateTime) > as.Date("2017-01-20"))

  t <- which(is.na(joined_df$Time.L.))
  joined_df <- joined_df[-t,]
  
  joined_df <- addVolRatio(joined_df)
  return(joined_df)
}
addVolRatio <- function(data_df){
  ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}
  data_df <- data_df[order(data_df$DateTime, decreasing = TRUE),]
  data_df$VolMAfuture <- ma(data_df$Volume, 5)
  data_df <- data_df[order(data_df$DateTime, decreasing = FALSE),]
  data_df$VolMAprev <- ma(data_df$Volume, 5)
  data_df <- data_df[order(data_df$DateTime, decreasing = TRUE),]
  data_df$VolRatio <- data_df$VolMAfuture/data_df$VolMAprev
  return(data_df)
}

filterOpenTimes <- function(data_df, start_time, end_time){
  market_start <- timeInMins(start_time)
  market_end <- timeInMins(end_time)
  data_df <- data_df %>% dplyr::filter(timeInMins(Time.L.) > market_start)
  data_df <- data_df %>% dplyr::filter(timeInMins(Time.L.) < market_end)
  return(data_df)
}

addConsider2df <- function(joined_df){
  joined_df <- joined_df %>% mutate(consider = ifelse(JUMP == 1 & !is.na(tweet.text), 1, 0))
  joined_df <- joined_df %>% mutate(consider = ifelse((JUMP == 1 | lag(JUMP) == 1) & !is.na(tweet.text), 1, 0))
  return(joined_df)
}

addConsiderAlt <- function(joined_df){
  joined_df <- joined_df %>% mutate(consider = ifelse(VolRatio  > 1.4 & !is.na(tweet.text) & (JUMP == 1 | lag(JUMP) == 1), 1, 0))
#  joined_df <- joined_df %>% mutate(consider = ifelse((JUMP == 1 | lag(JUMP) == 1) & !is.na(tweet.text), 1, 0))
  return(joined_df)
}


returnTweetsJump <- function(joined_df){
  joined_df <- joined_df %>% filter(consider == 1)
  joined_df <- na.omit(joined_df)
  return(joined_df)
}
returnTweetsNone <- function(joined_df){
  joined_df <- joined_df %>% filter(consider == 0)
  joined_df <- na.omit(joined_df)
  return(joined_df)
}


tweets_df <- prep2MergeTweets(merged_tweets_df)

market_df$DateTime <- as.POSIXct(paste(market_df$Date.L., market_df$Time.L.), format="%Y-%m-%d %H:%M:%S")
spy_df <- prep2MergeStocks(sp500_clean_df)
spy_joined <- fullJoinTime(spy_df, tweets_df)


spy_joined <- filterOpenTimes(spy_joined, "09:32:00", "15:58:00")
# 
# spy_tweets_of_interest <- returnTweetsJump(addConsiderAlt(spy_joined))
# spy_tweets_no_impact <- returnTweetsNone(addConsiderAlt(spy_joined))

spy_tweets_of_interest <- returnTweetsJump(addConsider2df(spy_joined))
spy_tweets_no_impact <- returnTweetsNone(addConsider2df(spy_joined))
