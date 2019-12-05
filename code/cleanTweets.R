rm(list=ls())
library(ndjson)
library(dplyr)
library(tidyr)

#clear environment

source("data_processing.R")
source("readTweets.R")

##input paremeters
mega_threshhold <- 100 #seconds
cboe_trading_start_time <- "09:30:00"
cboe_trading_end_time <- "16:10:00"

#########


fulldata_df <- readTweetCsv()


tweets_df <- readTweetJSon()

fulldata_df <- combineTweetSources(tweet_df, fulldata_df)

Tweet_df <- returnTweetOnly(fulldata_df)


#identify tweets that have occurred within a 5 minute time increment
Tweet_df <- addMegaTweetsID(Tweet_df, mega_threshhold)


merged_tweets_df <- mergeMegaTweets(Tweet_df)
merged_tweets_df$cleanTweet <- cleanWeirdCharacters(merged_tweets_df$tweet.text)

