library(tidyr)
library(dplyr)
library(zoo)
library(lubridate)
library(tm)
library(tidytext)
library(stringr)

#functions used for tweet processing

asEDTDate <- function(myDate){
  newDate <- as.Date(myDate, tz = Sys.timezone())
  return(newDate)
}

addMegaTweetsID <- function(data_df, threshhold){
  
  data_df <- data_df %>% mutate(time.difference = (Created.At - lead(Created.At)))
  #create a time.difference variable, create mega.tweet variable use this as unique id for mega tweets, and zero otherwise
  data_df <- data_df %>% mutate(mega.tweet = ifelse(lag(time.difference) < threshhold | time.difference <threshhold, 1, 0))
  #fix na issue
  data_df[is.na(data_df)] <- 0
  
  tweet_id <- c(1)
  for(i in 1:nrow(data_df)) {
    
    if((data_df$mega.tweet[i] == 1 & data_df$time.difference[i] > threshhold)) {
      tweet_id <- append(tweet_id, tweet_id[i-1])
    } else if(data_df$mega.tweet[i] == 1 & data_df$time.difference[i] < threshhold) {
      if ((data_df$mega.tweet[i-1] == 1 & data_df$time.difference[i-1] < threshhold) ){
        tweet_id <- append(tweet_id, tweet_id[i-1])
      } else {
        tweet_id <- append(tweet_id, tweet_id[i-1]+1)
      }
    } else {
      tweet_id <- append(tweet_id, tweet_id[i-1]+1)
    }
  }
  
  data_df$tweet.id <- tweet_id
  return(data_df)
}

mergeMegaTweets <- function(data_df) {
  ###### create new dataset with  mega tweets merged
  #initialize columns in new data set
  tweet.text <- c("")
  tweet.fav <- c(0)
  tweet.retweet <- c(0)
  tweet.createdAt <- as.POSIXct(NA)
  tweet.id <-c(0)
  tweet.mega <-c(0)
  
  #loop through the data to create new dataset
  for(i in 1:nrow(data_df)) {
    id_tweet_parts <- which(data_df$tweet.id == data_df$tweet.id[i])
    id_tweet_parts_array <- array(id_tweet_parts)
    current_fav_array <- apply(id_tweet_parts_array, 1, FUN = function(x) data_df$Favorites[x])
    current_retweet_array <- apply(id_tweet_parts_array, 1, FUN = function(x) data_df$Retweets[x])
    tot_tweet_parts <- length(id_tweet_parts)
    
    if(tot_tweet_parts >1) {
      if(length(tweet.fav)==i) {
        print(i)
        
      } else {
        tweet_array <- apply(rev(id_tweet_parts_array), 1, FUN = function(x) as.character(data_df$Text[x]))
        tweet.text[data_df$tweet.id[i]] <- paste(tweet_array, sep = "", collapse = " ")
        tweet.fav[data_df$tweet.id[i]] <- max(current_fav_array)
        tweet.retweet[data_df$tweet.id[i]] <- mean(current_retweet_array)
        tweet.createdAt[data_df$tweet.id[i]]  <- data_df$Created.At[max(id_tweet_parts_array)]
        tweet.id[data_df$tweet.id[i]]  <- data_df$tweet.id[i]
      }
      
      
    } else if (tot_tweet_parts == 1){
      tweet.text[data_df$tweet.id[i]] <- as.character(data_df$Text[i])
      tweet.fav[data_df$tweet.id[i]]  <- data_df$Favorites[i]
      tweet.retweet[data_df$tweet.id[i]]  <- data_df$Retweets[i]
      tweet.createdAt[data_df$tweet.id[i]]  <- data_df$Created.At[i]
      tweet.id[data_df$tweet.id[i]]  <- data_df$tweet.id[i]
      
    } else {
      
    }
    tweet.mega[data_df$tweet.id[i]]  <- tot_tweet_parts
    
  }
  
  new_tweets_df <- data.frame(tweet.id, tweet.mega, tweet.text, tweet.fav, tweet.retweet, tweet.createdAt)
  return(new_tweets_df)
}

fixTweetDate <- function(data_df, dt_colnum) {
  
  timeCol_ch <- as.character(data_df[,dt_colnum])
  #remove the columnt from the dataset
  data_df <- subset(data_df, select = c(-dt_colnum))
  
  #split the data using tidyr
  datetimestamp_df <- as.data.frame(timeCol_ch) %>% separate(timeCol_ch, c("day", "mo", "date", "hr","min","sec", "foo", "year"))
  #separate out the date value
  datestamp_df <- subset(datetimestamp_df, select =c("mo","date", "year"))
  #separate out the time value
  timestamp_df <- subset(datetimestamp_df, select =c("hr","min", "sec"))
  
  datestamp_ch <- apply(datestamp_df, 1, function(x) paste(x, sep="", collapse = "-"))
  datestamp_dt <- as.Date(datestamp_ch, "%b-%d-%Y")
  
  datestamp_ch <- as.character(datestamp_dt, "%Y-%m-%d")
  
  timestamp_ch <- apply(timestamp_df,1, function(x) paste(x, sep="", collapse = ":"))
  datetime_ch <- paste(datestamp_ch, timestamp_ch, sep =" ", collapse= NULL)
  
  datetimestamp <- as.POSIXct(datetime_ch, "%Y-%m-%d %H:%M:%OS" )
  
  data_df$Created.At = datetimestamp
  
  #extract time #strftime(times, format="%H:%M:%S")
  #extract day of the week strftime(df$date,'%A')
  return(data_df)
}

cleanWeirdCharacters <- function(tweet.text) {
  
  
  data_df2 <- gsub("http.*","",tweet.text)
  
  data_df2 <- gsub("https.*","",data_df2)
  
  data_df2 <- gsub("#","",data_df2)
  data_df2 <- gsub("&amp;","and",data_df2)
  
  data_df2 <- gsub("@","",data_df2)
  
  
  data_df2 <- gsub("[^[:alnum:]///' ]", "", data_df2)
  
 
  data_df2 <- gsub("^\\s+|\\s+$", "", data_df2)
  
  return(data_df2)
}

populateTweetType <-function(data_df) {
  data_df[ , "tweet.type"] <- NA
  for(i in 1:nrow(data_df)) {
  if(is.na(data_df$in_reply_to_screen_name[i])){
    if(is.na(data_df$retweeted_status.user.name[i])){
      data_df$tweet.type[i] <- "Tweet"
    } else {
      data_df$tweet.type[i] <- "Retweet"
    }
  } else {
    data_df$tweet.type[i]<-"Reply"
  }
  }
  return(data_df)
}


timeInMins <-function(dateTime) {
  myBase <- sub(".*\\s+","", dateTime)
  myHr <- as.numeric(sub(":.*", "", myBase))
  myMins <- sub("[[:digit:]]+:","", myBase)
  myMins <- as.numeric(sub(":[[:digit:]]+","", myMins))
  totMins <- 60*myHr + myMins                
  return(totMins)
}

