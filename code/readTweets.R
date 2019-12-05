readTweetCsv <- function(){
  
  datasources <- c("../data/trump04042019.csv", "../data/trump06012019.csv","../data/trump08242019.csv","../data/trump09192019.csv","../data/trump11172019.csv")
  #creates a tweets list
  for(i in 1:length(datasources)){
    
    tweets_lt <-read.csv(file=datasources[i], header = TRUE, sep = ",")
    #convert to dataframe
    
    tweets_df <- as.data.frame(tweets_lt)
    
    #format created.At column separately to make it useful
    
    ##### Process the data
    #drop extra items
    tweets_df <- subset(tweets_df, select = -c(Tweet.Id, Name, Screen.Name, Media.Type, URLs, Hashtags, Mentions))
    #convert factor to character
    colnum <- which(colnames(tweets_df)=="Created.At")
    
    tweets_df <- fixTweetDate(tweets_df, colnum)
    if(exists("fulldata_df")) {
      fulldata_df <- rbind(fulldata_df, tweets_df)
    } else {
      fulldata_df <- tweets_df
    }
  }

  return(fulldata_df)
  
}

readTweetJSon <- function(){
  jsonfile <- "../data/raw/realdonaldtrump.ndjson"
  tweets_tbl <-stream_in(jsonfile, cls = c("dt", "tbl"))
  tweets_df <- as.data.frame(tweets_tbl)
  not_all_na <- function(x) {!all(is.na(x))}
  tweets_df <- tweets_df %>% select_if(not_all_na)
  tweets_df<-subset(tweets_df, select=c(text,favorite_count, retweet_count, lang,source,retweeted_status.user.name,in_reply_to_screen_name,extended_entities.media.0.additional_media_info.description,entities.urls.0.display_url, entities.hashtags.0.text,  created_at))
  return(tweets_df)
}

combineTweetSources <-function(tweet_df, fulldata_df) {
  tweets_df <- populateTweetType(tweets_df)
  tweets_df <- subset(tweets_df, select = c(text, favorite_count, retweet_count, lang, source, tweet.type,created_at))
  df_names <- names(fulldata_df)
  names(tweets_df)<- df_names
  #convert factor to character
  colnum <- which(colnames(tweets_df)=="Created.At")
  tweets_df <- fixTweetDate(tweets_df, colnum)
  fulldata_df <- rbind(fulldata_df, tweets_df)

  #keep only unique values (based on timestamp)
  fulldata_df <- fulldata_df[ order(fulldata_df$Created.At , decreasing = TRUE ),]
  #fulldata_df <- fulldata_df[order(as.Date(fulldata_df$Created.At)),]
  fulldata_df <- fulldata_df[!duplicated(fulldata_df$Created.At),]
  return(fulldata_df)
}

returnTweetOnly <- function(fulldata_df){
  #separate dataset by type Reply, tweet, retweet
  tweet_type_array <- levels(fulldata_df$Tweet.Type)
  for (i in 1:length(tweet_type_array)) {
    assign(paste0(tweet_type_array[i], "_df"), filter(fulldata_df, Tweet.Type == tweet_type_array[i]))
  }
  return(Tweet_df)
}