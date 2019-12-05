# Load nontraditional data
# split up nicknames so each realname is a single variable
# Cross reference nicknames with agency and state lists
# Create a formula for unique object identifier
# Replace all sparse things in trump tweets with the identifier above, make sure to replace dems with democrats
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)

prep2MergeTweets <- function(tmp_tweets_df) {
  tmp_tweets_df <- tmp_tweets_df %>% filter(nchar(cleanTweet)>0)
  tmp_tweets_df$DateTime <- as.POSIXct(floor_date(tmp_tweets_df$tweet.createdAt, unit = "minutes"), tz=tz(Sys.time()))
  return(tmp_tweets_df)
}

# list of files we will use located in data/online_data
removeRedundantNames <- function(data_df, colnum) {
  redundant_id <- str_detect(data_df$Nickname, data_df[,colnum])
  data_df <- data_df[-which(redundant_id),]
  row.names(data_df) <- 1:nrow(data_df)
  return(data_df)
}
prepNicknames <- function(data_df){
  data_df <- subset(data_df, select=c("Nickname", "Realname", "Topic"))
  data_df <- removeRedundantNames(data_df, 2)
  data_df$Last <- apply(array(data_df$Realname), 1, function(x) strsplit(x, " ")[[1]][lengths(strsplit(x, " "))])
  data_df <- data_df %>% mutate(Person = ifelse(as.numeric(row.names(data_df))< which(data_df$Topic=="Organizations")[1], 1, 0))
  data_df <- data_df %>% mutate(Last = ifelse(Person==1, Last, gsub(" ", "",Realname)))
  data_df <- removeRedundantNames(data_df, 4)
  data_df <- removeDuplicateGroups(data_df)
  return(data_df)
}

removeDuplicateGroups <- function(data_df){
  dups <- which(duplicated(data_df$Nickname))
  dup_vec <-vector()
  solved_vec <- vector()
  for(i in length(dups):1) {
    
    if(length(which(solved_vec == dups[i]))>0){
      
    } else {
      t <-which(data_df$Nickname==data_df$Nickname[dups[i]])
      
      dup_vec <-vector()
      for(j in 1:length(t)){
        
        dup_vec <-c(dup_vec, data_df$Last[t[j]])
      }
      
      data_df$Last[t[1]] <- paste(dup_vec, collapse = ' ')
      data_df$Realname[t[1]] <- data_df$Last[t[1]] 
      
      t <- t[-1]
      solved_vec <- c(solved_vec, t)
    }
  }
  data_df <- data_df[!duplicated(data_df$Nickname), ]
  
  return(data_df)
}

prepHeadsOfState <- function(data_df){
  data_df <- data_df[-which(str_detect(data_df$Head.of.State.Name, "Trump")),]
  data_df <- data_df[!duplicated(data_df$Country), ]
  data_df <- data_df %>% mutate(Leader = ifelse(nchar(Head.of.Government.Name)>0, Head.of.Government.Name, Head.of.State.Name))
  data_df$LeaderLast <- apply(array(data_df$Leader), 1, function(x) strsplit(x, " ")[[1]][lengths(strsplit(x, " "))])
  data_df <- data_df %>% mutate(Replacement = gsub(" ", "",Country))
  data_df <- subset(data_df, select = c("Country", "Leader", "LeaderLast", "Replacement"))
  return(data_df)
}
returnNicknameHits <- function(target_vec, search_vec){
  hits <- vector()
  for(i in 1:length(search_vec)){
    hit<- which(str_detect(target_vec, search_vec[i]))
    hits <- c(hits, hit)
  }
  return(hits)
}
prepIndyAgency <- function(data_df){
  data_df$Lastname <- apply(array(data_df$Head), 1, function(x) strsplit(x, " ")[[1]][lengths(strsplit(x, " "))])
  data_df <- data_df %>% mutate(Replacement = paste0(Acronym, Lastname))
  data_df <- data_df %>% mutate(Agency.Name = gsub("The ", "",Agency.Name))
  return(data_df)
}

normalizeByDictionary <- function(target_df, dictionary_df, colser, colrep){

  for(i in 1:length(dictionary_df[,colser])){
    ids <- which(str_detect(target_df$normalizedTweet, dictionary_df[,colser][i]))
    for(j in 1:length(ids)){
      search_term <- paste0(dictionary_df[,colser][i], " ")
      replace_term <- paste0(dictionary_df[,colrep][i], " ")
      target_df$normalizedTweet[ids[j]]<-gsub(search_term, replace_term, target_df$normalizedTweet[ids[j]])
    }
  }
  return(target_df)
}

normalizeAllTweets <- function(tweets_df) {
  tweets_df <- prep2MergeTweets(tweets_df)
  tweets_df$normalizedTweet <- tweets_df$cleanTweet
  tweets_df <- normalizeByDictionary(tweets_df, nicknames_df, 1, 4)
  tweets_df <- normalizeByDictionary(tweets_df, nicknames_df, 2, 4)
  
  tweets_df <- normalizeByDictionary(tweets_df, HeadsOfState_df, 1, 4)
  tweets_df <- normalizeByDictionary(tweets_df, HeadsOfState_df, 2, 3)
  tweets_df <- normalizeByDictionary(tweets_df, HeadsOfState_df, 3, 4)
  
  tweets_df <- normalizeByDictionary(tweets_df, indyAgency_df, 3, 4)
  tweets_df <- normalizeByDictionary(tweets_df, indyAgency_df, 1, 2)
  tweets_df <- normalizeByDictionary(tweets_df, indyAgency_df, 4, 2)
  tweets_df <- fixtheFed(tweets_df)
  return(tweets_df)
}
fixtheFed <- function(target_df){
  target <-"the fed "
  replace <-"frb "
  ids <- which(str_detect(tolower(target_df$normalizedTweet), target))
  for(j in 1:length(ids)){
    target_df$normalizedTweet[ids[j]]<-gsub(target, replace, tolower(target_df$normalizedTweet[ids[j]]))
  }
  return(target_df)
}

datasources = list.files(path = "../data/online_data", pattern = "*.csv", full.names = T)
for(i in 1:length(datasources)){
  df_name <- gsub(".*/","",datasources[i])
  df_name <- gsub(".csv","_df",df_name)
  eval_string <- paste0(df_name, "<-read.csv(file='",datasources[i],"', header = TRUE, sep = ',', stringsAsFactors = FALSE)")
  eval(parse( text=eval_string ))
}

nicknames_df <- prepNicknames(nicknames_df)
HeadsOfState_df <- prepHeadsOfState(HeadsOfState_df)
indyAgency_df <- prepIndyAgency(indyAgency_df)



