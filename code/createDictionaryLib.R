#https://rstudio-pubs-static.s3.amazonaws.com/132792_864e3813b0ec47cb95c7e1e2e2ad83e7.html
#https://www.hackerearth.com/practice/machine-learning/advanced-techniques/text-mining-feature-engineering-r/tutorial/
#https://cbail.github.io/SICSS_Dictionary-Based_Text_Analysis.html
#https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/
#http://www.bernhardlearns.com/2017/04/cleaning-words-with-r-stemming.html
#https://stackoverflow.com/questions/28214148/how-to-perform-lemmatization-in-r
#https://cran.r-project.org/web/packages/SentimentAnalysis/vignettes/SentimentAnalysis.html
#parts of speech tagging https://smart-statistics.com/part-speech-tagging-r/
#https://www.rdocumentation.org/packages/NMOF/versions/1.6-0/topics/gridSearch
#http://rpubs.com/jeandsantos88/search_methods_for_hyperparameter_tuning_in_r
library(tm)
library(tidytext)
library(tidyr)
library(dplyr)
library(SnowballC)
library(wordcloud)
library(koRpus)
library(textstem)
library(lubridate)
library(SentimentAnalysis)
library(cvAUC)
# part of speech tagging
library(RDRPOSTagger)
library(tokenizers)
#library(parallel)
#source("addEmotionToTraining.R")
ngram_param <- 1
impact_rows <- 0
sim_num <- 0
min_tf <-3

rm(impact_size_df) 
impact_size_df <- data.frame(matrix(ncol = 4, nrow = 3500))
x <- c("size", "date", "dictstr", "dictprob")
colnames(impact_size_df) <- x
impact_size_df$date <- as.POSIXct(impact_size_df$date)

impact_size_cnt <- 0

###############
evaluateTweetDM <- function(data_df, ngram_num){
  data_df <- na.omit(data_df)

  tweet_corpus <- VCorpus(VectorSource(data_df$normalizedTweet))

  tweet_corpus = tm_map(tweet_corpus, content_transformer(tolower))
  tweet_corpus = tm_map(tweet_corpus, removeNumbers)

  tweet_corpus = tm_map(tweet_corpus, removePunctuation)
  tweet_corpus = tm_map(tweet_corpus, removeWords, c("the", "and", "amp", stopwords("english")))
  tweet_corpus =  tm_map(tweet_corpus, stripWhitespace)


#findFreqTerms(tweet_dtm, 5)
#https://stackoverflow.com/questions/8898521/finding-2-3-word-phrases-using-r-tm-package
#BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  NgramTokenizer <-
    function(x)
    unlist(lapply(ngrams(words(x), ngram_num), paste, collapse = " "), use.names = FALSE)

  tdm <- TermDocumentMatrix(tweet_corpus, control = list(tokenize = NgramTokenizer))
  return(tdm)
}
setPOSCorpusMatrix <- function(full_model_df=vx_joined) {
  if(!exists("fullPOSCorpus")){
    data_df <- na.omit(full_model_df)
    sentences <- tokenize_sentences(data_df$normalizedTweet, simplify = TRUE)
    unipostagger <- rdr_model(language = "English", annotation = "UniversalPOS")
    out_df <- rdr_pos(unipostagger, sentences)
    assign("fullPOSCorpus", out_df, envir = .GlobalEnv)
  }
  tmp <-subset(fullPOSCorpus, select =c("token", "pos"))
  out_df<-tmp[!duplicated(tmp$token),]
  names(out_df)<-c("term", "pos")
  out_df$term <- tolower(out_df$term)
  out_df$pos <- tolower(out_df$pos)
  return(out_df)
}
calculateTermProbability <- function(mydtm, impact_id, pos_wt) {
  tidy_dtm <- tidy(mydtm)
  tidy_dtm$term <- lemmatize_words(tidy_dtm$term)
  #unclear that numwords and numdocs is needed since we should trim the data set before going any further.
  numWords <- mydtm$nrow
  numDocs <- mydtm$ncol

  if(impact_id ==0) {

    tidy_dtm_prob <- calculateDTMprobs(tidy_dtm, "prob_none", numWords, numDocs, pos_wt)
  } else {

    tidy_dtm_prob <- calculateDTMprobs(tidy_dtm, "prob_impact", numWords, numDocs, pos_wt)
    assign("impact_rows", numWords, envir = .GlobalEnv)
  }
  return(tidy_dtm_prob)
}

subsetTweetSample <- function(data_df, currentPosix, win_size){
  sampleLimit <- as.Date(currentPosix) %m-% months(win_size)
  data_df <- na.omit(data_df)
  data_df <- data_df %>% filter(as.Date(tweet.createdAt) > sampleLimit)
  data_df <- data_df %>% filter(as.Date(tweet.createdAt) < as.Date(currentPosix))
  return(data_df)
}

createProbabilityDF <- function(data_df, currentDate, impact_id, ngram_num, pos_wt, win_size) {
  data_df <-subsetTweetSample(data_df, as.Date(currentDate), win_size)
  tdm <- evaluateTweetDM(data_df,ngram_num)
  prob_df <- calculateTermProbability(tdm, impact_id, pos_wt)
  return(prob_df)
}

returnJoinedCorpus <- function(none_df, impact_df){
  joined_df <- none_df %>% full_join(impact_df, by="term")
  joined_df$score.y[is.na(joined_df$score.y)] <- joined_df$score.x[is.na(joined_df$score.y)]
  #if(is.na(joined_df$score.y)) {joined_df$score.y <-joined_df$score.x}
  #if(is.na(joined_df$score.x)) {joined_df$score.x <-joined_df$score.y}
  joined_df[is.na(joined_df)] <- 0
  names(joined_df) <- c("term", "prob_none", "x","prob_impact","score")
  joined_df <- joined_df[,-3]
  return(joined_df)
}

returnImpactDictionary <- function(joined_df, cal_wt){
  #min_prob <- cal_wt[2]/impact_rows
  #impact_dict <- joined_df %>% filter(prob_impact>cal_wt[1]*prob_none) %>% filter(prob_impact > min_prob)
  impact_dict <- joined_df %>% filter(prob_impact>cal_wt*prob_none)
  impact_dict$prob_impact <- impact_dict$prob_impact - impact_dict$prob_none

  
  return(impact_dict)
  
}

returnNoneDictionary <- function(joined_df, cal_wt){
  
  #min_prob <- cal_wt[2]/impact_rows
  #none_dict <- joined_df %>% filter(prob_none>cal_wt[1]*prob_impact) %>% filter(prob_none > min_prob)
  none_dict <- joined_df %>% filter(prob_none>cal_wt*prob_impact)
  none_dict$prob_none <- none_dict$prob_none - none_dict$prob_impact
  none_dict$score <- -1*none_dict$score

  return(none_dict)
  
}

returnMinTFNone <- function(impactdict_df, nonedict_df, min_tf){
  size_impact <- dim(impactdict_df)[1]
  size_none <- dim(nonedict_df)[1]
  none_min_tf <- min_tf*size_none/size_impact
  return(none_min_tf)
}
returnProbabilityRankDf <- function(tmp_df){
  divider <- dim(tmp_df)[1]
  tmp_df[,2] <- tmp_df[,2]/divider
  return(tmp_df)
}
returnWeightedDictionary <-function(currentdate, tweets_of_interest, tweets_no_impact, ngram_param, pos_wt,win_size, cal_wt){
  tidy_impact_prob <- createProbabilityDF(tweets_of_interest, as.Date(currentdate), 1, ngram_param, pos_wt, win_size)
  tidy_none_prob <- createProbabilityDF(tweets_no_impact, as.Date(currentdate), 0, ngram_param, pos_wt, win_size)
  #### calculate probabilities here
  impact_tf <- min_tf
  none_tf <- returnMinTFNone(tidy_impact_prob, tidy_none_prob, impact_tf)
  tidy_impact_prob <- tidy_impact_prob %>% filter(prob_impact > impact_tf)
  tidy_none_prob <- tidy_none_prob %>% filter(prob_none > none_tf)
  
  tidy_impact_prob <- returnProbabilityRankDf(tidy_impact_prob)
  tidy_none_prob <- returnProbabilityRankDf(tidy_none_prob)
  
  joined_corpus <- returnJoinedCorpus(tidy_none_prob, tidy_impact_prob)

  
  impact_corpus <- returnImpactDictionary(joined_corpus, cal_wt)
  none_corpus <- returnNoneDictionary(joined_corpus, cal_wt)
  impact_corpus <- impact_corpus %>% mutate(prob = prob_impact)
  none_corpus <- none_corpus %>% mutate(prob = prob_none)
  impact_docs <- impact_corpus$term
  none_docs <- none_corpus$term
  
  ### create a series of dates and impact lengths
  # tmp_impact_size_df <- impact_size_df
  # tmp_impact_size_cnt <- impact_size_cnt +1
  # tmp_impact_size_df$size[tmp_impact_size_cnt] <- length(impact_docs)
  # tmp_impact_size_df$date[tmp_impact_size_cnt] <- currentdate
  # tmp_impact_size_df$dictstr[tmp_impact_size_cnt] <- list(impact_docs)
  # tmp_impact_size_df$dictprob[tmp_impact_size_cnt] <- list(impact_corpus$prob)
  # 
  # assign("impact_size_df", tmp_impact_size_df, envir = .GlobalEnv)
  # assign("impact_size_cnt", tmp_impact_size_cnt, envir = .GlobalEnv)
  
  dict_docs <- c(impact_docs, none_docs)
  dict_resp <- c(impact_corpus$score, none_corpus$score)
  dict_prob <- c(impact_corpus$prob, none_corpus$prob)
  
  dict <- data.frame("terms"=dict_docs, "score"=dict_resp, "prob" = dict_prob )
  return(dict)
}

calculateTweetScore <- function(tweetText, currentdate, tweets_of_interest, tweets_no_impact, ngram_param, type_impact_none, pos_wt, win_size, cal_wt){
  dict <- returnWeightedDictionary(currentdate, tweets_of_interest, tweets_no_impact, ngram_param, pos_wt,win_size, cal_wt)
  maxScore <- 0
  if(type_impact_none=="impact"){
      dict <- dict %>% filter(score >0)

    } else {
      dict <- dict %>% filter(score <0)

    }
  
  tweet_vec <- tolower(unlist(strsplit(tweetText, split = " ")))
  tweet_df <- data.frame("terms" = tweet_vec)
  tmp <- tweet_df %>% inner_join(dict, by= "terms")

  #remove repeating terms
  if(length(tmp$score) >0 & length(dict$terms)>3) {
    total_score <- sum(tmp$score*tmp$prob)#/(length(tmp$score)*maxScore)
  } else {
    total_score <- 0
  }

  return(total_score)
}

calculateDTMprobs <- function(tidy_dtm, prob_name, wordNum, docNum, pos_wt){
  #unclear that numdocs and numwords is needed
  dtm_prob <- aggregate(tidy_dtm$count, by=list(term=tidy_dtm$term), FUN=sum)
  dtm_prob <- data.frame("term"=dtm_prob$term, "prob" = dtm_prob$x)
  names(dtm_prob) <- c("term",prob_name)
  dtm_prob <- returnCalculatedProbs(dtm_prob, wordNum, docNum, prob_name, pos_wt)

  return(dtm_prob)
}
returnCalculatedProbs <-function(tidy_dtm_prob, numWords, numDocs, prob_name, pos_wt){

  pos_vec <-c("propn", "noun", "adv", "verb", "adj")
  pos_df <- setPOSCorpusMatrix()
  pos_df <- pos_df %>% mutate(score=ifelse(pos == pos_vec[1], pos_wt[1], ifelse(pos == pos_vec[2], pos_wt[2], ifelse(pos==pos_vec[3], pos_wt[3], ifelse(pos==pos_vec[4], pos_wt[4], ifelse(pos==pos_vec[5], 0,  0))))))
  t <- which(pos_df$score==0)
  pos_df <- pos_df[-t,]
  pos_df <-pos_df[!duplicated(pos_df$term),]
  tmp_df <- tidy_dtm_prob %>% inner_join(pos_df, by="term")
  doc_ratio <- numWords/numDocs
  tmp_df<-tmp_df[,-3]

  return(tmp_df)
}
calculateImpactNoneScoresDF <- function(tweets_df, impact_df, none_df, ngram_param, pos_wt, win_size, cal_wt){
  minStartDate <- min(as.Date(tweets_df$tweet.createdAt))%m+% months(win_size)
  maxId <- max(which(as.Date(tweets_df$tweet.createdAt) > as.Date(minStartDate)))
  
  tweets_df$impactscore <-0
  tweets_df$nonescore <-0
  for(i in 1:maxId){
    tweets_df$impactscore[i] <- calculateTweetScore(tweets_df$normalizedTweet[i], tweets_df$DateTime[i], impact_df, none_df, ngram_param,  "impact", pos_wt,win_size, cal_wt)
    tweets_df$nonescore[i] <- calculateTweetScore(tweets_df$normalizedTweet[i], tweets_df$DateTime[i], impact_df, none_df, ngram_param,  "none",  pos_wt, win_size,cal_wt)
  }
  return(tweets_df)
}

calculateGuessStats <- function(target_df, interest_df, none_df, ngram_param, impact_flag, pos_wt, win_size, cal_wt){

  tmp <- returnGuessDF(target_df, interest_df, none_df, ngram_param, impact_flag, pos_wt, win_size, cal_wt)
  successRatio <- sum(tmp$predicted_correct)/length(tmp$predicted_correct) + sum(tmp$predicted_incorrect)/length(tmp$predicted_incorrect)
  return(successRatio)
}

returnGuessDF <- function(target_df, interest_df, none_df, ngram_param, impact_flag, pos_wt, win_size, cal_wt){
  tmp <- calculateImpactNoneScoresDF(target_df, interest_df, none_df, ngram_param, pos_wt, win_size, cal_wt)
  tmp$totalscore <-tmp$impactscore+tmp$nonescore
  
  
  
  if(impact_flag == 1) {
    tmp$predicted_correct <- ifelse(tmp$totalscore >0, 1, 0)
    tmp$predicted_incorrect <- ifelse(tmp$totalscore < 0, -1, 0)

  } else {
    tmp$predicted_correct <- ifelse(tmp$totalscore <0, 1, 0)
    tmp$predicted_incorrect <- ifelse(tmp$totalscore>0, -1, 0)
  }
  
  tmp$totalscore <- ifelse(tmp$totalscore >0, 1, 0)
  
  return(tmp)
}
write2File <- function(data_str){
  
  file_name <- sprintf("../output/optimization/auc_%d.csv" , Sys.getpid())
  fileConn<-file(file_name, "a")
  writeLines(data_str, fileConn)
  close(fileConn)
}
calculateDictAUC <- function(impact_df, none_df, ngram_param, pos_wt, win_size, cal_wt){

  out_str <-""
  tmp_num <- sim_num +1
  assign("sim_num", tmp_num, envir = .GlobalEnv)
  out_str <-paste0(tmp_num, ",", pos_wt[1], ",", pos_wt[2], ",", pos_wt[3], ",", pos_wt[4],",", pos_wt[5], ",0,", pos_wt[6], ",", win_size, ",", cal_wt[1], ",", cal_wt[2])
  print(out_str)
  # impact <- returnGuessDF(impact_df, impact_df, none_df, ngram_param, 1, pos_wt, win_size, cal_wt)
  # none <- returnGuessDF(none_df, impact_df, none_df, ngram_param, 0, pos_wt, win_size, cal_wt)
  # total <- rbind(impact, none)
  # total <- total[order(total$DateTime, decreasing = TRUE),]
  total <- returnImpactNoneDF(impact_df, none_df, ngram_param, pos_wt, win_size, cal_wt)
  auc_score <- AUC(as.numeric(total$totalscore), as.numeric(total$consider))
  out_str <- paste0(out_str, ",", auc_score) 
  write2File(out_str)
  return(auc_score)
}

objStats <- function(impact_df, none_df, ngram_param, pos_wt, win_size, cal_wt){
  impact <- calculateGuessStats(impact_df, impact_df, none_df, ngram_param, 1, pos_wt, win_size, cal_wt)
  none <- calculateGuessStats(none_df, impact_df, none_df, ngram_param, 0, pos_wt, win_size, cal_wt)
  totalRatio <- impact+none
  return(totalRatio)
}

returnImpactNoneDF <- function(impact_df, none_df, ngram_param, pos_wt, win_size, cal_wt){
  impact <- returnGuessDF(impact_df, impact_df, none_df, ngram_param, 1, pos_wt, win_size, cal_wt)
  none <- returnGuessDF(none_df, impact_df, none_df, ngram_param, 0, pos_wt, win_size, cal_wt)
  total <- rbind(impact, none)
  total <- total[order(total$DateTime, decreasing = TRUE),]
  return(total)
}
##### run only once, this takes forever
tmp <- setPOSCorpusMatrix(na.omit(spy_joined))
########################################
#######################################


