setwd("/path/to/code/")

####################################Mask the directory above###############################
source("cleanTweets.R")
source("curatedSample.R")
source("createDictionaryLib.R")

library(NMOF)
OPT_FLAG <- FALSE


calculateFullAUC <- function(){#length_param, pos_param, neg_param){
  total <- returnImpactNoneDF(spy_tweets_of_interest, spy_tweets_no_impact, 1, c(1, 0.75, 0.6, 0.4, 0.6, 1.21), 6, c(1.3, 7))
  total$adj_score <-as.numeric(total$totalscore)
  
  auc_score <- AUC(as.numeric(total$adj_score), as.numeric(total$consider))

  return(auc_score)

}

addTFPN2df <- function(tmp_df, alt){
  if(alt==1){
    tmp_df$AltConsider <- ifelse(tmp_df$JUMP == 1 | tmp_df$VolRatio >1.4, 1, 0)
    tmp_df$TP <- ifelse(tmp_df$AltConsider ==1 & tmp_df$totalscore == 1, 1, 0)
    tmp_df$TN <- ifelse(tmp_df$AltConsider ==0 & tmp_df$totalscore == 0, 1, 0)
    tmp_df$FP <- ifelse(tmp_df$AltConsider==0 & tmp_df$totalscore ==1, 1, 0)
    tmp_df$FN <- ifelse(tmp_df$AltConsider==1 & tmp_df$totalscore==0, 1, 0)
  }else{
    tmp_df$TP <- ifelse(tmp_df$consider ==1 & tmp_df$totalscore == 1, 1, 0)
    tmp_df$TN <- ifelse(tmp_df$consider ==0 & tmp_df$totalscore == 0, 1, 0)
    tmp_df$FP <- ifelse(tmp_df$consider==0 & tmp_df$totalscore ==1, 1, 0)
    tmp_df$FN <- ifelse(tmp_df$consider==1 & tmp_df$totalscore==0, 1, 0)
  }
  return(tmp_df)
}


if(OPT_FLAG){
  num_cores <- as.integer( Sys.getenv("SLURM_CPUS_ON_NODE") )
  if ( is.na(num_cores) ) {
    num_cores <- as.integer( Sys.getenv("SALT_SLURM_CPUS_ON_NODE") )
  }
  if ( is.na(num_cores) ) {
    num_cores <- detectCores()
  }
#http://enricoschumann.net/files/NMOFdist.pdf
#https://www.rdocumentation.org/packages/parallel/versions/3.4.0/topics/mclapply
#https://www.rdocumentation.org/packages/NMOF/versions/1.6-0/topics/gridSearch
  GS_T0 <- Sys.time()


  set.seed(42)
  myObjFunc <- function(x)
    calculateDictAUC(spy_tweets_of_interest, spy_tweets_no_impact, ngram_param, c(x[1L], x[2L], x[3L], x[4L]), x[5L],  x[6L])
  sol <- gridSearch(fun = myObjFunc, levels = list(1,seq(0, 1, by=.1),seq(0, 1, by=.1),seq(0, 1, by=.1), seq(4, 8, by=1), 1.2), method = "multicore", mc.control=list(mc.cores=num_cores, mc.set.seed=FALSE))


  GS_T1 <- Sys.time()
  GS_T1-GS_T0

} else {


calculateTweetPrediction <- function(data_df, i){
  data_df <- normalizeAllTweets(data_df[i,])
  impactscore <- calculateTweetScore(data_df$normalizedTweet[1],  data_df$DateTime[1], spy_tweets_of_interest, spy_tweets_no_impact, 1,  "impact", c(1, 0.1, 0.7, 0.2), 6, 1.2)
  nonescore <- calculateTweetScore(data_df$normalizedTweet[1], data_df$DateTime[1], spy_tweets_of_interest, spy_tweets_no_impact, 1,  "none",  c(1, 0.1, 0.7, 0.2), 6, 1.2)

  totalscore <- ifelse((impactscore+nonescore)>0,1, 0)

  return(totalscore)
}

myscore <- calculateTweetPrediction(tweets_df, 4)
print(tweets_df$cleanTweet[4])
print(myscore)
}