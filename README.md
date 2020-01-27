# Algorithmic detection of tweets by @realDonaldTrump that are likely to have an impact on the stock market
The repository consists of the [*code/*](./code/) and [*data/*](./data/) folders.

The structure of data is as follows:  
* A series of downloaded .csv tweet files from https://www.vicinitas.io/free-tools/download-user-tweets
* A [*raw/*](./data/raw) folder which should contain: ndjson file from this
  link - the data is too large for GitHub https://doi.org/10.7910/DVN/KJEBIL
* Finally, [*online_data/*](./data/online_data/) folder that contains clean
  nontraditional data used for synonym casting. You can find the associated
  perl scripts used for data cleaning in
  [*online_data/cleanScript*](./data/online_data/cleanScript/)

In addition to this *data/*, should contain another folder called SPY/ which
 should contian a series of .csv files one for each day that has SPY.P minutely
 data for that date.  It should be named as *yyyy-mm-dd.csv* (for example,
 2004-09-20.csv).  It should have the following columns:
 Date.L, Time.L, RIC, Close Price, Volume. 

Example: 9/20/2004, 10:02:00, SPY.P, 114.45, 2300  SPY minutely data is not
 included because it is not public.

The code folder contains all the R scripts needed to build and run the model.

There are 3 stages to this.

* Stage 1: data load and cleaning, and identifying previous tweets that are
   associated with impact.  This part is time consuming.  Before attempting
   this two adjustments to the code will need to be made:
   1. In [*readMinutelyData.R*](./code/readMinutelyData.R) make sure to provide
      a path to your SPY data: market_data_source <-
      *"/path/to/SPY/minutely/data/"*
   1. In [*main.R*](./code/main.R) set your working directory at the top of the
      file.

To run stage one do the following:
```
source("cleanTweets.R")
source("curatedSample.R")
source("createDictionaryLib.R")
```

* Stage 2: Running your own opimization.
   In *main.R*  set `OPT_FLAG <- TRUE`

   `source("main.R")`

   main sources files in stage 1 make sure to comment them out to not repeat that process

* Stage 3:
   assuming stage 1 had been completed run
```
myscore <- calculateTweetPrediction(tweets_df, 4)
print(tweets_df$cleanTweet[4])
print(myscore)
```
where tweets_df$cleanTweet[4] contains your target tweet.
