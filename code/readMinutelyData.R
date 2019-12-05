library(chron)
library(zoo)
library(lubridate)
#### Provide the location of SPY.P data below, make sure to include the final slash after directory name

market_data_source <- "/path/to/SPY/minutely/data/"
#########################################################################
readSubsetFile <- function(dataSource){
  subsetFile <-read.csv(file=dataSource, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  subsetFile$date <- as.Date(subsetFile$date, "%d-%b-%Y")
  return(subsetFile)
}

getSubsetLimits <- function(data_df, ref_df, dates_array, curr_index ) {
  ric_all <- unique(data_df$RIC)
  index <- which(ref_df$date == dates_array[curr_index])
  
  if(length(index)==0){
    j<-curr_index
    while(length(index)==0 ){

      if(is.na(dates_array[j])){

        index <- which(ref_df$date == dates_array[curr_index-1])
      } else {
        index <- which(ref_df$date == dates_array[j])

      
        j<- j+1
      }
    }
    
  }
  
  RIC <- ref_df$RIC[index]
  ric_id <- which(ric_all==RIC)
  RIC_prev <- NULL
  if(ric_id == 1){
    RIC_prev <- NULL
  } else {
    RIC_prev <- ric_all[ric_id-1]
  }

  if(is.null(RIC_prev)){
    df_limits <- list(0, tail(which(data_df$RIC ==RIC), n=1))
  } else {
    df_limits <- list(tail(which(data_df$RIC==RIC_prev), n=1), tail(which(data_df$RIC ==RIC), n=1))
  }

  return(df_limits)
}

readMarketData <- function(start_date,end_date, ticker, subset_df=NULL) {

  alldates <- seq.Date(end_date, start_date, by = "-1 day")
  datasources <- apply(array(as.character(alldates)), 1,  function(x) paste0(market_data_source,ticker, x,".csv"))
  mydates <- array(as.character(alldates))

 
  for(i in 1:length(datasources)){
    if(file.exists(datasources[i])) {

      stock <-read.csv(file=datasources[i], header = TRUE, sep = ",", stringsAsFactors = FALSE)
      stock  <- subset(stock , select = c("Date.L.", "Time.L.", "RIC", "Close.Price","Volume"))
      if(!is.null(subset_df)) {
        
        limits_lst <- getSubsetLimits(stock, subset_df, mydates, i)
        start_ind <- limits_lst[[1]]+1
        stop_ind <- limits_lst[[2]]
        stock <- stock[start_ind:stop_ind,]
        
      } else {
        stock   <- na.omit(stock) 
      }

      if(exists("stock_df")) {
        stock_df <- rbind(stock, stock_df)
      } else {
        stock_df <- stock
      }
    } else {
      next
    }
  }
  

  return(stock_df)
}

fixOffsetBlanks <- function(data_df){
  blanks <- which(data_df$RIC == "")
  blank_start_index <- seq(1, length(blanks), by=60)
  blank_end_index<-seq((blank_start_index[2]-1), length(blanks), by=60)
  
  for(i in 1:length(blank_start_index)){
    data_df$Date.L.[blanks[blank_start_index[i]]:blanks[blank_end_index[i]]] <- data_df$Date.L.[blanks[blank_start_index[i]]-1]
    data_df$Time.L.[blanks[blank_start_index[i]]:blanks[blank_end_index[i]]] <- data_df$Time.L.[(blanks[blank_start_index[i]]-60):(blanks[blank_start_index[i]]-1)]
    data_df$RIC[blanks[blank_start_index[i]]:blanks[blank_end_index[i]]]<- data_df$RIC[blanks[blank_start_index[i]]-1]
  }
  
  for (i in length(blank_start_index):1){
    data_df <- data_df[-seq((blanks[blank_start_index[i]]-60),(blanks[blank_start_index[i]]-1)),]
  }
  return(data_df)
}

market_df <-readMarketData(as.Date("2017-01-20"), as.Date("2019-11-17"), "SPY")
market_df$DateTime <- as.POSIXct(paste(market_df$Date.L., market_df$Time.L.), format="%Y-%m-%d %H:%M:%S")
market_df <- market_df[ order(market_df$DateTime , decreasing = TRUE ),]
row.names(market_df) <- 1:nrow(market_df)


