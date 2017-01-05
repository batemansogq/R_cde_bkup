# read in tweets files

#work with dates
library(lubridate)
#graphing
library(ggplot2)
# DF
library(dplyr)
# text
library(stringr)
# sebtiment
library(tidytext)

#library(readr)
# 
library(scales)
#reorg df
library(reshape2)
library(data.table)


#set directory
setwd("E://R/Twitter/Tweets")

#list all files in directory
ls_fl <- as.list(list.files("E://R/Twitter/Tweets"))

#find the created date of the file
ls_fl_dt <- file.info(list.files("E://R/Twitter/Tweets"))$mtime
ls_fl_dt <- substr(ls_fl_dt,0,10)

#make the data frame
ls_df <- as.data.frame(cbind(ls_fl, ls_fl_dt))
#arrange the df according to date
ls_df <- ls_df %>% mutate(dt=ymd(ls_fl_dt)) %>% arrange(desc(dt))

# create DF 
x <- read.csv2(paste0("E://R/Twitter/Tweets/",ls_fl[1]), sep=",", stringsAsFactors = FALSE,
               header=TRUE)

res_df <- x[0,]

#loop to read files in and append to main df
for (i in 1:nrow(ls_df)) {
  
  step_y <- read.csv2(paste0("E://R/Twitter/Tweets/",ls_df[i,1]), sep=",", stringsAsFactors = FALSE,
                 header=TRUE)
  #do a null check
  if (nrow(step_y)>0) {
    #create the logical vector
    mat_log <- paste0(step_y$id, step_y$text)  %in% paste0(res_df$id, res_df$text)
    #append only non-matches
    res_df <- rbind(res_df, step_y[!mat_log,])
  }
}


# find unqinue tweets
unq_text_df <- res_df 
# addin a text flag
unq_text_df$te <- substr(unq_text_df$text, 0, 15)
# remove tweets with no content
unq_text_df <- subset(unq_text_df, te!="FALSE")

#reweet cnt
table(unq_text_df$retweetCount)
#fav count
subset(unq_text_df, favoriteCount>=5) %>% 
  filter(favoriteCount!=FALSE) %>% 
  select(text, favoriteCount) %>% 
  arrange(desc(favoriteCount))

