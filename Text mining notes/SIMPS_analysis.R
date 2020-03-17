# install from local zip file
#install.packages("//svrau100qsm00.oceania.corp.anz.com/mckinns2$/My Documents/Current Work/R/dplyr_0.4.3.zip", repos = NULL)
library("dplyr")

#getwd()
#setwd("//svrau100qsm00.oceania.corp.anz.com/mckinns2$/My Documents/Current Work/R/Analytics_data")
#setwd("//svrau100qsm00.oceania.corp.anz.com/mckinns2$/My Documents/Current Work/R")


#word analysis
library(tm)
# reframe data
library(reshape2)
#graphing
library(ggplot2)
#Correlogram is a graph of correlation matrix
library(corrplot)
#remove unwanted chars
library(stringr)
#req for stem
library(SnowballC)
# wordcloud
library(wordcloud)
library(RColorBrewer)

#import data
simp <- readLines("E://R/Text mining notes/SIMP_s08_e23.txt", ok=TRUE)
#remove special chars
simp <- str_replace_all(simp, "[^[:alnum:]]", " ")

#bind 3 cols into 1 and rename
#simp1 <- as.data.frame(paste(simp$V1,simp$V2,simp$V3))
simp1 <- as.data.frame(simp, stringsAsFactors = FALSE)
colnames(simp1) <- c( "Lines")


#create corpus
simp_cp <- Corpus(VectorSource(simp1))
inspect(simp_cp[1])
#clean up corpus
simp_cp <- tm_map(simp_cp, tolower)
simp_cp <- tm_map(simp_cp, PlainTextDocument)
simp_cp <- tm_map(simp_cp, removePunctuation)
simp_cp <- tm_map(simp_cp, removeNumbers)
simp_cp <- tm_map(simp_cp, removeWords, stopwords("english"))


#stem
simp_cp <- tm_map(simp_cp, stemDocument)
simp_cp <- tm_map(simp_cp, stripWhitespace)

simp_cp <- score(simp_cp)

#make into matrix
simp_dtm <-  DocumentTermMatrix(simp_cp)
findFreqTerms(simp_dtm, 5)
#most common terms
head(sort(colSums(as.matrix(simp_dtm)), decreasing=TRUE),20)
# find coorrlation between words
findAssocs(simp_dtm, c("homer" , "frank"), corlimit=0.1)

#remove sparse terms - not required
#simp_dtm_com = removeSparseTerms(simp_dtm, sparse = 0.43)
#inspect(simp_dtm_com[,1:10])

simp_dtm_m <- as.matrix(simp_dtm)
# simp_dtm_m <- melt(simp_dtm_m, value.name = "count") - not required, single line
head(simp_dtm_m)

#==============================================================
# sentiment
# http://www.r-bloggers.com/sentiment-analysis-on-donald-trump-using-r-and-tableau/
#===============================================================

positives <- read.table("E://R/Text mining notes/positive-words.txt", header=FALSE, skip=35)
negatives = read.table("E://R/Text mining notes/negative-words.txt", header=FALSE, skip=35)

sentiment_scores = function(df, positive_words, negative_words){
    df_score <-data.frame(Score=integer())
  
    for (i in 1:nrow(df)) {
      #make lower
      df_l = tolower(df[i,])
      # split sentence into words with str_split function from stringr package
      word_list = str_split(df_l, " ")
      words = unlist(word_list)
      # get the position of the matched term or NA
      # we just want a TRUE/FALSE
      positive_matches = sum(!is.na(match(words,positives$V1)), na.rm=TRUE)
      negative_matches = sum(!is.na(match(words,negatives$V1)), na.rm=TRUE)
      #get scores
      score = sum(positive_matches) - sum(negative_matches)
      df_score[i,] <- score 
         
    }
    return(df_score) 
}

score = sentiment_scores(simp1, positives, negatives)
simp1$score=score
#Let's plot a histogram of the sentiment score:

qplot(simp1$score, geom="histogram") 

#word cloud vs corpus
wordcloud(simp_cp, scale=c(5,0.5), 
          max.words=100, 
          random.order=FALSE, 
          rot.per=0.35, 
          use.r.layout=FALSE, 
          colors=brewer.pal(8, 'Dark2'))

# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
head(simp_dtm_m) 
v <- sort(colSums(simp_dtm_m ),decreasing=TRUE)
df <- data.frame(word = names(v),freq=v)
head(df, 10)

col_sent = function(df, positive_words, negative_words){
  df_score <-data.frame(Score=integer())
  #loop across all words in list
  for (i in 1:nrow(df)) {
    #make lower
    df_l = tolower(df[i,1])
    #find a pos or neg sentiment for that word
    positive_matches = sum(!is.na(match(df_l,positives$V1)), na.rm=TRUE)
    negative_matches = sum(!is.na(match(df_l,negatives$V1)), na.rm=TRUE)
    #get scores
    score = if (positive_matches>=1){1} else if (negative_matches>=1) {-1} else {0}
    df_score[i,] <- score 
    
  }
  return(df_score) 
}

score = col_sent(df, positives, negatives)
df$score=score

#word cloud from a data frame
set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
        #  colors=brewer.pal(8, "Dark2"))
        colours=df$score)

#heatmap
ggplot(head(df,55), aes(X=freq, y=word, fill=score)) + 
  geom_tile(aes(score), colour = "white") + 
  scale_fill_gradient(low = "red", high = "steelblue", name="Ep Sentiment")