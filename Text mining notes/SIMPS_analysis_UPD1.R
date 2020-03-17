# install from local zip file
install.packages("//svrau100qsm00.oceania.corp.anz.com/mckinns2$/My Documents/Current Work/R/dplyr_0.4.3.zip", repos = NULL)
library("dplyr")

#getwd()
#setwd("//svrau100qsm00.oceania.corp.anz.com/mckinns2$/My Documents/Current Work/R/Analytics_data")
setwd("E://R/Text mining notes")


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

#import data, use 2 to get a complete line.
simp <- read.csv2("SIMP_s08_e23.txt", 
                  header=FALSE,
                  na.strings=c("-", "[", "]"),
                  blank.lines.skip=TRUE)
#remove special chars
simp$V1 <- str_replace_all(simp$V1, "[^[:alnum:]]", " ")
#simp$V2 <- str_replace_all(simp$V2, "[^[:alnum:]]", " ")
#simp$V3 <- str_replace_all(simp$V3, "[^[:alnum:]]", " ")

#bind 3 cols into 1 and rename
#simp1 <- as.data.frame(paste(simp$V1,simp$V2,simp$V3))
colnames(simp) <- c( "Lines")


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

positives= readLines("\\\\svrau100qsm00.oceania.corp.anz.com/mckinns2$/My Documents/Current Work/R/positive-words.txt")
negatives = readLines("\\\\svrau100qsm00.oceania.corp.anz.com/mckinns2$/My Documents/Current Work/R/negative-words.txt")

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
      positive_matches = sum(!is.na(match(words,positives)), na.rm=TRUE)
      negative_matches = sum(!is.na(match(words,negatives)), na.rm=TRUE)
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


set.seed(142)   
freq <- sort(simp_dtm_m, decreasing=TRUE)   
wordcloud(colnames(simp_dtm_m), min.freq=5, scale=c(5, .1), colors=brewer.pal(6, "Dark2")) 
wordcloud(colnames(simp_dtm_m), min.word=5, scale=c(5, .1), colors=brewer.pal(6, "Dark2")) 

######################################################################
# http://juliasilge.com/blog/Joy-to-the-World/
# https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
#####################################################################

library(syuzhet)

#import data, use 2 to get a complete line.
simp <- read.csv2("\\\\svrau100qsm00.oceania.corp.anz.com/mckinns2$/My Documents/Current Work/R/SIMP_s08_e23.txt", 
                  header=FALSE,
                  na.strings=c("-", "[", "]"),
                  blank.lines.skip=TRUE)
#remove special chars
simp1 <- gsub("\\[", " ", simp$V1)
simp1 <- gsub("\\]", " ", simp1)
simp1 <- gsub("-", " ", simp1)
simp1 <- gsub("\\(", " ", simp1)
simp1 <- gsub("\n", " ", simp1)

# get sentence structure
simp_sent <- get_sentences(simp1)
#get sentiment
simp_sentiment <- get_nrc_sentiment(simp_sent)
head(simp_sentiment)
tail(simp_sentiment)

#graph it
# limit to raw 
sentimentTotals <- data.frame(colSums(simp_sentiment[,c(1:8)]))
# include all 
# sentimentTotals <- data.frame(colSums(simp_sentiment))

names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score each sentence in script")


pra <- cbind(simp_sent, simp_sentiment)
head(pra)


