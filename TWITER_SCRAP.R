#######################################################################
# twiter scrap with sentiment analysis & network graph
########################################################################

# load require data man packages
x <- c("twitteR", "ROAuth", "stringr", "tm", "ggmap", "dplyr", "plyr", "wordcloud")
lapply(x, require, character.only = TRUE)

#############
# step #1 - get twitter auth

#key=
#secret=
setwd("E://R/Twitter")

#download.file(url="http://curl.haxx.se/ca/cacert.pem",
#              destfile = "/cacaert.pem", 
 #             method="auto")
#authenicate <- OAuthFactory$new(consumerKey=key,
#                                consumerSecret=secret,
#                               requestURL="https://api.twitter.com/oauth/request_token",
#                                accessURL="https://api.twitter.com/oauth/access_token",
#                                authURL="https://api.twitter.com/oauth/authorize")

#setup_twitter_oauth(api_key, api_secret, token, token_secret)
#save(authenicate, file="twitter authenication.Rdata")

api_key <- "" # From dev.twitter.com
api_secret <- "" # From dev.twitter.com
token <- " " # From dev.twitter.com
token_secret <- " " # From dev.twitter.com

# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)


#############
# step #2 - get tweets

#all named
tweets <- searchTwitter("@abbruzzd", lang="en", since="2014-12-01")
tweets_df <- twListToDF(tweets)
#timeline
tweets_UT=userTimeline('@abbruzzd')
tweets_UT_df <- twListToDF(tweets_UT)


#nam <- paste0("tweets_", substr(date(),0,10))
nam1 <- paste0("tweets_time", substr(date(),0,10))
nam2 <- paste0("tweets_men", substr(date(),0,10))

#write.table(tweets_df, file=paste0("./", nam, ".txt"), sep=",", row.names = FALSE)
write.table(tweets_UT_df, file=paste0("./", nam1, ".txt"), sep=",", row.names = FALSE)
write.table(tweets_df, file=paste0("./", nam2, ".txt"), sep=",", row.names = FALSE)
############
# step 3A - get user details

UserAB <- getUser('@abbruzzd')
UserAB$getDescription()
UserAB$created
UserAB$getFriends()
UserAB$getFriendIDs()
UserAB$getFollowers()
UserAB$getFavorites()
UserAB$getProfileImageUrl()


#############
# step #3 - clean up text

TwitCorp <- Corpus(VectorSource(tweets_UT_df$text))
#lower case
TwitCorp <- tm_map(TwitCorp, content_transformer(tolower))
TwitCorp <- tm_map(TwitCorp, PlainTextDocument)
#remove URLS
#removeURL <- function(X) gsub("http[^[:space:]]*", "", x)
#TwitCorp <- tm_map(TwitCorp, content_transformer(removeURL))
#remove non-english letters or non-spaces
#removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
#TwitCorp <- tm_map(TwitCorp, content_transformer(removeNumPunct))

TwitCorp <- tm_map(TwitCorp, removePunctuation)
TwitCorp <- tm_map(TwitCorp, removeNumbers)
TwitCorp <- tm_map(TwitCorp, removeWords, stopwords("english"))


#stem 
st_TwitCorp <- tm_map(TwitCorp, stemDocument)
st_TwitCorp <- tm_map(st_TwitCorp, stripWhitespace)

#make into matrix
st_Twit_dtm <-  DocumentTermMatrix(st_TwitCorp)
#findFreqTerms(Twit_dtm, 5)
st_Twit_dtm_m <- as.matrix(st_Twit_dtm)

#############
# step #4 - make a wordcloud

st_word_freq <- sort(colSums(st_Twit_dtm_m), decreasing = T)

Cloud_col <- brewer.pal(8, "Set1")
Cloud_col <- Cloud_col[-(1:4)]

wordcloud(words=names(st_word_freq), 
          freq = st_word_freq, 
          random.order = F, 
          colors = Cloud_col)

#################
# no stem

#make into matrix
Twit_dtm <-  DocumentTermMatrix(TwitCorp)
Twit_dtm_m <- as.matrix(Twit_dtm)
word_freq <- sort(colSums(Twit_dtm_m), decreasing = T)

wordcloud(words=names(word_freq), 
          freq = word_freq, 
          random.order = F, 
          colors = Cloud_col,
          min.freq = 2)



#############
# step #5 - create a dendrogram

dist_m <- dist(scale(Twit_dtm_m))
fit <- hclust(dist_m, method = "ward.D2")
plot(fit)
