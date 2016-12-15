#tidytext mining
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
#graph labels
library(ggrepel)


tw_df <- tweets_UT_df 

ggplot(data=tw_df, aes(x=created)) +
  geom_histogram(alpha=0.6, na.rm = FALSE) + 
  ggtitle(paste(userName," tweet activity",sep="")) 

# clean up the text
reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_df <- tw_df %>% select(text) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

tot = nrow(tidy_df)

# find freq of words
frequency <- as.data.frame(table(tidy_df)) %>%  
  mutate(ratio = Freq/tot) 

frequency

# graph it 
ggplot(data=frequency[1:20,], aes( reorder(tidy_df, ratio), ratio)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  coord_flip()


  #sentiment
  sentiment_nrc <- tidy_df %>%
    inner_join(get_sentiments("nrc")) 
  
  sentiment_afinn <- tidy_df %>%
    inner_join(get_sentiments("afinn")) 
  
  sentiment_bing <- tidy_df %>%
    inner_join(get_sentiments("bing")) 
  
  #graphs
  grh_nrc <- as.data.frame(table(sentiment_nrc$sentiment))
  levels(grh_nrc$Var1)<-c("negative","anger", "disgust", "fear", 
                          "sadness",  "positive","joy", "anticipation",
                          "surprise", "trust")
  
  ggplot(data = grh_nrc , aes(x = Var1, y = Freq)) +
    geom_bar(aes(fill = Var1), stat = "identity") +
    theme(legend.position = "none") +
    ggtitle("Total Sentiment Score NRC") + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  
  grh_afinn <- as.data.frame(table(sentiment_afinn$score))
  
  ggplot(data = grh_afinn , aes(x = Var1, y = Freq)) +
    geom_bar(aes(fill = Var1), stat = "identity") +
    theme(legend.position = "none") +
    ggtitle("Total Sentiment Score Afinn") + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  
  grh_bing <- as.data.frame(table(sentiment_bing$sentiment))
  
  ggplot(data = grh_bing , aes(x = Var1, y = Freq)) +
    geom_bar(aes(fill = Var1), stat = "identity") +
    theme(legend.position = "none") +
    ggtitle("Total Sentiment Score BING") + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  
  #####################################################
  # followers charts
  #######################################################
  
  
  fol_txt <- read.csv2("E://R/Twitter/Networks/DA_followers.txt", 
                       sep=",", na.strings = "NA", stringsAsFactors=FALSE)
  

  
  #drop limits for graph
  z <- scale(fol_txt[,2:3])
  
  #update d
  
  score <- apply(z, 1, mean)
  roster <- cbind(fol_txt, score)
  
  y <- quantile(score, c(.8,.6,.4,.2))
  fol_txt$Q[score >=y[1]] <- "A"
  fol_txt$Q[score < y[1] & score >= y[2]] <- "B"
  fol_txt$Q[score <y[2] & score >= y[3]] <- "C"
  fol_txt$Q[score < y[3] & score >= y[4]] <- "D"
  fol_txt$Q[score < y[4]] <- "E"
  
  sub_fol_txt <- subset(fol_txt, Q %in% c("B", "C", "D"))
  
  #update titles
  sub_fol_txt$lab <- ifelse(sub_fol_txt$cnt_friend>=2000, sub_fol_txt$txt_nam, 
                            (ifelse(sub_fol_txt$cnt_follow>=2000, sub_fol_txt$txt_nam, 
                                    (ifelse(sub_fol_txt$txt_nam=="abbruzzd","abbruzzd", ""))))) 
  
  #limited view
  ggplot(data=sub_fol_txt, aes(x=cnt_follow, y=cnt_friend)) + 
    ggtitle("Limited View") +
    xlab("Followers") +
    ylab("Friends") +
    geom_point(aes(shape=ANZ, colour=ANZ)) + 
    scale_y_continuous(limits = c(0, 3000)) +
    scale_x_continuous(limits = c(0, 3000)) +
  #  geom_smooth(method='lm') +
    geom_text_repel( data=filter(sub_fol_txt, !is.na(lab)),
      aes(
        color = factor(ANZ),
        label = lab
      ),
      point.padding = unit(0.25, "lines"),
      box.padding = unit(0.25, "lines"),
      nudge_y = 0.1
    ) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(colour = "grey40"),
      axis.title.y = element_text(colour = "grey40"),
      plot.title = element_text(colour = "grey40")
    )
  
  #update titles
  ANZ_fol_txt <- subset(fol_txt, (ANZ==TRUE | txt_nam=="abbruzzd"))
  ANZ_fol_txt$lab <- ifelse(ANZ_fol_txt$cnt_friend>=1500, ANZ_fol_txt$txt_nam, 
                            (ifelse(ANZ_fol_txt$cnt_follow>=1500, ANZ_fol_txt$txt_nam, 
                                    (ifelse(ANZ_fol_txt$txt_nam=="abbruzzd","abbruzzd", ""))))) 
  
  #ANZ view
  ggplot(data=ANZ_fol_txt, aes(x=cnt_follow, y=cnt_friend)) + 
    ggtitle("Just ANZ") +
    xlab("Followers") +
    ylab("Friends") +
    geom_point(aes(shape=ANZ, colour=ANZ)) + 
    geom_smooth(method='lm') +
    geom_text_repel( data=filter(ANZ_fol_txt, !is.na(lab)),
                     aes(
                       color = factor(ANZ),
                       label = lab
                     ),
                     point.padding = unit(0.25, "lines"),
                     box.padding = unit(0.25, "lines"),
                     nudge_y = 0.1
    ) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(colour = "grey40"),
      axis.title.y = element_text(colour = "grey40"),
      plot.title = element_text(colour = "grey40")
    )
  
  
  
  
  #everything view
  ggplot(data=fol_txt, aes(x=cnt_follow, y=cnt_friend)) + 
    ggtitle("Twitter's Darren is following") +
    xlab("Followers") +
    ylab("Friends") +
   geom_point(aes(shape=ANZ, colour=ANZ)) + 
    geom_text_repel( data=filter(fol_txt, (cnt_follow>=100000 | cnt_friend >= 100000)),
                     aes(
                      color = factor(ANZ),
                       #size = hp,
                       label = txt_nam
                     ),
                     point.padding = unit(0.25, "lines"),
                     box.padding = unit(0.25, "lines"),
                     nudge_y = 0.1
    ) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(colour = "grey40"),
      axis.title.y = element_text(colour = "grey40"),
      plot.title = element_text(colour = "grey40")
    )
  
  #####################################################
  # friends charts
  #######################################################
  
  
  frd_txt <- read.csv2("E://R/Twitter/Networks/DA_friends.txt", 
                       sep=",", na.strings = "NA", stringsAsFactors=FALSE)
  
  #drop limits for graph
  z <- scale(frd_txt[,2:3])
  
  #update d
  
  score <- apply(z, 1, mean)
  roster <- cbind(frd_txt, score)
  
  y <- quantile(score, c(.8,.6,.4,.2))
  frd_txt$Q[score >=y[1]] <- "A"
  frd_txt$Q[score < y[1] & score >= y[2]] <- "B"
  frd_txt$Q[score <y[2] & score >= y[3]] <- "C"
  frd_txt$Q[score < y[3] & score >= y[4]] <- "D"
  frd_txt$Q[score < y[4]] <- "E"
  
  sub_frd_txt <- subset(frd_txt, Q %in% c("B", "C", "D"))
  
  #update titles
  sub_frd_txt$lab <- ifelse(sub_frd_txt$cnt_friend>=2000, sub_frd_txt$txt_nam, 
                            (ifelse(sub_frd_txt$cnt_follow>=2000, sub_frd_txt$txt_nam, 
                                    (ifelse(sub_frd_txt$txt_nam=="abbruzzd","abbruzzd", ""))))) 
  
  #limited view
  ggplot(data=sub_frd_txt, aes(x=cnt_follow, y=cnt_friend)) + 
    ggtitle("Limited View") +
    xlab("Followers") +
    ylab("Friends") +
    geom_point(aes(shape=ANZ, colour=ANZ)) + 
    scale_y_continuous(limits = c(0, 3000)) +
    scale_x_continuous(limits = c(0, 3000)) +
    #  geom_smooth(method='lm') +
    geom_text_repel( data=filter(sub_frd_txt, !is.na(lab)),
                     aes(
                       color = factor(ANZ),
                       label = lab
                     ),
                     point.padding = unit(0.25, "lines"),
                     box.padding = unit(0.25, "lines"),
                     nudge_y = 0.1
    ) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(colour = "grey40"),
      axis.title.y = element_text(colour = "grey40"),
      plot.title = element_text(colour = "grey40")
    )
  
  #update titles
  ANZ_frd_txt <- subset(frd_txt, (ANZ==TRUE | txt_nam=="abbruzzd"))
  ANZ_frd_txt$lab <- ifelse(ANZ_frd_txt$cnt_friend>=1500, ANZ_frd_txt$txt_nam, 
                            (ifelse(ANZ_frd_txt$cnt_follow>=1500, ANZ_frd_txt$txt_nam, 
                                    (ifelse(ANZ_frd_txt$txt_nam=="abbruzzd","abbruzzd", ""))))) 
  
  #ANZ view
  ggplot(data=ANZ_frd_txt, aes(x=cnt_follow, y=cnt_friend)) + 
    ggtitle("Just ANZ") +
    xlab("Followers") +
    ylab("Friends") +
    geom_point(aes(shape=ANZ, colour=ANZ)) + 
    geom_smooth(method='lm') +
    geom_text_repel( data=filter(ANZ_frd_txt, !is.na(lab)),
                     aes(
                       color = factor(ANZ),
                       label = lab
                     ),
                     point.padding = unit(0.25, "lines"),
                     box.padding = unit(0.25, "lines"),
                     nudge_y = 0.1
    ) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(colour = "grey40"),
      axis.title.y = element_text(colour = "grey40"),
      plot.title = element_text(colour = "grey40")
    )
  
  
  
  
  #everything view
  ggplot(data=frd_txt, aes(x=cnt_follow, y=cnt_friend)) + 
    ggtitle("Darren's Twitter friends") +
    xlab("Followers") +
    ylab("Friends") +
    geom_point(aes(shape=ANZ, colour=ANZ)) + 
    #  scale_y_continuous(limits = c(0, 5000)) +
    #  scale_x_continuous(limits = c(0, 5000)) +
    # geom_smooth(method='lm') +
    geom_text_repel( data=filter(frd_txt, (cnt_follow>=2500000 | cnt_friend >= 20000)),
                     aes(
                       # color = factor(ANZ),
                       #size = hp,
                       label = txt_nam
                     ),
                     point.padding = unit(0.25, "lines"),
                     box.padding = unit(0.25, "lines"),
                     nudge_y = 0.1
    ) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(colour = "grey40"),
      axis.title.y = element_text(colour = "grey40"),
      plot.title = element_text(colour = "grey40")
    )
  
  #  geom_text_repel(data=filter(results, padj<0.05), aes(label=Gene))

  