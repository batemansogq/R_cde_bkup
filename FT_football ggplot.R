# https://raw.githubusercontent.com/johnburnmurdoch/johnburnmurdoch.github.io/master/slides/r-ggplot/r-scripts.R?
#set directory for packages
#setwd("//svrau100qsm00.oceania.corp.anz.com/mckinns2$/My Documents/Current Work/R")


library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(RColorBrewer)
library(scales)

#pull data
allSeasons <- read.csv('https://raw.githubusercontent.com/johnburnmurdoch/johnburnmurdoch.github.io/master/slides/r-ggplot/changing-tides-of-football.csv', stringsAsFactors = F)
oneSeason <- allSeasons %>% filter(year==2016)


#1 - dot chart in a line
ggplot(oneSeason
       ,aes(x=1,score,col=country)) + 
  geom_point(size=3) + 
  scale_color_manual(breaks=unique(allSeasons$country),values=brewer_pal('qual',2)(7)) +
  theme_bw() +
  theme(legend.position="right", legend.direction="vertical") +
  xlab('') +
  ggtitle("dot chart in a line") +
  theme(plot.title = element_text(lineheight=.6, face="italic"))

#2 - dot chart spread in across rank 
ggplot(oneSeason
       ,aes(rank,score,col=country)) + 
  geom_point(size=3) + 
  scale_color_manual(breaks=unique(allSeasons$country),values=brewer_pal('qual',2)(7)) +
  theme_bw() +
  theme(legend.position="top") +
  ggtitle("dot chart spread in across rank") +
  theme(plot.title = element_text(lineheight=.6, face="italic"))

#3 - dot chart linked by line, spread in across rank 
ggplot(oneSeason
       ,aes(rank,score,col=country)) + 
  geom_path() + 
  geom_point(size=3) + 
  scale_color_manual(breaks=unique(allSeasons$country),values=brewer_pal('qual',2)(7)) +
  theme_bw() +
  theme(legend.position="top")+
  ggtitle("dot chart linked by line, spread in across rank ") +
  theme(plot.title = element_text(lineheight=.6, face="italic"))

#4 ENG vs. ESP strenght gap
ggplot(allSeasons %>%
         filter(year==2016 & country %in% c('ENG','ESP')) %>%
         dplyr::select(score, rank, country) %>% 
         spread(., country, score) %>% 
         rowwise() %>%
         mutate(
           gap = ESP-ENG,
           max = max(ESP,ENG),
           min = min(ESP,ENG)
         )
       ,aes(rank,min,fill=gap>0)) + 
  geom_rect(aes(xmin = rank-0.5, xmax=rank+0.5, ymin=min, ymax=max), alpha=0.5) +
  scale_fill_manual(breaks=unique(allSeasons$country),values=brewer_pal('qual',2)(7)) +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("strenght across the league gap chart") +
  theme(plot.title = element_text(lineheight=.6, face="italic"))

#5 season strength across rank, smooth line
ggplot(oneSeason %>%
         filter(country == 'ENG'),aes(rank,score,col=country)) + 
  geom_ribbon(aes(ymin = atw, ymax = atb, fill=country),col='transparent', alpha=0.5) +
  #add a stair case plot
  geom_path(alpha=1, size=1.5) + 
  geom_point(size=3) + 
  scale_color_manual(breaks=unique(allSeasons$country),values=brewer_pal('qual',2)(7)) +
  scale_fill_manual(breaks=unique(allSeasons$country),values=brewer_pal('qual',2)(7)) +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("strenght band across the league") +
  theme(plot.title = element_text(lineheight=.6, face="italic"))

#6 season strength across rank, box and dots
ggplot(oneSeason %>%
         filter(country == 'ENG'),aes(rank,score,col=country, fill=country)) + 
  geom_rect(aes(xmin=rank-0.5, xmax=rank+0.5, ymin = atw, ymax = atb),col='transparent', alpha=0.5) +
  geom_point(size=5) + 
  scale_color_manual(breaks=unique(allSeasons$country),values=brewer_pal('qual',2)(7)) +
  scale_fill_manual(breaks=unique(allSeasons$country),values=brewer_pal('qual',2)(7)) +
  theme_bw() +
  theme(legend.position="none")+
  ggtitle("season strength across rank, box and dots") +
  theme(plot.title = element_text(lineheight=.6, face="italic"))

#7 season strength across rank, box and lines, gaps between boxes
ggplot(oneSeason %>%
         filter(country == 'ENG'),aes(rank,score,col=country, fill=country)) + 
  geom_rect(aes(xmin=rank-0.45, xmax=rank+0.45, ymin = atw, ymax = atb),col='transparent', alpha=0.5) +
  geom_point(size=5) + 
  scale_color_manual(breaks=unique(allSeasons$country),values=brewer_pal('qual',2)(7)) +
  scale_fill_manual(breaks=unique(allSeasons$country),values=brewer_pal('qual',2)(7)) +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("season strength across rank, gaps for boxes") +
  theme(plot.title = element_text(lineheight=.6, face="italic"))

#8 all leagues, facetted across year, 5X5 grid
ggplot(allSeasons,aes(rank,score,col=country)) + 
  geom_path() + 
  geom_point(size=1) + 
  scale_color_manual(breaks=unique(allSeasons$country),values=brewer_pal('qual',2)(7)) +
  facet_wrap(~year, ncol=4) + 
  theme_bw() +
  theme(legend.position="top") +
  ggtitle("Facetted graph grid") +
  theme(plot.title=element_text(size=12, color="red", face="italic"))


#9 strenght gap between ENG v ESP grid by season
ggplot(allSeasons %>%
         filter(country %in% c('ENG','ESP')) %>%
         dplyr::select(score, year, rank, country) %>% 
         spread(., country, score) %>% 
         rowwise() %>%
         mutate(
           gap = ESP-ENG,
           max = max(ESP,ENG),
           min = min(ESP,ENG)
         )
       ,aes(rank,min,fill=gap>0)) + 
  geom_rect(aes(xmin = rank-0.5, xmax=rank+0.5, ymin=min, ymax=max), alpha=0.5) +
  scale_fill_manual(breaks=unique(allSeasons$country),values=brewer_pal('qual',2)(7)) +
  facet_wrap(~year, ncol=4) + 
  theme_bw() +
  theme(legend.position="none") + 
  ggtitle("Facetted strenght gap grid") +
  theme(plot.title=element_text(size=12, color="light green", face="italic"))

#10 Facetted strenght bar w gap grid
ggplot(allSeasons %>% filter(country == 'ENG'),aes(rank,score,col=country, fill=country)) + 
  geom_rect(aes(xmin=rank-0.45, xmax=rank+0.45, ymin = atw, ymax = atb),col='transparent', alpha=0.5) +
  geom_path(alpha=1) + 
  geom_point(size=1) + 
  scale_color_manual(breaks=unique(allSeasons$country),values=brewer_pal('qual',2)(7)) +
  scale_fill_manual(breaks=unique(allSeasons$country),values=brewer_pal('qual',2)(7)) +
  facet_wrap(~year, ncol=4) + 
  theme_bw() +
  theme(legend.position="none")  + 
  ggtitle("Facetted strenght bar w gap grid") +
  theme(plot.title=element_text(size=12, color="light blue", face="italic"))

