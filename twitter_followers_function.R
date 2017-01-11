##########################################
# http://biostat.jhsph.edu/~jleek/code/twitterMap.R
#########################################

#########################################################################################
#   An R function to make a personalized map of people you follow and who follow you on twitter. 
#   R functions Copyright (C) 2011 Jeff Leek (jtleek@gmail.com), and the Simply Statistics Blog
#   (http://simplystatistics.tumblr.com, http://twitter.com/simplystats)
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details, see <http://www.gnu.org/licenses/>.
#
#
#   These functions depend on the packages: twitteR, maps, geosphere, and RColorBrewer. It will
#   attempt to install them if they are not installed when you source this function. Care
#   should be used when using this function since the twitteR API has rate limiting in place.
#   If you have a large number of followers, or run the function many times, you may be
#   rate limited. 
#
#
#   How to use: 
#       # Source the function
#       source("http://biostat.jhsph.edu/~jleek/code/twitterMap.R")
#
#      # Make your twittermap
#      twitterMap("simplystats")
#
#      #If your location can't be found or latitude longitude can't be calculated
#      #choose a bigger city near you. The list of cities used by twitterMap
#      #can be found like so:
#      data(world.cities)
#      grep("Baltimore",world.cities[,1])
#
#      # Then make the map using that big city
#      twitterMap("simplystats",userLocation="Baltimore")
#   
#      #If you want both your followers and people you follow in a plot you can do:
#      twitterMap("simplystats",plotType="both")
#      
########################################################################################
getPckg <- function(pckg) install.packages(pckg, repos = "http://cran.r-project.org")

pckg = try(require(twitteR))
if(!pckg) {
  cat("Installing 'twitteR' from CRAN\n")
  getPckg("twitteR")
  require("twitteR")
}

pckg = try(require(maps))
if(!pckg) {
  cat("Installing 'maps' from CRAN\n")
  getPckg("maps")
  require("maps")
}

pckg = try(require(geosphere))
if(!pckg) {
  cat("Installing 'geosphere' from CRAN\n")
  getPckg("geosphere")
  require("geosphere")
}


pckg = try(require(RColorBrewer))
if(!pckg) {
  cat("Installing 'RColorBrewer' from CRAN\n")
  getPckg("RColorBrewer")
  require("RColorBrewer")
}

Con_CSV <- read.csv2("E://R/Twitter/Locations_Conts.csv", header=FALSE, 
                     sep=",", na.strings = "NA", stringsAsFactors=FALSE)
#drop join col to lower
Con_CSV$V1 <- tolower(Con_CSV$V1)

#twitterMap <- function(userName,fileName="twitterMap.pdf",
 #                      plotType=c("followers","both","following")){
  
  # Get location data
  cat("Getting data from Twitter, this may take a moment.\n")
  tmp = getUser('@abbruzzd')
    userName = '@abbruzzd'
    userLocation = 'Melbourne'
    userCountry = 'Australia'
    fileName="twitterMap.pdf"
    
  #updated to DF
  followers=twListToDF(tmp$getFollowers())
  followersLocation = tolower(followers$location)
  
  #correct for US
  followersLocation <- replace(followersLocation, followersLocation=="melbourne", "melbourne, Australia")
  following = twListToDF(tmp$getFriends())
  followingLocation = following$location
    
  #correct for US
  followingLocation <- replace(followingLocation, followingLocation=="melbourne", "melbourne, Australia")
  
  # Load the geographic data
  data(world.cities)
  data(us.cities)
  data(canada.cities)
  
  # Find the latitude and longitude of the user
    userLL <- world.cities %>% filter(name==userLocation & country.etc==userCountry) %>% 
      select(long, lat)
  
  
  # Find the latitude and longitude of each of the followers/following
  # and calcualte the distance to the user
  
  #followersLL = matrix(NA,nrow=length(followers),ncol=4)
  #followingLL = matrix(NA,nrow=length(following),ncol=4)
  
  #using ggmap - goole API
  tst_followers <- cbind(followersLocation, geocode(followersLocation), dist=NA)
  tst_following <- cbind(followingLocation, geocode(followingLocation),dist=NA)
  
  write.table(tst_followers, file=paste0("./follow_", substr(date(),0,10), ".txt"), sep=",", row.names = FALSE)
  write.table(tst_following, file=paste0("./friends_", substr(date(),0,10), ".txt"), sep=",", row.names = FALSE)
  
  #find the distance between the points
  for (i in 1:nrow(tst_followers)) {
    if (!is.na(tst_followers[i,2])) {
     tst_followers[i,4] =distCosine(round(userLL,0),round(tst_followers[i,(2:3)],0))
    }
  }
  
  for (i in 1:nrow(tst_following)) {
    if (!is.na(tst_following[i,2])) {
      tst_following[i,4] =distCosine(round(userLL,0),round(tst_following[i,(2:3)],0))
    }
  }
  
  #add in the continent details
  tst_followers <- tst_followers %>% left_join(Con_CSV, c("followersLocation"="V1"))
  tst_following <- tst_following %>% left_join(Con_CSV, c("followingLocation"="V1"))
    
  # remove NA continents
  for (i in 1:nrow(tst_followers)) {
    if (is.na(tst_followers[i,5])) {
      tst_followers[i,5] ="Parts Unknown"
    }
  }
  
  for (i in 1:nrow(tst_following)) {
    if (is.na(tst_following[i,5])) {
      tst_following[i,5] ="Parts Unknown"
    }
  }
  
  cat("Plotting results.\n")
  # Set up the colors
  cols = brewer.pal(8,"Set1")
  cols=cols[-7]
  
 
  ## Just followers
  if(plotType=="followers"){
    pdf(fileName,height=6,width=10)
    data(worldMapEnv)
 #   map('world',col="#191919",bg="black",fill=T,mar=rep(0,4),border=0,
  #      orientation=c(90,0,180), wrap=TRUE)
    
    map("world",projection="rectangular",
        col="#191919",
        bg="black",
        fill=T,
        mar=rep(0,4),
        border=0,
        parameter=0,orientation=c(95,-36,180), wrap = TRUE)
    
    mtext(paste(userName," Follower Map",sep=""),col="lightgrey")
    nFollowers = dim(tst_followers)[1]
    for(i in 1:nFollowers){
      if (tst_followers[i,5] != "Parts Unknown") {
        #greatC = getGreatCircle(userLL,tst_followers[i,2:3])
        greatC = gcIntermediate(userLL, tst_followers[i,2:3], n=50,
                                #breakAtDateLine = TRUE,
                                addStartEnd=TRUE)
        lines(greatC,col=if (tst_followers[i,5]=='Asia') {cols[1]} 
              else if (tst_followers[i,5]=='Africa') {cols[2]}
              else if (tst_followers[i,5]=='North America') {cols[3]}
              else if (tst_followers[i,5]=='Australia') {cols[4]}
              else if (tst_followers[i,5]=='Europe') {cols[5]}
              else  cols[6]
              ,lwd=0.8)
        }
    }
    
    legend(-190,10,legend = c(paste("Asia",nrow(subset(tst_followers, V2=='Asia'))),
                             paste("Africa",nrow(subset(tst_followers, V2=='Africa'))),
                             paste("N. America",nrow(subset(tst_followers, V2=='North America'))),
                            # paste("S. America",nrow(subset(tst_followers, V2=='South America'))),
                             paste("Australia",nrow(subset(tst_followers, V2=='Australia'))),
                             paste("Europe",nrow(subset(tst_followers, V2=='Europe'))),
                             paste("Oceania",nrow(subset(tst_followers, V2=='Oceania'))), 
                             paste("Parts Unknown",nrow(subset(tst_followers, V2=='Parts Unknown')))
                             ),
           text.col=cols[1:7],
           bg="black",cex=0.75)
    mtext("taken from @simplystats twitterMap",side=1,adj=1,cex=0.8,col="grey")
    dev.off()
    dev.off()
    
  }
  
}


getGreatCircle = function(userLL,relationLL){
  tmpCircle = greatCircle(userLL,relationLL)
  start = which.min(abs(tmpCircle[,1] - userLL[1,1]))
  end = which.min(abs(tmpCircle[,1] - relationLL[1]))
  greatC = tmpCircle[start:end,]
  return(greatC)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# run the function
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

twitterMap <- function(userName,
                       fileName="twitterMap.pdf",
                       plotType=c("follower","both","following"))
  
  
twitterMap("@abbruzzd")  


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# centre a map on aussie
# http://stackoverflow.com/questions/11201997/world-map-with-ggmap
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

library(maps)
library(plyr)
library(ggplot2)
library(sp)
library(ggmap)

# Get some points to plot - CRAN Mirrors
#Mirrors = getCRANmirrors(all = FALSE, local.only = FALSE)

#Mirrors$Place = paste(Mirrors$City, ", ", Mirrors$Country, sep = "")    # Be patient
#tmp = geocode(Mirrors$Place)
#Mirrors = cbind(Mirrors, tmp)

###################################################################################################
# Recentre worldmap (and Mirrors coordinates) on longitude 160
### Code by Claudia Engel  March 19, 2012, www.stanford.edu/~cengel/blog

### Recenter ####
center <- 144 # positive values only

# shift coordinates to recenter CRAN Mirrors
#Mirrors$long.recenter <- ifelse(Mirrors$lon < center - 180 , Mirrors$lon + 360, Mirrors$lon)
tst_followers$lon.recent <- ifelse(tst_followers$lon < center-180, 
                                   tst_followers$lon+360,
                                   tst_followers$lon)

# shift coordinates to recenter worldmap
worldmap <- map_data ("world")
worldmap$long.recenter <- ifelse(worldmap$long < center - 180 , 
                                 worldmap$long + 360, worldmap$long)

### Function to regroup split lines and polygons
# Takes dataframe, column with long and unique group variable, 
# returns df with added column named group.regroup
RegroupElements <- function(df, longcol, idcol){
  g <- rep(1, length(df[,longcol]))
  if (diff(range(df[,longcol])) > 300) { # check if longitude within group differs more 
    # than 300 deg, ie if element was split
    d <- df[,longcol] > mean(range(df[,longcol])) # we use the mean to help us separate 
    # the extreme values
    g[!d] <- 1 # some marker for parts that stay in place 
    # (we cheat here a little, as we do not take into account concave polygons)
    g[d] <- 2 # parts that are moved
  }
  g <- paste(df[, idcol], g, sep=".") # attach to id to create unique group variable 
  # for the dataset
  df$group.regroup <- g
  df
}

### Function to close regrouped polygons
# Takes dataframe, checks if 1st and last longitude value are the same,
# if not, inserts first as last and reassigns order variable
ClosePolygons <- function(df, longcol, ordercol){
  if (df[1,longcol] != df[nrow(df),longcol]) {
    tmp <- df[1,]
    df <- rbind(df,tmp)
  }
  o <- c(1: nrow(df)) # rassign the order variable
  df[,ordercol] <- o
  df
}

# now regroup
worldmap.rg <- ddply(worldmap, .(group), RegroupElements, "long.recenter", "group")

# close polys
worldmap.cp <- ddply(worldmap.rg, .(group.regroup), ClosePolygons, "long.recenter", "order") 
# use the new grouping var
#############################################################################

# Plot worldmap using data from worldmap.cp
#windows(9.2, 4)
# map('world',col="#191919",bg="black",fill=T,mar=rep(0,4),border=0)
worldmap = ggplot(aes(x = long.recenter, y = lat,col="#191919"), data = worldmap.cp) + 
                   geom_polygon(aes(group = group.regroup), fill="black", colour = "grey65") + 
  scale_y_continuous(limits = c(-60, 85)) + 
  ggtitle(paste(userName," Follower Map",sep=""))+
  coord_equal() +  theme_bw() + 
  theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(colour = "grey65"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank() ,
        panel.border = element_rect(colour = "black"),
        panel.background = element_rect(fill = "black", colour = "black"),
       plot.background = element_rect(fill = "black", colour = "black")
            )

tst_line = tst_followers %>% filter(V2!="Parts Unknown") %>% 
  select(lon.recent, lat, lon, V2 ) %>% mutate(col = ifelse(V2=="Asia", cols[1],
                                                               (ifelse(V2=="Africa",cols[2], 
                                                                       (ifelse(V2=="North America",cols[3], 
                                                                               (ifelse(V2=="Europe",cols[4],
                                                                                       ( ifelse(V2=="Australia", 
                                                                                                cols[5], cols[6]))))))))))

#bubble colour
#bubble size

leg_tab <- melt(table(tst_followers$V2))
colnames(leg_tab) <- c("continents", "Followers")

library(gridExtra)
library(reshape2)

# Plot the CRAN Mirrors
worldmap1 = worldmap + 
  geom_point(data = tst_line, aes(lon.recent, lat , colour = col), 
             pch = 19, size = 3, alpha = .6) +
  scale_colour_manual(breaks = tst_line$V2, 
                      values = unique(as.character(tst_line$col)))+
  ggtitle(paste(userName," Follower Map",sep="")) + 
  scale_fill_discrete(breaks=tst_line$V2,
                      labels=tst_line$V2)
  #annotation_custom(tableGrob(leg_tab), xmin=200, xmax=205, ymin=2.5, ymax=1)

leg <- leg_tab

grid.arrange( worldmap1,
             tableGrob(leg,
                       gridExtra::ttheme_default(
                         core = list(fg_params=list(cex = 2.0)),
                         colhead = list(fg_params=list(cex = 1.0)),
                         rowhead = list(fg_params=list(cex = 1.0)))),
             ncol=2, widths=c(1, 1), clip=FALSE)

plot(worldmap1)
dev.off()
# geom_line(aes(x1, y1, colour=g1), d1

# Colour Aust

worldmap + geom_polygon(data = subset(worldmap.cp, region == "Australia", 
                                      select = c(long, lat, group.regroup)), 
                        aes(x = long, y = lat, group = group.regroup), fill = "grey65")


plot(worldmap)
dev.off()

##############################################################################
# find the followers
#################################################################################

DF_follow_DA <- followers$screenName
DF_friends_DA <- following$screenName

#loop for follows, write out files
for (i in 1:length(DF_follow_DA)) {
  
  if (i==1) {res_df <- data.frame(txt_nam = character(),
                                  cnt_follow = numeric(),
                                 cnt_friend = numeric(), 
                                  cnt_tweet = numeric())}
    
  x_txt <- DF_follow_DA[i]
  x_FO <- getUser(DF_follow_DA[i])$followersCount
  x_FR <- getUser(DF_follow_DA[i])$friendsCount
  x_TW <- getUser(DF_follow_DA[i])$statusesCount
  
  x_df <- as.data.frame(cbind.data.frame(txt_nam = x_txt, cnt_follow =x_FO, 
                              cnt_friend =x_FR, cnt_tweet =x_TW))
  res_df <- rbind.data.frame(res_df, x_df)
  
  if(i==length(DF_follow_DA)) {  
    # add in ANZ flag
    res_df <- cbind.data.frame(res_df, ANZ=grepl('anz', followers$description, ignore.case=TRUE))
    # addin DA
    DA_txt <- getUser(userName)$screenName
    DA_FO <- getUser(userName)$followersCount
    DA_FR <- getUser(userName)$friendsCount
    DA_TW <- getUser(userName)$statusesCount
    res_df <- rbind.data.frame(res_df, as.data.frame(cbind.data.frame(txt_nam = DA_txt, 
                                                cnt_follow = DA_FO, 
                                                cnt_friend = DA_FR, 
                                                cnt_tweet = DA_TW, 
                                                ANZ="ZDA" )))
    
    # save file
    write.table(res_df, file=paste0("./Networks/followers_", userName, substr(date(),0,10), ".txt"), 
                      sep=",", row.names = FALSE)
    print("job done") }

  print(paste("end ", i))
}

## UPD this for the new logic
#loop for follows, write out files
for (i in 1:length(DF_friends_DA)) {
  
  if (i==1) {res_df <- data.frame(txt_nam = character(),
                                  cnt_follow = numeric(),
                                  cnt_friend = numeric(), 
                                  cnt_tweet = numeric())}
  
  x_txt <- DF_friends_DA[i]
  x_FO <- as.numeric(getUser(DF_friends_DA[i])$followersCount)
  x_FR <- as.numeric(getUser(DF_friends_DA[i])$friendsCount)
  x_TW <- as.numeric(getUser(DF_friends_DA[i])$statusesCount)
  
  x_df <- cbind.data.frame(txt_nam = x_txt, cnt_follow =as.numeric(x_FO), 
                              cnt_friend =x_FR, cnt_tweet=x_TW)
  res_df <- rbind.data.frame(res_df, x_df)
  
  if(i==length(DF_friends_DA)) {  
    # add in ANZ flag
    res_df <- cbind.data.frame(res_df, ANZ=grepl('anz', following$description, ignore.case=TRUE))
    # addin DA
    DA_txt <- getUser(userName)$screenName
    DA_FO <- as.numeric(getUser(userName)$followersCount)
    DA_FR <- as.numeric(getUser(userName)$friendsCount)
    DA_TW <- as.numeric(getUser(userName)$statusesCount)
    res_df <- rbind.data.frame(res_df, as.data.frame(cbind.data.frame(txt_nam = DA_txt, 
                                                cnt_follow = DA_FO, 
                                                cnt_friend = DA_FR, 
                                                cnt_tweet = DA_TW, 
                                                ANZ="ZDA")))
    
    # save file
    write.table(res_df, file=paste0("./Networks/friends_", userName, substr(date(),0,10), ".txt"), 
                sep=",", row.names = FALSE)
    print("job done") }
  
  print(paste("end ", i))
}

#############################################
# relationship between tweets & followers
###############################################


str(res_df)

ggplot(data=res_df, aes(cnt_tweet, cnt_friend)) + geom_point()



