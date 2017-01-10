# twitter mapping work
###########################################
# load require data main packages
############################################
x <- c("maps",  "RColorBrewer")
lapply(x, require, character.only = TRUE)


###########################################
# get the data
############################################

#load in the reference data
Con_CSV <- read.csv2("E://R/Twitter/Locations_Conts.csv", header=FALSE, 
                     sep=",", na.strings = "NA", stringsAsFactors=FALSE)
#drop join col to lower
Con_CSV$V1 <- tolower(Con_CSV$V1)

#read in data
sou_followers <- read.csv2("E://R/Twitter/follow_Mon Jan 09.txt" , sep=",", stringsAsFactors = FALSE,
                           header=TRUE)

# manipulate datasets
library(dplyr)

#drop join col to lower
sou_followers$followersLocation <- tolower(sou_followers$followersLocation)

#add in the continent details
sou_followers <- sou_followers %>% left_join(Con_CSV, c("followersLocation"="V1"))


###########################################
# do the map
############################################

# prepare the data for ploting
#remove the NA lons, enabling the map to be centred on Aus
map_followers <- subset(sou_followers, !is.na(lon))
#update the lat/lon dat types
map_followers$lon <- as.numeric(map_followers$lon)
map_followers$lat <- as.numeric(map_followers$lat)

#do the data work to centre the map on Australia and plot
### Code by Claudia Engel  March 19, 2012, www.stanford.edu/~cengel/blog

### Recenter on Australia ####
center <- 144 

# shift coordinates of followers to match the recentering
map_followers$lon.recent <- ifelse(map_followers$lon < center-180, 
                                   map_followers$lon+360,
                                   map_followers$lon)

# used for the worldmap object
library(maps)

# shift coordinates to recenter worldmap
worldmap <- map_data ("world")
worldmap$long.recenter <- ifelse(worldmap$long < center - 180 , 
                                 worldmap$long + 360, worldmap$long)

# A series of functions to accpunt for the earth not being flat
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

#plotting 
library(ggplot2)

# create the global map object
worldmap = ggplot(aes(x = long.recenter, y = lat,
                      col="#191919"), data = worldmap.cp) + 
  geom_polygon(aes(group = group.regroup), fill="black", colour = "grey65") + 
  scale_y_continuous(limits = c(-60, 85)) + 
  ggtitle(paste(userName," Follower Map",sep=""))+
  coord_equal() +  
  theme_bw() + 
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

# use colour brewer for better colour range
library(RColorBrewer)
# Set up the ploting colors
cols = brewer.pal(8,"Set1")
cols=cols[-7]

#attach the colours to each cont
map_dots = map_followers %>% filter(V2!="Parts Unknown") %>% 
  select(lon.recent, lat, lon, V2 ) %>% mutate(dot_col = ifelse(V2=="Asia", cols[1],
                                                            (ifelse(V2=="Africa",cols[2], 
                                                                    (ifelse(V2=="North America",cols[3], 
                                                                            (ifelse(V2=="Europe",cols[4],
                                                                                    ( ifelse(V2=="Australia", 
                                                                                             cols[6], cols[5]))))))))))
#make cont a factor for colouring of graph
map_dots$V2 <- as.factor(map_dots$V2)

#create the legend detail, including the NA cont
# load reshape for melt
library(reshape2)
# create the legend table
leg_tab <- melt(table(sou_followers$V2))
colnames(leg_tab) <- c("Continents", "Followers")
#add in cont colours
map_col <- map_dots %>% select(V2, dot_col) %>% distinct(V2, dot_col)
leg_tab <- leg_tab %>% left_join(map_col, c("Continents"="V2"))
#update unknown to black
leg_tab[7,3] ="#000000"
#sort for displa
leg_tab <- leg_tab %>% arrange(desc(Followers))

# load on the followers data as points, using the cont coloring
worldmap1 = worldmap + 
  geom_point(data = map_dots, aes(x=lon.recent, y=lat, color=V2), 
             pch = 19, size = 3, alpha = .6) +
  scale_colour_manual(name="",  
                      values = c("Europe"= "#984EA3",
                                 "North America"="#4DAF4A",
                                 "Australia" ="#FFFF33", 
                                 "Africa" = "#377EB8", 
                                 "Asia"="#E41A1C", 
                                 "Oceania"="#FF7F00"))
  ggtitle(paste(userName," Follower Map",sep="")) 

#load grid extra for to add on the legend as a summary table
library(gridExtra)
grid.arrange( worldmap1,
              tableGrob(leg_tab[,1:2], 
                        theme = ttheme_minimal(base_size = 8,
                                               base_colour = leg_tab[,3], 
                                               core=list(bg_params = list(fill = c("grey75", "grey95"), col=NA), 
                                                         fg_params=list(fontface="bold")),
                                               colhead=list(fg_params=list(col="black"))), 
                        #remove row id's
                        row = NULL),
              ncol=2, 
              widths=c(2.3, 0.8), 
              clip=FALSE)

# show the plot

dev.off()


