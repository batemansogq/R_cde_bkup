#############################################################
# create the reference data & import the data
############################################################# 

# load packages 
x <- c("dplyr")
lapply(x, require, character.only=TRUE)

#==================================================================================
# Get the results data in and create the team listing ref data
#==================================================================================

#set the directory
setwd("E:/R/Football/football_data")

#list out the available files
fl <- list.files()

#create the blank DF
df_data_res<- data.frame(date = as.Date(character()),
                         hm_team = character(), 
                         aw_team = character(),
                         FTHG = as.factor(numeric()),
                         FTAG = as.factor(numeric()),
                         RES = as.factor(character()),
                         HTHG = as.factor(numeric()),
                         HTAG = as.factor(numeric()),
                         HTR = as.factor(character()),
                         HS= as.factor(numeric()),
                         AS= as.factor(numeric()),
                         HST= as.factor(numeric()),
                         AST= as.factor(numeric()),
                         HF= as.factor(numeric()),
                         AF= as.factor(numeric()),
                         HC= as.factor(numeric()),
                         AC= as.factor(numeric()),
                         HY= as.factor(numeric()),
                         AY= as.factor(numeric()),
                         HR= as.factor(numeric()),
                         AR= as.factor(numeric()),
                         B365H= as.factor(numeric()),
                         B365D= as.factor(numeric()),
                         B365A= as.factor(numeric()),
                         BWH= as.factor(numeric()),
                         BWD= as.factor(numeric()),
                         BWA= as.factor(numeric()),
                         LBH= as.factor(numeric()),
                         LBD= as.factor(numeric()),
                         LBA= as.factor(numeric())
                         )
#set the colnames for filtering the imports
df_cols <- c('Date','HomeTeam','AwayTeam','FTHG','FTAG','FTR','HTHG','HTAG','HTR','HS','AS',
             'HST','AST','HF','AF','HC','AC','HY','AY','HR','AR','B365H','B365D','B365A','BWH',
             'BWD','BWA','LBH','LBD','LBA')

# loop through and import them in a single DF
for (i in 1:length(fl)) {
  sou_dat <- read.csv2(fl[i], header =TRUE, sep=",")
  sou_dat<-sou_dat[,df_cols]
  
  # add the df 
  df_data_res <- rbind(df_data_res, sou_dat)  
}

#update dates
df_data_res$Date <- as.Date(df_data_res$Date, format="%d/%m/%y")

#make distinct listing of teams
tm_list <- df_data_res %>% select(HomeTeam) %>% distinct(HomeTeam)
#write the reference file
write.csv2(tm_list, file="E://R/Football/RF_team_listing_res.csv", row.names = FALSE)


#==================================================================================
# Get the league ladder data and get the team listing
#==================================================================================

#set the directory
setwd("E:/R/Football/Raw_data")

#list out the available files
fl <- list.files()
#find schedules files
sc_fl <- list.files(pattern ='*schedule.csv')
#exclude them
fl <- fl[!(fl %in% sc_fl)]

#create the blank DF
df_data_lad<- data.frame(Rd = as.factor(numeric()),
                         LgPos = as.factor(numeric()),
                         Team = character(), 
                         won  = as.factor(numeric()),
                         drw = as.factor(numeric()),
                         lost = as.factor(numeric()),
                         Goal_dif= numeric(),
                         LeaguePt = numeric(),
                         Year = as.factor(numeric()) 
)
#set the colnames for filtering the imports
df_cols <- c('pos','tm','mt','wn','dw','ls','df','lp')

# loop through and import them in a single DF
for (i in 1:length(fl)) {
  sou_dat <- read.csv2(fl[i], header =TRUE, sep=",")
  sou_dat<-sou_dat[,df_cols]
  #add in the league year
  sou_dat <- cbind(sou_dat, 'Year'=substr(fl[i],6,9))
  #re-arrange columns for df
  sou_dat <- cbind("Rd"=sou_dat[,3], sou_dat[,1:2], sou_dat[,4:9] )
  # add the df 
  df_data_lad <- rbind(df_data_lad, sou_dat)  
}
#make distinct listing of teams
tm_list <- df_data_lad %>% select(tm) %>% distinct(tm)
#write the reference file
write.csv2(tm_list, file="E://R/Football/RF_team_listing_lad.csv", row.names = FALSE)

#==================================================================================
# Get the schedule data for the round to date data & team listing
#==================================================================================

#set the directory
setwd("E:/R/Football/Raw_data")

#list out the schedules files
fl <-  list.files(pattern ='*schedule.csv')

#create the blank DF
df_data_rds<- data.frame(Rd = as.factor(numeric()),
                         HmTeam = character(), 
                         AwTeam = character(),
                         Dt = as.Date(character(),
                         Season = numeric()) 
)
#set the colnames for filtering the imports
df_cols <- c('i','hm_tm','aw_tm','dt')

# loop through and import them in a single DF
for (i in 1:length(fl)) {
  sou_dat <- read.csv2(fl[i], header =TRUE, sep=",")
  sou_dat<-cbind(sou_dat[,df_cols],"Season"=as.numeric(substr(fl[i],6,9)))
  
  # add the df 
  df_data_rds <- rbind(df_data_rds, sou_dat)  
}
#make distinct listing of teams
tm_list <- df_data_rds %>% select(hm_tm) %>% distinct(hm_tm)
#write the reference file
write.csv2(tm_list, file="RF_team_listing_rds.csv", row.names = FALSE)

#update dates
df_data_rds$dt <- as.Date(df_data_rds$dt, format="%d/%m/%Y")

#function to update the dates of the round
rd_up <- function(df=df_data_rds){
  #trap for no dates
   if (is.na(df_data_rds[1,4])) {break} 
  
    for (i in 1:nrow(df_data_rds)) {
    #set the date
      if (!is.na(df_data_rds[i,4])) {dt_fill <-df_data_rds[i,4]} 
      else {#update the date with the fill
      df_data_rds[i,4] <- dt_fill}
      
  }
  return (df_data_rds)
}
#update the dates in the rounds df
df_data_rds <- rd_up()

#==================================================================================
# import reference team listing
#==================================================================================
#set the directory
setwd("E:/R/Football")

ref_team <- read.csv2("FB_Ref_data_teams.csv", header =TRUE, sep=",")


#==================================================================================
# create base file set
#==================================================================================
# write out key files

write.csv2(df_data_rds, file="df_data_rds.csv", row.names = FALSE)
write.csv2(df_data_lad, file="df_data_lad.csv", row.names = FALSE)
write.csv2(df_data_res, file="df_data_res.csv", row.names = FALSE)
