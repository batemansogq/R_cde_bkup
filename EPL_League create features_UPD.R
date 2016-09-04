#############################################################
# re-factor of the additional features creating, adding in ELO form
# based upon the 64% accuratcy model,
############################################################# 

# read in source data
setwd("E:/R/Football")
df_data_rds <- read.csv2( file="df_data_rds.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)
df_data_lad <- read.csv2( file="df_data_lad.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)
df_data_res <- read.csv2( file="df_data_res.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)
ref_team <- read.csv2("FB_Ref_data_teams.csv", header =TRUE, sep=",", stringsAsFactors = FALSE)

# load packages 
x <- c("dplyr",  "plyr", "zoo", "stringr", "sqldf", "DataCombine")
lapply(x, require, character.only=TRUE)

#==================================================================================
# group the table
#==================================================================================
for (i in 1:nrow(df_data_lad)) {
  #go thru each row and find the current position
  if (df_data_lad[i,"pos"]>=17) {
    tab_pos=4} else if (df_data_lad[i,"pos"]>=11) {
      tab_pos=3} else if (df_data_lad[i,"pos"]>=5) {
        tab_pos=2} else {tab_pos=1}
  #update the df
  df_data_lad$tab_grp[i] <- tab_pos

  }


#==================================================================================
# form H
#==================================================================================
#merge rounds with ref for joining
df_data_rds <- df_data_rds %>% left_join(ref_team, c("hm_tm" = "Ladder")) %>% select(-Rounds)
#cleanup
colnames(df_data_rds)[6]="Hm_Res_lk"
#away team link
df_data_rds <- df_data_rds %>% left_join(ref_team, c("aw_tm" = "Ladder")) %>% select(-Rounds)
#cleanup
colnames(df_data_rds)[7]="Aw_Res_lk"

#######################
# add in the ELO scores
#######################

ls <- ref_team[,3]
#convert dates for joining
df_data_rds$dt <- as.Date( as.character(df_data_rds$dt), "%Y-%m-%d")
df_data_res$Date <- as.Date( as.character(df_data_res$Date), "%Y-%m-%d")

#create results df
#get df structure
df_res_h <-df_data_rds[1,]
#addin ELO place holders
df_res_h <- cbind(df_res_h, "hm_elo" = 0.0)
#remove row to get blank df
df_res_h <-df_res_h[0,]

# loop thru HM filtering on team adding in elo
for (j in 1:length(ls)) {
  #create the file ref
  fil <- paste0("E://R/Football/ELO/", ls[j], ".csv")
  
  if (file.exists(fil)) {
    #get the data
    df_fil <- read.csv2( file=fil, header = TRUE, sep=",")
    #convert dates
    df_fil$From <- as.Date( as.character(df_fil$From), "%Y-%m-%d")
    df_fil$To <- as.Date( as.character(df_fil$To), "%Y-%m-%d")
    #update col names to avoid sqldf join issues
    colnames(df_fil)[6] <- "fm"
    colnames(df_fil)[7] <- "ed"
    
    #get the data subset
    df_sub <- df_data_rds %>% filter(Hm_Res_lk== ls[j])
    #join to get ELO, use SQl
    df_sub <- sqldf("select df_sub.* ,df_fil.elo 
                    from df_sub left outer join df_fil
                    where df_sub.Hm_Res_lk = df_fil.club
                    and df_sub.dt between df_fil.fm and df_fil.ed")
    colnames(df_sub)[8] <- "hm_elo"
    
    #append to Results
    df_res_h <- rbind(df_res_h, df_sub)
  }
}

# away elo

#create results df
#get df structure
df_res_a <-df_res_h[1,]
#addin ELO place holders
df_res_a <- cbind(df_res_a, "aw_elo" = 0.0)
#remove row to get blank df
df_res_a <-df_res_a[0,]

# loop thru AW filtering on team adding in elo
for (k in 1:length(ls)) {
  #create the file ref
  fil <- paste0("E://R/Football/ELO/", ls[k], ".csv")
  
  if (file.exists(fil)) {
    #get the data
    df_fil <- read.csv2( file=fil, header = TRUE, sep=",")
    #convert dates
    df_fil$From <- as.Date( as.character(df_fil$From), "%Y-%m-%d")
    df_fil$To <- as.Date( as.character(df_fil$To), "%Y-%m-%d")
    #update col names to avoid sqldf join issues
    colnames(df_fil)[6] <- "fm"
    colnames(df_fil)[7] <- "ed"
    
    #get the data subset
    df_sub <- df_res_h %>% filter(Aw_Res_lk== ls[k])
    #join to get ELO, use SQl
    df_sub <- sqldf("select df_sub.* ,df_fil.elo 
                    from df_sub left outer join df_fil
                    where df_sub.Aw_Res_lk = df_fil.club
                    and df_sub.dt between df_fil.fm and df_fil.ed")
    colnames(df_sub)[9] <- "aw_elo"
    
    #append to Results
    df_res_a <- rbind(df_res_a, df_sub)
  }
}

# update the elo to numeric
df_res_a$hm_elo <- as.numeric(as.character(df_res_a$hm_elo))
df_res_a$aw_elo <- as.numeric(as.character(df_res_a$aw_elo))


#merge rds to schedule for home
df_home <- df_res_a %>% left_join(df_data_res, c("Hm_Res_lk"="HomeTeam", "dt"="Date"))

#remove 2009 season detail
#df_home <- subset(df_home, !is.na(df_home$aw_tm))

#plyr func mapvalues, used for the FRM calc
df_home$Res <-as.numeric(as.character(mapvalues(df_home$FTR, from=c("H", "D", "A"), to=c(1,0,-1))))

hm_form_3 <- function(Tm_ls) {
  frm <- data.frame(hm_Res_lk = character(),
                    dt = as.Date(character()),
                    Season = numeric(),
                    F3_AS= numeric(),
                    Frm3 = numeric(),
                    F3_Hm_elo = numeric(),
                    F3_Hm_elo_df = numeric()
                    )

  #make the distinct list of Seasons
  df_Se <- df_home %>% distinct(Season)
  
  for (h in 1:nrow(df_Se)) {
    df_Sea <- df_home %>% 
      select(Hm_Res_lk, dt, Season, AS, Res, hm_elo) %>%
      dplyr::filter(Season==df_Se[h,1]) %>% 
      arrange(dt)
  
        for (i in 1:length(Tm_ls)) {
          #for that team in that season, clac the elo lag dif
          df_frm <- df_Sea %>%  
            dplyr::filter(Hm_Res_lk==Tm_ls[i]) %>% 
            arrange(dt) %>% 
            #find the rd
            mutate(rw=rank(dt),
                   #lag to find the old elo
                   lag_hm=ifelse(rw<2, lag(hm_elo, 0, order_by=dt),
                                 (ifelse(rw<3,lag(hm_elo, 1, order_by=dt), 
                                         ifelse(rw<4,lag(hm_elo, 2, order_by=dt),
                                                lag(hm_elo, 3, order_by=dt))))), 
                   #get the elo dif
                   elo_df=hm_elo-lag_hm,
                   # the last 3 games AS 
                   F3_AS= ifelse(rw<2, AS,
                                 (ifelse(rw<3, rollmean(AS, 2, align = "right", fill = 0), 
                                         rollmean(AS, 3, align = "right", fill = 0)))),
                   # get last 3 games results
                   Frm3 = ifelse(rw<2, Res,
                                 (ifelse(rw<3, rollsum(Res, 2, align = "right", fill = 0), 
                                         rollsum(Res, 3, align = "right", fill = 0))))) %>% 
            select(-rw, -lag_hm)
          
        #append matrix 
          frm <- rbind(frm, df_frm)
        }
  }
  #add in home flag
  frm$H_A <- "H"
  return (frm)
}

# find the form for the home team
df_frm_hm <- hm_form_3(ls)
# append back to the home df
df_home <- df_home %>% left_join(df_frm_hm, c("Hm_Res_lk"="Hm_Res_lk", "dt"="dt")) 

#remove excess cols from merge
df_home <- df_home[,c(-10,-39:-42)]
#correct merge col names
colnames(df_home)[5] <- "Season"
colnames(df_home)[17] <- "AS"
colnames(df_home)[8] <- "hm_elo"
colnames(df_home)[37] <- "Res" 


#==================================================================================
# form AW - repeat hm calc
#==================================================================================

#merge rds to schedule for away
df_away <- df_res_a %>% left_join(df_data_res, c("Aw_Res_lk" = "AwayTeam", "dt"="Date"))

#plyr func mapvalues
df_away$Res <-as.numeric(as.character(mapvalues(df_away$FTR, from=c("H", "D", "A"), to=c(-1,0,1))))

Aw_form_3 <- function(Tm_ls) {
    aw_frm <- data.frame(Aw_Res_lk = character(),
                      dt = as.Date(character()),
                      Season = numeric(),
                      F3_AS= numeric(),
                      Frm3 = numeric(),
                      F3_Aw_elo = numeric(),
                      F3_Aw_elo_df = numeric()
    )
    
    #make the distinct list of Seasons
    df_Se <- df_away %>% distinct(Season)
    
    for (h in 1:nrow(df_Se)) {
      df_Sea <- df_away %>% 
        select(Aw_Res_lk, dt, Season, AS, Res, aw_elo) %>%
        dplyr::filter(Season==df_Se[h,1]) %>% 
        arrange(dt)
      
      for (i in 1:length(Tm_ls)) {
        #for that team in that season, clac the elo lag dif
        df_frm_aw <- df_Sea %>%  
          dplyr::filter(Aw_Res_lk==Tm_ls[i]) %>% 
          arrange(dt) %>% 
          #find the rd
          mutate(rw=rank(dt),
                 #lag to find the old elo
                 lag_Aw=ifelse(rw<2, lag(aw_elo, 0, order_by=dt),
                               (ifelse(rw<3,lag(aw_elo, 1, order_by=dt), 
                                       ifelse(rw<4,lag(aw_elo, 2, order_by=dt),
                                              lag(aw_elo, 3, order_by=dt))))), 
                 #get the elo dif
                 elo_df=aw_elo-lag_Aw,
                 # the last 3 games AS 
                 F3_AS= ifelse(rw<2, AS,
                               (ifelse(rw<3, rollmean(AS, 2, align = "right", fill = 0), 
                                       rollmean(AS, 3, align = "right", fill = 0)))),
                 # get last 3 games results
                 Frm3 = ifelse(rw<2, Res,
                               (ifelse(rw<3, rollsum(Res, 2, align = "right", fill = 0), 
                                       rollsum(Res, 3, align = "right", fill = 0))))) %>% 
          select(-rw, -lag_Aw)
        
        #append matrix 
        aw_frm <- rbind(aw_frm, df_frm_aw)
      }
    }

    #add in away flag
    aw_frm$H_A <- "A"
    return (aw_frm)
  }
  
  # find the form for the home team
  df_frm_aw <- Aw_form_3(ls)
  # append back to the home df
  df_away <- df_away %>% left_join(df_frm_aw, c("Aw_Res_lk"="Aw_Res_lk", "dt"="dt")) 
  
  #remove excess cols from merge
  df_away <- df_away[,c(-10, -39:-42)]
  #correct merge col names
  colnames(df_away)[5] <- "Season"
  colnames(df_away)[17] <- "AS"
  colnames(df_away)[9] <- "aw_elo"
  colnames(df_away)[37] <- "Res" 

#==================================================================================
# Season form
#==================================================================================

# join ladder to results for home team
df_Stg_H <- df_data_lad %>% inner_join(df_home, c("tm" = "hm_tm", "Rd"="i", "Year"="Season"))
#update the opp team col
colnames(df_Stg_H)[11] <- "Opp.Team" 
# clean up df
df_Stg_H <- df_Stg_H[,c( -13, -14, -17, -18 ) ]

#repeat for away detail
df_Stg_A <- df_data_lad %>% inner_join(df_away, c("tm" = "aw_tm", "Rd"="i", "Year"="Season"))
colnames(df_Stg_A)[11] <- "Opp.Team" 
df_Stg_A <- df_Stg_A[,c( -13, -14, -17, -18 ) ]

#merge both dfs together for complete listing
df_full <- rbind(df_Stg_H, df_Stg_A)

##########################################
#create the season form guide
season_form <- function() {
  sea_frm <- data.frame(Team = character(),
                    dt = as.Date(character()),
                    SE_FRM = numeric()
  )
  #make the distinct list of teams
  df_se_tm <- df_full %>% distinct(tm, Year)
  
  for (i in 1:nrow(df_se_tm)) {
    #make df with for querying
    df_sea_frm <- df_full %>%
      #using rd for ease, but should be data, i.e. 2010 spurs play toffies in rd1, in Jan?
      select(tm, Rd, Year, Res) %>%
      dplyr::filter(tm==df_se_tm[i,1], Year==df_se_tm[i,2] ) %>%
      arrange(Rd)
    #roll through each rd to calc season totals
    for (i in 1:38) {
      df_rd_frm <- df_sea_frm %>% filter(Rd<=i) %>% 
      mutate( SE_FRM = rollsum(Res, i, align = "right", fill = 0))
      
      #append to return df 
      sea_frm <- rbind(sea_frm, df_rd_frm[i,])
      
          }
  }
  #remove excess columns
  #sea_frm <- sea_frm[,-3:-19]
  return (sea_frm)
}
# find the form for the season
df_sea <- season_form()
#remove redundant cols for joining
df_sea <- df_sea[,-4]

#join back to full dataframe to complete the dataset.
df_full <- df_full %>% left_join(df_sea, c("tm" = "tm", "Year"="Year", "Rd"="Rd"))

#check file
write.csv2(df_full, file="E://R/Football/df_full.csv", row.names = FALSE)

#==================================================================================
# make model dataset
# - remove excess columns
# - create a lag for detail not know at the time of the match 
#==================================================================================
#check file
df_full <-read.csv2("df_full.csv", header =TRUE, sep=",", stringsAsFactors = FALSE) 

##############################################
# start here
##############################################

#remove excess columns
df_full <- df_full[, c(1:15, 20, 31:33, 40:45 )]

#reorder df for ease of function
df_full <- cbind(df_full[,c(1:12, 15, 17:19,24 )],df_full[,c(13:14,16,20:23,25 )] )

#current detail
head(df_full[, c(1:4, 31:39)])
#detail previous
head(df_full[,c(1:30,  41:75)])

## refactor for updated df 

#get the lagged values, based upon sets
lag_set <- function() {
  #create the results df
  df_lag_res <- df_full[0,c(1:30,  41:75)]
  
  #make the distinct list of teams
  df_lag_tm <- df_full %>% distinct(tm, Year)
  
  for (i in 1:nrow(df_lag_tm)) { 
    #filter the df with for each team/season 
    df_lag_frm <- df_full[,c(1:30,  41:75)] %>%
      dplyr::filter(tm==df_lag_tm[i,1], Year==df_lag_tm[i,2] ) %>%
      arrange(Rd)
    #break up the df & drop the last row
    df_lag_data <- df_lag_frm[-38,c(5:30,  41:75)]
    #generate a NA row
    lag_rw <- seq(0,61,1)
    lag_rw[] <- NA
    #create the lagged season view
    df_lag_data <- rbind(lag_rw, df_lag_data)
    
    #make complete df for Team/season
    df_lag_sub <- cbind((df_lag_frm[,c(1:4)]),df_lag_data )
    #add back on to results df
    df_lag_res <- rbind(df_lag_res, df_lag_sub)
  }
  #update col names
  colnames(df_lag_res)[4:65] <- paste0("Lg_", names(df_lag_res[,4:39]))
  return (df_lag_res)
}

# get the lagged detail for the modeling
df_lag <- lag_set()
#create the final model data set.
df_lag_full <- df_full[, c(1:4, 31:39)] %>% 
  join(df_lag, c( "Rd"="Rd", "tm" = "tm", "Year"="Year")) 

#check file
write.csv2(df_lag_full, file="E://R/Football/df_lag_full.csv", row.names = FALSE)

#create opp df
df_lag_opp <- df_lag_full[,c(1:10,22:57)]
#update column names
colnames(df_lag_opp) <- paste0("Opp_", names(df_lag_opp))
#join back for model view hm & away
#join back to full dataframe to complete the dataset.
df_model <- df_lag_full %>% left_join(df_lag_opp, c("Opp.Team" = "Opp_tm", "Year"="Opp_Year", "Rd"="Opp_Rd"))

#check file
write.csv(df_model, file="E://R/Football/df_model.csv", row.names = FALSE)

