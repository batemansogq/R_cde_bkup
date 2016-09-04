#############################################################
# create the additional features
############################################################# 

# read in source data
setwd("E:/R/Football")
df_data_rds <- read.csv2( file="df_data_rds.csv", header = TRUE, sep=";")
df_data_lad <- read.csv2( file="df_data_lad.csv", header = TRUE, sep=";")
df_data_res <- read.csv2( file="df_data_res.csv", header = TRUE, sep=";")
ref_team <- read.csv2("FB_Ref_data_teams.csv", header =TRUE, sep=",")

# load packages 
x <- c("dplyr", "plyr", "zoo")
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

#merge rds to schedule for home
df_home <- df_data_rds %>% left_join(df_data_res, c("Hm_Res_lk"="HomeTeam", "dt"="Date"))

#remove 2009 season detail
#df_home <- subset(df_home, !is.na(df_home$aw_tm))

#plyr func mapvalues
df_home$Res <-as.numeric(as.character(mapvalues(df_home$FTR, from=c("H", "D", "A"), to=c(1,0,-1))))

hm_form_3 <- function() {
  frm <- data.frame(Team = character(),
                    dt = as.Date(character()),
                    F3_FTHG = numeric(),
                    F3_FTAG = numeric(),
                    F3_HTHG = numeric(),
                    F3_HTAG = numeric(),
                    F3_HS= numeric(),
                    F3_AS= numeric(),
                    F3_HST= numeric(),
                    F3_AST= numeric(),
                    F3_HF= numeric(),
                    F3_AF= numeric(),
                    F3_HC= numeric(),
                    F3_AC= numeric(),
                    F3_HY= numeric(),
                    F3_AY= numeric(),
                    F3_HR= numeric(),
                    F3_AR= numeric(),
                    Frm3 = numeric()
                    )
  #make the distinct list of teams
  df_tm <- df_home %>% distinct(hm_tm)
  
  for (i in 1:nrow(df_tm)) {
    #make df with for querying
    df_frm <- df_home %>% 
      select(hm_tm, dt,FTHG, FTAG, HTHG, HTAG, HS, AS, HST, AST, HF, AF, HC, 
             AC, HY, AY, HR, AR,Res) %>%
      dplyr::filter(hm_tm==df_tm[i,1]) %>% 
      arrange(dt) %>% 
      mutate(F3_FTHG = rollmean(FTHG, 3, align = "right", fill = 0),
             F3_FTAG = rollmean(FTAG, 3, align = "right", fill = 0),
             F3_HTHG = rollmean(HTHG, 3, align = "right", fill = 0),
             F3_HTAG = rollmean(HTAG, 3, align = "right", fill = 0),
             F3_HS= rollmean(HS, 3, align = "right", fill = 0),
             F3_AS= rollmean(AS, 3, align = "right", fill = 0),
             F3_HST= rollmean(HST, 3, align = "right", fill = 0),
             F3_AST= rollmean(AST, 3, align = "right", fill = 0),
             F3_HF= rollmean(HF, 3, align = "right", fill = 0),
             F3_AF= rollmean(AF, 3, align = "right", fill = 0),
             F3_HC= rollmean(HC, 3, align = "right", fill = 0),
             F3_AC= rollmean(AC, 3, align = "right", fill = 0),
             F3_HY= rollmean(HY, 3, align = "right", fill = 0),
             F3_AY= rollmean(AY, 3, align = "right", fill = 0),
             F3_HR= rollmean(HR, 3, align = "right", fill = 0),
             F3_AR= rollmean(AR, 3, align = "right", fill = 0),
            Frm3 = rollsum(Res, 3, align = "right", fill = 0))
    #correct for 1st & second row entries of the rolling calcs
    for (i in 1:16) {
      df_frm[1,(i+19)] <- df_frm[1, (i+2)]
      df_frm[2,(i+19)] <- mean(df_frm[1:2, (i+2)])
    }

    #correct for res which is a sum
    df_frm$Frm3[1] <- df_frm$Res[1]
    df_frm$Frm3[2] <- sum(df_frm$Res[1:2])
        
  #append matrix 
    frm <- rbind(frm, df_frm)
  }
  #remove excess columns
  frm <- frm[,-3:-19]
  #add in home flag
  frm$H_A <- "H"
  return (frm)
}
# find the form for the home team
df_frm <- hm_form_3()
# append back to the home df
df_home <- df_home %>% left_join(df_frm, c("hm_tm"="hm_tm", "dt"="dt")) 

#remove excess cols from merge
df_home <- df_home[,-8]


#==================================================================================
# form AW
#==================================================================================

#merge rds to schedule for away
df_away <- df_data_rds %>% left_join(df_data_res, c("Aw_Res_lk" = "AwayTeam", "dt"="Date"))

#remove 2009 season detail
#df_away <- subset(df_away, !is.na(df_away$aw_tm))

#plyr func mapvalues
df_away$Res <-as.numeric(as.character(mapvalues(df_away$FTR, from=c("H", "D", "A"), to=c(-1,0,1))))

aw_form_3 <- function() {
  aw_frm <- data.frame(Team = character(),
                    dt = as.Date(character()),
                    F3_FTHG = numeric(),
                    F3_FTAG = numeric(),
                    F3_HTHG = numeric(),
                    F3_HTAG = numeric(),
                    F3_HS= numeric(),
                    F3_AS= numeric(),
                    F3_HST= numeric(),
                    F3_AST= numeric(),
                    F3_HF= numeric(),
                    F3_AF= numeric(),
                    F3_HC= numeric(),
                    F3_AC= numeric(),
                    F3_HY= numeric(),
                    F3_AY= numeric(),
                    F3_HR= numeric(),
                    F3_AR= numeric(),
                    Frm3 = numeric()
  )
  #make the distinct list of teams
  df_tm <- df_away %>% distinct(aw_tm)
  
  for (i in 1:nrow(df_tm)) {
    #make df with for querying
    aw_df_frm <- df_away %>% 
      select(aw_tm, dt,FTHG, FTAG, HTHG, HTAG, HS, AS, HST, AST, HF, AF, HC, 
             AC, HY, AY, HR, AR,Res) %>%
      dplyr::filter(aw_tm==df_tm[i,1]) %>% 
      arrange(dt) %>% 
      mutate(F3_FTHG = rollmean(FTHG, 3, align = "right", fill = 0),
             F3_FTAG = rollmean(FTAG, 3, align = "right", fill = 0),
             F3_HTHG = rollmean(HTHG, 3, align = "right", fill = 0),
             F3_HTAG = rollmean(HTAG, 3, align = "right", fill = 0),
             F3_HS= rollmean(HS, 3, align = "right", fill = 0),
             F3_AS= rollmean(AS, 3, align = "right", fill = 0),
             F3_HST= rollmean(HST, 3, align = "right", fill = 0),
             F3_AST= rollmean(AST, 3, align = "right", fill = 0),
             F3_HF= rollmean(HF, 3, align = "right", fill = 0),
             F3_AF= rollmean(AF, 3, align = "right", fill = 0),
             F3_HC= rollmean(HC, 3, align = "right", fill = 0),
             F3_AC= rollmean(AC, 3, align = "right", fill = 0),
             F3_HY= rollmean(HY, 3, align = "right", fill = 0),
             F3_AY= rollmean(AY, 3, align = "right", fill = 0),
             F3_HR= rollmean(HR, 3, align = "right", fill = 0),
             F3_AR= rollmean(AR, 3, align = "right", fill = 0),
             Frm3 = rollsum(Res, 3, align = "right", fill = 0))
    #correct for 1st & second row entries of the rolling calcs
    for (i in 1:16) {
      aw_df_frm[1,(i+19)] <- aw_df_frm[1, (i+2)]
      aw_df_frm[2,(i+19)] <- mean(aw_df_frm[1:2, (i+2)])
    }
    
    #correct for res which is a sum
    aw_df_frm$Frm3[1] <- aw_df_frm$Res[1]
    aw_df_frm$Frm3[2] <- sum(aw_df_frm$Res[1:2])
    
    #append matrix 
    aw_frm <- rbind(aw_frm, aw_df_frm)
  }
  #remove excess columns
  aw_frm <- aw_frm[,-3:-19]
  #add in home flag
  aw_frm$H_A <- "A"
  return (aw_frm)
}
# find the form for the away team
df_frm <- aw_form_3()
# append back to the away df
df_away <- df_away %>% left_join(df_frm, c("aw_tm" = "aw_tm", "dt"="dt"))
#remove excess cols from merge
df_away <- df_away[,-8 ]
#==================================================================================
# Season form
#==================================================================================

# join ladder to results for home team
df_Stg_H <- df_data_lad %>% left_join(df_data_rds, c("tm" = "hm_tm", "Rd"="i", "Year"="Season"))
# remove away teams
df_Stg_H <- subset(df_Stg_H, !is.na(df_Stg_H$dt))
#update the opp team col
colnames(df_Stg_H)[11] <- "Opp.Team" 
# join to the home df to complete the view
df_Stg_H <- df_Stg_H %>% left_join(df_home, c("tm" = "hm_tm", "Rd"="i", "dt"="dt"))
# clean up df
df_Stg_H <- df_Stg_H[,c( -13:-18 ) ]

#repeat for away detail
df_Stg_A <- df_data_lad %>% left_join(df_data_rds, c("tm" = "aw_tm", "Rd"="i", "Year"="Season")) 
df_Stg_A <- subset(df_Stg_A, !is.na(df_Stg_A$dt))
colnames(df_Stg_A)[11] <- "Opp.Team" 
df_Stg_A <- df_Stg_A %>% left_join(df_away, c("tm" = "aw_tm", "Rd"="i", "dt"="dt"))
df_Stg_A <- df_Stg_A[,c( -13:-18 ) ]

#merge both dfs together for complete listing
df_full <- rbind(df_Stg_H, df_Stg_A)

#create the season form guide
season_form <- function() {
  sea_frm <- data.frame(Team = character(),
                    dt = as.Date(character()),
                    SE_FTHG = numeric(),
                    SE_FTAG = numeric(),
                    SE_HTHG = numeric(),
                    SE_HTAG = numeric(),
                    SE_HS= numeric(),
                    SE_AS= numeric(),
                    SE_HST= numeric(),
                    SE_AST= numeric(),
                    SE_HF= numeric(),
                    SE_AF= numeric(),
                    SE_HC= numeric(),
                    SE_AC= numeric(),
                    SE_HY= numeric(),
                    SE_AY= numeric(),
                    SE_HR= numeric(),
                    SE_AR= numeric(),
                    SE_FRM = numeric()
  )
  #make the distinct list of teams
  df_se_tm <- df_full %>% distinct(tm, Year)
  
  for (i in 1:nrow(df_se_tm)) {
    #make df with for querying
    df_sea_frm <- df_full %>%
      #using rd for ease, but should be data, i.e. 2010 spurs play toffies in rd1, in Jan?
      select(tm, Rd, Year, FTHG, FTAG, HTHG, HTAG, HS, AS, HST, AST, HF, AF, HC, 
             AC, HY, AY, HR, AR,Res) %>%
      dplyr::filter(tm==df_se_tm[i,1], Year==df_se_tm[i,2] ) %>%
      arrange(Rd)
    #roll through each rd to calc season totals
    for (i in 1:38) {
      df_rd_frm <- df_sea_frm %>% filter(Rd<=i) %>% 
      mutate(SE_FTHG = rollmean(FTHG, i, align = "right", fill = 0),
             SE_FTAG = rollmean(FTAG, i, align = "right", fill = 0),
             SE_HTHG = rollmean(HTHG, i, align = "right", fill = 0),
             SE_HTAG = rollmean(HTAG, i, align = "right", fill = 0),
             SE_HS= rollmean(HS, i, align = "right", fill = 0),
             SE_AS= rollmean(AS, i, align = "right", fill = 0),
             SE_HST= rollmean(HST, i, align = "right", fill = 0),
             SE_AST= rollmean(AST, i, align = "right", fill = 0),
             SE_HF= rollmean(HF, i, align = "right", fill = 0),
             SE_AF= rollmean(AF, i, align = "right", fill = 0),
             SE_HC= rollmean(HC, i, align = "right", fill = 0),
             SE_AC= rollmean(AC, i, align = "right", fill = 0),
             SE_HY= rollmean(HY, i, align = "right", fill = 0),
             SE_AY= rollmean(AY, i, align = "right", fill = 0),
             SE_HR= rollmean(HR, i, align = "right", fill = 0),
             SE_AR= rollmean(AR, i, align = "right", fill = 0),
             SE_FRM = rollsum(Res, i, align = "right", fill = 0))
      
      #append to return df 
      sea_frm <- rbind(sea_frm, df_rd_frm[i,])
      
          }
  }
  #remove excess columns
  #sea_frm <- sea_frm[,-3:-19]
  return (sea_frm)
}
# find the form for the home team
df_sea <- season_form()
#remove redundant cols for joining
df_sea <- df_sea[,c(-4:-20)]

#join back to full dataframe to complete the dataset.
df_full <- df_full %>% left_join(df_sea, c("tm" = "tm", "Year"="Year", "Rd"="Rd"))

#check file
write.csv(df_full, file="E://R/Football/df_full.csv", row.names = FALSE)

#==================================================================================
# make model hm and away detail
#==================================================================================
#reorder df for ease of function
df_full <- cbind(df_full[,c(1,3,11:12)],df_full[,c(-1,-3,-11:-12)] )

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

