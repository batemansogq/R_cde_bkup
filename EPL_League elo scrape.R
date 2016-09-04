#############################################################
# the elo data from the API source
############################################################# 

# load packages 
x <- c("dplyr", "httr", "stringr", "sqldf", "zoo")
lapply(x, require, character.only=TRUE)

# read in source data
setwd("E:/R/Football")
df_data_rds <- read.csv2( file="df_data_rds.csv", header = TRUE, sep=";")
ref_team <- read.csv2("FB_Ref_data_teams.csv", header =TRUE, sep=",", stringsAsFactors = FALSE)


# create the team listing for the api call
ls <- ref_team[,3]

for (i in 1:length(ls)) {
  #find file path
  fil <- paste0("E://R/Football/ELO/", ls[i], ".csv")
  #trap from file existing already
  if (!file.exists(fil)) {
  #write api ref
  api <- paste0("http://api.clubelo.com/", gsub(" ","",ls[i]))
  #get the data
  sou_api <- GET(api)
  # check response
  if (http_error(sou_api)==FALSE) {
  #conform the data into a df
  sou_dat <- as.data.frame(content(sou_api))
  #create file name
  nam <- paste0("E://R/Football/ELO/", ls[i], ".csv")
  #write file
  write.csv(sou_dat, file=nam, row.names = FALSE)
  }
  }
}

###################################################################
# join up to schedule 
###################################################################


# join ref to schedule for join
#merge rounds with ref for joining
df_data_rds <- df_data_rds %>% left_join(ref_team, c("hm_tm" = "Ladder")) %>% select(-Rounds)
#upd column name
colnames(df_data_rds)[6] <- "hm_lk"
#make factor for joins
df_data_rds[,"hm_lk"] <- as.factor(df_data_rds[,"hm_lk"])
#convert dates for join
df_data_rds$dt <- as.Date( as.character(df_data_rds$dt), "%Y-%m-%d")

df_data_rds <- df_data_rds %>% left_join(ref_team, c("aw_tm" = "Ladder")) %>% select(-Rounds)
colnames(df_data_rds)[7] <- "aw_lk"
df_data_rds[,"aw_lk"] <- as.factor(df_data_rds[,"aw_lk"])

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
    df_sub <- df_data_rds %>% filter(hm_lk== ls[j])
    #join to get ELO, use SQl
    df_sub <- sqldf("select df_sub.* ,df_fil.elo 
                    from df_sub left outer join df_fil
                    where df_sub.hm_lk = df_fil.club
                    and df_sub.dt between df_fil.fm and df_fil.ed")
    colnames(df_sub)[8] <- "hm_elo"
    
    #append to Results
    df_res_h <- rbind(df_res_h, df_sub)
    }
}

# write out result
write.csv(df_res_h, file="E://R/Football/df_data_h.csv", row.names = FALSE)

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
    df_sub <- df_res_h %>% filter(aw_lk== ls[k])
    #join to get ELO, use SQl
    df_sub <- sqldf("select df_sub.* ,df_fil.elo 
                    from df_sub left outer join df_fil
                    where df_sub.aw_lk = df_fil.club
                    and df_sub.dt between df_fil.fm and df_fil.ed")
    colnames(df_sub)[9] <- "aw_elo"
    
    #append to Results
    df_res_a <- rbind(df_res_a, df_sub)
  }
}
# write out result
# write.csv(df_res_a, file="E://R/Football/df_data_elo.csv", row.names = FALSE)

##########################################################################
# add to features build
############################################################################

#load in the modeling data
setwd("E:/R/Football")
df_model <- read.csv2("df_model.csv", header =TRUE, sep=",", stringsAsFactors = FALSE)

#addin ELO numbers
df_model_UPD <- sqldf("select df_model.* ,df_res_a.hm_elo,df_res_a.aw_elo  
        from df_model left outer join df_res_a
      where df_model.Rd = df_res_a.i
        and df_model.Year = df_res_a.Season
        and df_model.Tm = df_res_a.hm_tm")

df_model_UPD$hm_elo <- as.numeric(as.character(df_model_UPD$hm_elo))
df_model_UPD$aw_elo <- as.numeric(as.character(df_model_UPD$aw_elo))

#remove excessive columns
df_model_UPD <- df_model_UPD[,c(1:16,22,33:34, 51, 57:59, 65, 76:77,94:96)]
# write out result
write.csv(df_model_UPD, file="E://R/Football/df_model_elo.csv", row.names = FALSE)

##########################################################################
# use zoo to create a form of ELO
############################################################################

#load in the modeling data
df_model_UPD <-read.csv2("E://R/Football/df_model_elo.csv", header =TRUE, sep=",", stringsAsFactors = FALSE)
