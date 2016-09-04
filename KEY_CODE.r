# load require data man packages
	x <- c("plyr", "dplyr", "data.table", "caret")
	lapply(x, require, character.only = TRUE)

#unload a package without restart
	detach("package:caret", unload = TRUE)
	
# suppress the package load messaging
	suppressPackageStartupMessages(library(googleVis))
	
# install a github library	
	install.packages("devtools", dependencies = TRUE)
	suppressPackageStartupMessages(library("devtools"))
	install_github("ropensci/plotly")
	
#clear the workspace
rm(list=ls())

# create an empty list
rm <- as.list(NULL)

###########################################################################################
# get data in
########################################################################################### 

# delimited file
act_lab <- read.delim(act_lab_sou, header=FALSE, sep=" ", col.names = c("ID", "Desc"))
# fixed width
fl_lab <- read.fwf(fl_lab_sou, widths=c(1), col.names = c("ID"))
# csv, remove all null values
url <- "E://R/Cousera/practical-machine-learning/Project/pml-training.csv"
t1 <- read.csv(url, header=TRUE, sep=",", na.strings = c('','NA','#DIV/0!'))

# db 
	# set up a src that connects to the mysql database (src_mysql is provided by dplyr)
	my_db <- src_mysql(dbname = "dplyr",                       
					  host = "dplyr.csrrinzqubik.us-east-1.rds.amazonaws.com", 
					  port = 3306, user = "dplyr", password = "dplyr")                 
	# and reference a table within that src
	nycflights <- tbl(my_db, "dplyr") 

#check to see if ile exists, pull data from a API, run response check	
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

###########################################################################################
# data manipulation
########################################################################################### 

#create the data time column
	B$DateTime <- as.POSIXct(paste(B$Date, B$Time), format="%d/%m/%Y %H:%M:%S")
#convert Date column to a date
	B$Date <- as.Date( as.character(B$Date), "%d/%m/%Y")
	
# paste elements togther without the space
	paste("http://www.worldfootball.net/",1, "/", sep="" )
	
# create an empty DF
	frm <- data.frame(Team = character(),
	                  dt = as.Date(character()),
	                  Frm3 = character())

# subset data frames
	#subset the data 2 days
	GP <- subset(B, Date==as.Date("2007-02-01") | Date==as.Date("2007-02-02") )

	# subset the dataframe with a search based on name
	flt <-  filter(ft_lab, grepl('mean()|std()', Desc))
	df2 <- fl_lab[,flt$Desc, with=FALSE]
	
	#subset the df based upon the col function
	t1.upd <- t1[, -which(names(t1) %in% chk(t1))]
	# remove 1st 7 columns, as not rel to modeling
	t1.upd <- t1.upd[, -c(1:7)]

						
# update each measure column title to include prefix of Avg
	names(df_tidy) <- c(names(df_tidy[1:2]),paste("Avg_", names(df_tidy[3:81]), sep=""))

# update a ind column name in a df
	colnames(df_Stg_A)[11] <- "Opp.Team"
# many col names	
	colnames(df_lag_res)[4:38] <- paste0("Lg_", names(df_lag_res[,4:38]))

# join data together on key
# combine the test set and labels together into single data frame.
df1 <- arrange(join(fl_lab, act_lab), ID)

#merge data together without dplyr
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

#merge on many columns
df_home <- merge(df_data_rds, df_data_res, by.x = c("Hm_Res_lk", "dt"), 
                 by.y = c("HomeTeam", "Date"), all.x = TRUE)

#generate a set of numbers
labCol <- seq(0, 100, 1)
# replace all with NA, except mutliples of 5
labCol[labCol %% 5 != 0] <- NA

#==============================================================================
#table function
#basic summary table, 2 dim with count of obs
table(mvt$Year, mvt$Arrest)

#subset table
table(mvt$Year, mvt$Arrest)[1,2]

# subset a table and calc
sum(table(CPS$Citizenship)[c(1,3)])/nrow(CPS)

# table with calc
table(CPS$State, is.na(CPS$MetroAreaCode))
table(CPS$Region, is.na(CPS$MetroAreaCode))[4,2]/table(CPS$Region)[4]

# use tapply with a calc to establish a percentage
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))
#==============================================================================

#dplyr example
	hflights %>%
	  mutate(RealTime = ActualElapsedTime + 100, mph = Distance / RealTime * 60) %>%
	  filter(mph < 105 | Cancelled == 1 | Diverted == 1) %>%
	  summarise(n_non = n(), 
				p_non = n_non / nrow(hflights) * 100, 
				n_dest = n_distinct(Dest), 
				min_dist = min (Distance), 
				max_dist = max(Distance))
			
# complex dplyr example
	popHealthDF <- sou %>%
	  select(BGN_DATE, EVTYPE, FATALITIES, INJURIES)%>% 
	  filter(FATALITIES>0 | INJURIES>0 )%>%
	  mutate(Year=as.numeric(format(as.Date(BGN_DATE, "%m/%d/%Y %T"), "%Y"))) %>% 
	  group_by(EVTYPE)%>% 
	  summarise(tot.fatalities=sum(FATALITIES), 
				tot.injuries=sum(INJURIES),
				tot.years = n_distinct(Year),
				tot = sum(FATALITIES + INJURIES),
				avg.impact = tot/tot.years)%>%
	  ungroup()%>%
	  mutate(rank.impact = dense_rank(desc(tot)),
			 rank.avg.impact = dense_rank(desc(avg.impact)) ) %>%
	  #sort a column
	  arrange( rank.impact )
	
#dplyr multi column join
	df_Stg_H <- df_data_lad %>% left_join(df_data_rds, c("tm" = "hm_tm", "Rd"="i", "Year"="Year"))
	
#use sqldf to do a between on dates
	df_sub <- sqldf("select df_sub.* ,df_fil.elo 
                    from df_sub left outer join df_fil
	                where df_sub.aw_lk = df_fil.club
	                and df_sub.dt between df_fil.fm and df_fil.ed")

# If you want your results printed to the console, add
# print to the end of your chain.
	cran %>%
	  select(ip_id, country, package, size) %>%
	  mutate(size_mb = size / 2^20) %>%
	  filter(size_mb <= 0.5) %>%
	  #sort a column
	  arrange(desc(size_mb)) %>% 
	  print()
	
#make a distinct list
	tm_list <- df_data_res %>% select(HomeTeam) %>% distinct(HomeTeam)
	
# use dyplr & lag to get earlier values, use if to adjust lag based on row number, for the first 3 rows of the df
	x <- y %>% group_by(i) %>% mutate(rw=rank(i), 
	                                  lag_hm=ifelse(rw<2, lag(hm_elo, 0, order_by=i),
	                                                (ifelse(rw<3,lag(hm_elo, 1, order_by=i), 
	                                                        ifelse(rw<4,lag(hm_elo, 2, order_by=i),lag(hm_elo, 3, order_by=i))))))
	
# use zoo for rolling sum of previous period
	df_frm <- df_away %>% 
	  select(aw_tm, dt, Res) %>%
	  dplyr::filter(aw_tm==df_tm[i,1]) %>% 
	  #zoo function
	  mutate(Frm3 = rollsum(Res, 3, align = "right", fill = 0))
	
# use DataCombine function slide to get previous period and update name
	for (j in 1:(ncol(df_lag_frm)-3)) {
	  df_lag_col <- df_lag_frm[,c(1:2, j+3) ] %>% 
	    slide(, Var = names(df_lag_frm[j+3]), NewVar=paste0("Lg_",df_lag_frm[j+3]), slideBy = -1)
	}
	
# create a matrix
	mdat <- matrix(round(rnorm(20),3), nrow = 4, ncol = 5, byrow = TRUE,
	               dimnames = list(c("r1", "r2", "r3", "r4"),
	                               c("C1", "C2", "C3", "c4", "c5")))
# seek the diag
	diag(mdat)
	
#subset a data frame and stop the conversion
	mdat[,2, drop=FALSE]
	
# if statement basic
	x <- -5
	if(x > 0){
	  print("Non-negative number")
	} else {
	  print("Negative number")
	}
	
# nested if
	x <- 0
	if (x < 0) {
	  print("Negative number")
	} else if (x > 0) {
	  print("Positive number")
	} else
	  print("Zero")

#============
# file check before process
	
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
	  df_sub <- df_res_h %>% filter(aw_lk== ls[i])
	  #join to get ELO, use SQl
	  df_sub <- sqldf("select a.* ,b.elo from df_sub a, df_fil b
                    where a.dt between b.fm and b.ed")
	  colnames(df_sub)[8] <- "aw_elo"
	  
	  #append to Results
	  df_res_a <- rbind(df_res_a, df_sub)
	}
	
	
###########################################################################################
# send data out
########################################################################################### 

# export file to working directory
	write.table(df_tidy, file="./q5_tidy_data.txt", sep=",", row.names = FALSE)

# standard plot out
	#set the output png
	png(file="./plot2.png" , width = 480, height = 480, units = "px")
	#create the plot
	plot(GP$DateTime, GP$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
	#shut the png down
	dev.off()

# ggplot out
	# set the output png
	png(file="./plot3.png" , width = 480, height = 480, units = "px")
	# create a plot
	ggplot(GP.Sum, aes(x=year, y=Emissions_Totals, colour=type)) +
	  geom_line() +
	  ggtitle("Type of PM2.5 Emissions in Baltimore City, Maryland")

	# shut the png creation down
	dev.off()

###########################################################################################
# symbol bubble chart with data labels
########################################################################################### 
# bubble chart with symbols
	ph2rd <- sqrt( popHealthGH2$avg.impact/ pi )
	symbols(popHealthGH2$avg.impact,  popHealthGH2$tot.years, circles=ph2rd, 
			inches=0.35, fg="white", bg="light green",
			xlab="Average Count of Fatalities/Injuries", 
			ylab=" ",
			ylim = c(0,80))
	text(popHealthGH2$avg.impact,  popHealthGH2$tot.years, popHealthGH2$EVTYPE, cex=0.5)

	
	ggplot(grp, aes(x=ProbDeath, y=ProbSurv, size=DeathCnt)) + 
	  geom_point(shape=21, colour="black", fill= "green") +
	  scale_size_area(max_size=15, guide=FALSE) +
	  ylim(0,1)+
	  xlim(0,1)	
	
	# great plotting 
	# http://www.r-bloggers.com/15-questions-all-r-users-have-about-plots/

###########################################################################################
# load swirl
########################################################################################### 
	# load swirl
	library(swirl)
	install_from_swirl("Statistical Inference")

	swirl()

###########################################################################################
# basic function
########################################################################################### 
# basic function
	corr <- function(directory, threshold = 0) {

	  x <- data.frame("Date"=str(0), "sulfate"=numeric(0), "nitrate"=numeric(0),"ID"=numeric(0) )
	  z <- numeric()
	  
	  for (i in 1:332) {
		y <-read.csv(paste(directory,'/',formatC(i, width=3, flag="0"), '.csv',sep=""))
		x <- subset(y, complete.cases(y))
		  if (nrow(x)>threshold) {
			z <- c(z, cor(x$sulfate, x$nitrate) )
		  } 
	  }
	  z
	}