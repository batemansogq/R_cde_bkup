
#install.packages(&quot;rvest&quot;)
library(rvest)
library(stringr)

# whole table
#     .box2+ .box td , th
# schedule
#.wfb-ad+ .box td


#page ref  
ref <- read_html("http://www.worldfootball.net/schedule/eng-premier-league-2011-2012-spieltag/1/")
# "http://www.worldfootball.net/schedule/eng-premier-league-2012-2013-spieltag/38/"

leagueRd <- ref %>% html_nodes(".box2+ .box td:nth-child(3)") %>% html_text()

leagueRd

# trim out the team name
# find the start
# substring(leagueRd[2],22)
# using the end text, extract the full team name
# substring(n, 0, (str_locate(n,"\r\n")[1,1])-1)
# single statement
# substring((substring(leagueRd[3],22)), 0, (str_locate((substring(leagueRd[3],22)),"\r\n")[1,1])-1)

#lappy 
lapply(leagueRd, function (x) substring((substring(x,22)), 0, (str_locate((substring(x,22)),"\r\n")[1,1])-1))



#position
  lg <- ".box2+ .box .standard_tabelle td:nth-child(1)"
#team
  tm <- ".box2+ .box td:nth-child(3)"
#matches
  mt <- ".box2+ .box td:nth-child(4)"
#wins
  wn <- ".box2+ .box td:nth-child(5)"
#draws
  dw <- ".box2+ .box td:nth-child(6)"
#losses
  ls <- ".box2+ .box td:nth-child(7)"
#for against
  fa <- ".box2+ .box td:nth-child(8)"
#dif
  df <- ".box2+ .box td:nth-child(9)"
#league points
  lp <- "td:nth-child(10)"

#league pos
  pos <-c(1:20)

############################################################################################  
    
league_table <- function(x) {
 # set the table position, not all col values are filled
  pos <-c(1:20)
  # create the blank data frame
  leaguetable <- data.frame(round=as.factor(numeric()),
                            position=as.factor(numeric()),
                            team=character(), 
                            matches=as.factor(numeric()),
                            win=numeric(),
                            draw=numeric(), 
                            loss=numeric(),
                            for_against=character(), 
                            goal_diff=numeric(),
                            points=numeric())
   # set a loop for the 38 rounds
for (i in 1:38)  {
  # create the string page reference for each rd
  pg <- paste("http://www.worldfootball.net/schedule/eng-premier-league-", x , "-spieltag/",i, "/", sep="" )
  # read the page in
  ref <- read_html(pg)
  # from the page, take the individual col ref
  #team
  tm <- ref %>% html_nodes(".box2+ .box td:nth-child(3)") %>% html_text()
  # extract the team name from the URL reference
  tm <- lapply(tm, function (x) substring((substring(x,22)), 0, (str_locate((substring(x,22)),"\r\n")[1,1])-1))
  
  #matches
  mt <- ref %>% html_nodes(".box2+ .box td:nth-child(4)") %>% html_text()
  #wins
  wn <- ref %>% html_nodes(".box2+ .box td:nth-child(5)") %>% html_text()
  #draws
  dw <- ref %>% html_nodes(".box2+ .box td:nth-child(6)") %>% html_text()
  #losses
  ls <- ref %>% html_nodes(".box2+ .box td:nth-child(7)") %>% html_text()
  #for against
  fa <- ref %>% html_nodes(".box2+ .box td:nth-child(8)") %>% html_text()
  #dif
  df <- ref %>% html_nodes(".box2+ .box td:nth-child(9)") %>% html_text()
  #league points
  lp <- ref %>% html_nodes("td:nth-child(10)") %>% html_text()
  
  # bind all of the values into a single data frame
  rd_res <- as.data.frame(cbind(i, pos,tm,mt,wn,dw,ls,fa,df,lp))
  
  # add the round detail to the league table
  leaguetable <- rbind(leaguetable, rd_res)
}
  # return the league table
 leaguetable
}


season <- c("2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016")  

league_hist <- function(s="2010-2011") {  

  for (i in 1:length(s)) {
  res_df <- league_table(s[i])
  res_df <-  data.frame(lapply(res_df, as.character), stringsAsFactors=FALSE)
  file_path <- paste("E://R/Football/Raw_data/", s[i], ".csv", sep="")
  
  write.csv(res_df, file=file_path)
  }
}

lapply(season, league_hist 
       
       