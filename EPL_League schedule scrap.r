ref <- read_html("http://www.worldfootball.net/schedule/eng-premier-league-2011-2012-spieltag/1/")
# "http://www.worldfootball.net/schedule/eng-premier-league-2012-2013-spieltag/38/"

leagueRd <- ref %>% html_nodes(".standard_tabelle td:nth-child(6) a") %>% html_text()

leagueRd


tbody td
http://www.flashscore.com/soccer/england/premier-league-2010-2011/results/

#date
.wfb-ad+ .box td:nth-child(1)

# 1st team
.dunkel:nth-child(3) , .hell:nth-child(3) a

# 2nd team
.dunkel:nth-child(5) , .hell:nth-child(5) a

# box scores 
.standard_tabelle td:nth-child(6) a

# load packages 
x <- c("rvest","stringr")
lapply(x, require, character.only=TRUE)


############################################################################################  

league_schedule <- function(x="2010-2011") {
  # set the table position, not all col values are filled
  pos <-c(1:20)
  # create the blank data frame
  leagueschedule<- data.frame(round=as.factor(numeric()),
                              
                            hm_team=character(), 
                            aw_team=character(),
                            score=character(),
                            date=as.Date(character()))
                            
  # set a loop for the 38 rounds
  for (i in 1:38)  {
    # create the string page reference for each rd
    pg <- paste("http://www.worldfootball.net/schedule/eng-premier-league-", x , "-spieltag/",i, "/", sep="" )
    # read the page in
    ref <- read_html(pg)
    #date
    dt <- ref %>% html_nodes(".wfb-ad+ .box td:nth-child(1)") %>% html_text()
    dt<- dt[-c(11:20)]
    
    # 1st team
    hm_tm <- ref %>% html_nodes(".dunkel:nth-child(3) , .hell:nth-child(3) a") %>% html_text()
    # trim out URL if returned
    hm_tm <- lapply(hm_tm, function (x) if (substr(x, 0, 1)=="\r") 
      {substring((substring(x,22)), 0, (str_locate((substring(x,22)),"\r\n")[1,1])-1)} 
      else {x})
    
    # 2nd team
    aw_tm <- ref %>% html_nodes(".dunkel:nth-child(5) , .hell:nth-child(5) a") %>% html_text()
    # trim out URL if returned
    aw_tm <- lapply(aw_tm, function (x) if (substr(x, 0, 1)=="\r") 
    {substring((substring(x,22)), 0, (str_locate((substring(x,22)),"\r\n")[1,1])-1)} 
    else {x})
    
    
    # box scores 
    src <- ref %>% html_nodes(".standard_tabelle td:nth-child(6) a") %>% html_text()
    
    # bind all of the values into a single data frame
    rd_res <- as.data.frame(cbind(i, hm_tm, aw_tm, src, dt))
    
    # add the round detail to the league table
    leagueschedule <- rbind(leagueschedule, rd_res)
  }
  # return the league table
  leagueschedule
}

season <- c("2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016")  

league_sced <- function(s="2010-2011") {  
  
  for (i in 1:length(s)) {
    res_df <- league_schedule(s[i])
    res_df <-  data.frame(lapply(res_df, as.character), stringsAsFactors=FALSE)
    file_path <- paste("E://R/Football/Raw_data/", s[i], "_schedule.csv", sep="")
    
    write.csv(res_df, file=file_path)
  }
}

lapply(season, league_sced)


