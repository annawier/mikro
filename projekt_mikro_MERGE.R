setwd("D:/Edukacja/1.Doktorat/MIKROEKONOMETRIA/database.sqlite")
# ZMIENIECIE wd pod siebie. 
#Musi tam byæ plik z baz¹ sqlite wtedy dziala wszystko

rm(list=ls())

#Wczytanie Bazy SQLIte#####

library(RSQLite)
# Create an ephemeral RSQLite database
con <- dbConnect(RSQLite::SQLite(), dbname="database.sqlite")

dbListTables(con)
## list all tables
tables <- dbListTables(con)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

data.class(tables)

lDataFrames <- vector("list", length=length(tables))

data.class(lDataFrames)

## create a data.frame for each table
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}

names(lDataFrames)<-tables

lDataFrames$Match

#Tutaj bardzo luzne sprawdzanie mozecie usunaæ

unique(lDataFrames[["Match"]]["home_player_3"])
hist(lDataFrames[["Match"]]["shoton"])

summary(lDataFrames[["Match"]]["home_player_3"])
summary(lDataFrames[["Match"]]["shoton"])

lDataFrames$Match[lDataFrames[["Match"]]["id"]==1000,]





###KO


colnames(lDataFrames$Match)
#aby przejœæ do sql - osobne tabele
sd_match <- lDataFrames$Match

sd_match_def<-sd_match[!is.na(sd_match$home_player_X1),]

head(sd_match_def)

library(sqldf)
#ograniczenie zbioru danych


away_result <- ifelse(sd_match_def$away_team_goal < sd_match_def$home_team_goal, "lost", 
                      ifelse(sd_match_def$away_team_goal > sd_match_def$home_team_goal, "won", "nil"))

home_result <- ifelse(sd_match_def$away_team_goal < sd_match_def$home_team_goal, "won", 
                      ifelse(sd_match_def$away_team_goal > sd_match_def$home_team_goal, "lost", "nil"))

sd_match_def <- cbind(sd_match_def, away_result, home_result)

remove(away_result)
remove(home_result)

sd_team_attr <- lDataFrames$Team_Attributes
sd_team_attr_def<-sd_team_attr

sd_league <- lDataFrames$League

pozycja_1 <-sqldf("
                  SELECT
                  a.*,
                  b.name as league_name
                  from sd_match_def a
                  left join sd_league b
                  on a.league_id=b.id
                  ") 

head(pozycja_1)


sd_team <- lDataFrames$Team

pozycja_2HOME <-sqldf("
                      SELECT
                      a.*,
                      b.team_long_name as home_team_name
                      from pozycja_1 a
                      left join sd_team b
                      on a.home_team_api_id=b.team_api_id
                      ") 
head(pozycja_2HOME)

pozycja_2AWAY <-sqldf("
                      SELECT
                      a.*,
                      b.team_long_name as away_team_name
                      from pozycja_2HOME a
                      left join sd_team b
                      on a.away_team_api_id=b.team_api_id
                      ") 
head(pozycja_2AWAY)



sd_glm <- sqldf("
                SELECT id, 
                country_id, league_id, league_name, season, stage, date, match_api_id, home_result result,
                'home' as home_or_away, home_team_api_id as team_api_id, home_team_goal goals,
                goal, shoton, shotoff, foulcommit, card, cross, corner, possession, home_team_name as team_name, 
                home_player_X1 player_X1, home_player_X2 player_X2,home_player_X3 player_X3, home_player_X4 player_X4,
                home_player_X5 player_X5,home_player_X6 player_X6,home_player_X7 player_X7,home_player_X8 player_X8,
                home_player_X9 player_X9,home_player_X10 player_X10,home_player_X11 player_X11,
                home_player_Y1 player_Y1, home_player_Y2 player_Y2,home_player_Y3 player_Y3, home_player_Y4 player_Y4,
                home_player_Y5 player_Y5,home_player_Y6 player_Y6,home_player_Y7 player_Y7,home_player_Y8 player_Y8,
                home_player_Y9 player_Y9,home_player_Y10 player_Y10,home_player_Y11 player_Y11,
                home_player_1 player_1, home_player_2 player_2,home_player_3 player_3, home_player_4 player_4,
                home_player_5 player_5,home_player_6 player_6,home_player_7 player_7,home_player_8 player_8,
                home_player_9 player_9,home_player_10 player_10,home_player_11 player_11
from pozycja_2HOME
                UNION ALL
                SELECT id, 
                country_id, league_id, league_name, season, stage, date, match_api_id, away_result result,
                'away' as home_or_away, away_team_api_id as team_api_id, away_team_goal goals,
                goal, shoton, shotoff, foulcommit, card, cross, corner, possession, away_team_name as team_name,
                away_player_X1 player_X1, away_player_X2 player_X2,away_player_X3 player_X3, away_player_X4 player_X4,
                away_player_X5 player_X5,away_player_X6 player_X6,away_player_X7 player_X7,away_player_X8 player_X8,
                away_player_X9 player_X9,away_player_X10 player_X10,away_player_X11 player_X11,
                away_player_Y1 player_Y1, away_player_Y2 player_Y2,away_player_Y3 player_Y3, away_player_Y4 player_Y4,
                away_player_Y5 player_Y5,away_player_Y6 player_Y6,away_player_Y7 player_Y7,away_player_Y8 player_Y8,
                away_player_Y9 player_Y9,away_player_Y10 player_Y10,away_player_Y11 player_Y11,
                away_player_1 player_1, away_player_2 player_2,away_player_3 player_3, away_player_4 player_4,
                away_player_5 player_5,away_player_6 player_6,away_player_7 player_7,away_player_8 player_8,
                away_player_9 player_9,away_player_10 player_10,away_player_11 player_11
from pozycja_2AWAY
                ")

sd_player <- lDataFrames$Player

#pozycja_3<-sd_glm

#for(i in colnames(pozycja_3)[56:77]){
  
#  sql_content<-paste("SELECT a.*, b.player_name as ",i,"_NAME from pozycja_3 as a left join sd_player as b on a.",i,"=b.player_api_id", sep="")
  
  
  
#  pozycja_3 <-sqldf(sql_content)
  
#}

#head(pozycja_3)

nrow(sd_glm) #48316
nrow(sd_team_attr_def) #1458

#£¥CZENIE Playerow


sd_fifa <- lDataFrames$Player_Attributes
#nie ³¹czymy bo brak unikalnego identyfikatora
#sd_glm_2 <- merge(x=sd_glm, y=sd_team_attr_def, by="team_api_id", all.x=TRUE )
#nrow(sd_glm_2) #284549



library(dplyr)
library(plyr)
head(sd_team_attr_def)

sd_team_attr_def$ID <- seq.int(nrow(sd_team_attr_def))
sd_team_attr_def <- sd_team_attr_def[sd_team_attr_def$ID != '861',]


sd_team_attr_def<-transform(sd_team_attr_def, Rank = ave (date, team_api_id, 
                                                          FUN = function(x) rank (x, ties.method ="min")))

sd_team_attr_def$ID <- NULL

sd_team_attr_def_dates <- sqldf("select a.*, b.date date_end
                                from sd_team_attr_def a
                                left join sd_team_attr_def b
                                on a.rank=b.rank-1 and a.team_api_id=b.team_api_id" 
)


sd_team_attr_def_dates$date_end[is.na(sd_team_attr_def_dates$date_end)] <- c("2020-12-31 00:00:00")
sd_team_attr_def_dates$date_end <- as.Date(sd_team_attr_def_dates$date_end)
sd_team_attr_def_dates$date <- as.Date(sd_team_attr_def_dates$date)
sd_glm$date <- as.Date(sd_glm$date)

data.class(sd_team_attr_def_dates$date_end)
data.class(sd_team_attr_def_dates$date)
data.class(sd_glm$date)


library(lubridate)
sd_team_attr_def_dates$date_end <- sd_team_attr_def_dates$date_end -days(1)



nrow(sd_team_attr_def_dates)
head(sd_team_attr_def_dates)
sd_team_attr_def_dates[, c("id", "team_api_id", "date", "date_end")]

unique(sd_team_attr_def_dates[, c("team_api_id", "date", "date_end")])


#library(xlsx)
#write.xlsx(sd_team_attr_def_dates, "C:/Users/kostoj/Documents/Professional_documentation/EDUCATION/Mikroekonometria/db/aa.xlsx")

sd_glm_merged <- sqldf("select a.*, b.*
                       from sd_glm a
                       left join sd_team_attr_def_dates b
                       on a.team_api_id=b.team_api_id 
                       and a.date between b.date and b.date_end" )


#write.csv(sd_glm_merged, "C:/Users/kostoj/Documents/Professional_documentation/EDUCATION/Mikroekonometria/db/aa.csv")


tail(sd_glm_merged)

nrow(sd_glm)
nrow(sd_glm_merged) ##usun¹æ duplikaty z sd_glm_merged

#Trik Kariny

head(sd_fifa)

sd_fifa<-transform(sd_fifa, Rank = ave (date, player_api_id, 
                                                          FUN = function(x) rank (x, ties.method ="min")))


sd_fifa_dates <- sqldf("select a.*, b.date date_end
                                from sd_fifa a
                                left join sd_fifa b
                                on a.rank=b.rank-1 and a.player_api_id=b.player_api_id" 
)


sd_fifa_dates$date_end[is.na(sd_fifa_dates$date_end)] <- c("2020-12-31 00:00:00")
sd_fifa_dates$date_end <- as.Date(sd_fifa_dates$date_end)
sd_fifa_dates$date <- as.Date(sd_fifa_dates$date)
#sd_glm$match_date <- as.Date(sd_glm$match_date)

data.class(sd_fifa_dates$date_end)
data.class(sd_fifa_dates$date)
#data.class(sd_glm$match_date)


library(lubridate)
sd_fifa_dates$date_end <- sd_fifa_dates$date_end -days(1)

#sd_fifa_dates[, c("id", "player_api_id", "date", "date_end")]
#unique(sd_fifa_dates[, c("player_api_id", "date", "date_end")])

skills<-c('overall_rating','long_passing','acceleration','sprint_speed','agility','jumping','strength','interceptions','positioning','marking','standing_tackle','sliding_tackle','short_passing','long_shots')

sd_glm_merged$ID<-NULL

sd_glm_merged[,c(-56,-57)]->sd_glm_merged

colnames(sd_glm_merged)

sd_glm_merged_fifa<-sd_glm_merged



for(i in c("player_2","player_3","player_4","player_5")){
  sql_content<-c()
    for(j in skills){

      
sql_content<-paste(sql_content,",b.",j," as ",i,"_",j, sep="")


  }
  
  sql_content1<-paste("select a.*",sql_content," from sd_glm_merged_fifa a left join sd_fifa_dates b on a.",i,"=b.player_api_id and a.date between b.date and b.date_end",sep="")
  sd_glm_merged_fifa <-sqldf(sql_content1)
}

#FINALNA
sd_glm_merged_fifa 


#ANALIZA POZYCJI #####

#Laczenie nazw lig
#£¹cznienie nazw zespo³ów
#Laczenie nazw graczy
#Filtrowanie kilk najlepszych teamów i wybranie kilku meczu swiezych i check linupoów


sd_league <- lDataFrames$League


pozycja_1 <-sqldf("
                  SELECT
                  a.*,
                  b.name as league_name
                  from sd_glm a
                  left join sd_league b
                  on a.league_id=b.id
                  ") 

head(pozycja_1)


sd_team <- lDataFrames$Team

pozycja_2HOME <-sqldf("
                      SELECT
                      a.*,
                      b.team_long_name as home_team_name
                      from pozycja_1 a
                      left join sd_team b
                      on a.home_team_api_id=b.team_api_id
                      ") 
head(pozycja_2HOME)

pozycja_2AWAY <-sqldf("
                      SELECT
                      a.*,
                      b.team_long_name as away_team_name
                      from pozycja_2HOME a
                      left join sd_team b
                      on a.away_team_api_id=b.team_api_id
                      ") 
head(pozycja_2AWAY)

sd_player <- lDataFrames$Player

pozycja_3<-pozycja_2AWAY

for(i in colnames(pozycja_2AWAY)[56:77]){
  
  sql_content<-paste("SELECT a.*, b.player_name as ",i,"_NAME from pozycja_3 as a left join sd_player as b on a.",i,"=b.player_api_id", sep="")
  
  
  
  pozycja_3 <-sqldf(sql_content)
  
}

head(pozycja_3)

pozycja_3[pozycja_3$home_team_name %in% c("Real Madrid CF","Arsenal") & pozycja_3$away_team_name %in% c("Real Madrid CF","Arsenal")
          ,]
unique(sd_team$team_long_name)

sd_fifa <- lDataFrames$Player_Attributes
