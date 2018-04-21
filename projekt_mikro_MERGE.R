setwd("D:/Edukacja/1.Doktorat/MIKROEKONOMETRIA/database.sqlite")
# ZMIENIECIE wd pod siebie. 
#Musi tam byæ plik z baz¹ sqlite wtedy dziala wszystko


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

sd_match_clean<-sd_match[!is.na(sd_match$home_player_X1),]

head(sd_match_clean)



library(sqldf)
#ograniczenie zbioru danych
sd_match_def <- sqldf("SELECT id, country_id, league_id, season, stage, date as match_date, match_api_id, 
                      home_team_api_id, away_team_api_id, home_team_goal, away_team_goal, home_player_1,
                      goal, shoton, shotoff, foulcommit, card, cross, corner, possession
                      from sd_match")

away_result <- ifelse(sd_match_def$away_team_goal < sd_match_def$home_team_goal, "lost", 
                      ifelse(sd_match_def$away_team_goal > sd_match_def$home_team_goal, "won", "nil"))

home_result <- ifelse(sd_match_def$away_team_goal < sd_match_def$home_team_goal, "won", 
                      ifelse(sd_match_def$away_team_goal > sd_match_def$home_team_goal, "lost", "nil"))

sd_match_def <- cbind(sd_match_def, away_result, home_result)

remove(away_result)
remove(home_result)

sd_team_attr <- lDataFrames$Team_Attributes
sd_team_attr_def <- sqldf("SELECT team_fifa_api_id, team_api_id,                   
                          date, buildUpPlaySpeed, buildUpPlaySpeedClass,          
                          buildUpPlayDribbling, buildUpPlayDribblingClass, buildUpPlayPassing,             
                          buildUpPlayPassingClass, buildUpPlayPositioningClass, chanceCreationPassing, 
                          chanceCreationPassingClass, chanceCreationCrossing, chanceCreationCrossingClass, 
                          chanceCreationShooting, chanceCreationShootingClass, chanceCreationPositioningClass,
                          defencePressure, defencePressureClass, defenceAggression,              
                          defenceAggressionClass, defenceTeamWidth, defenceTeamWidthClass, 
                          defenceDefenderLineClass
                          from sd_team_attr")

sd_glm <- sqldf("
                SELECT id, 
                country_id, league_id, season, stage, match_date, match_api_id, home_result result,
                'home' as home_or_away, home_team_api_id as team_api_id, home_team_goal goals,
                goal, shoton, shotoff, foulcommit, card, cross, corner, possession
                from sd_match_def
                UNION ALL
                SELECT id, 
                country_id, league_id, season, stage, match_date, match_api_id, away_result result,
                'away' as home_or_away, away_team_api_id as team_api_id, away_team_goal goals,
                goal, shoton, shotoff, foulcommit, card, cross, corner, possession
                from sd_match_def
                ")


nrow(sd_glm) #51958
nrow(sd_team_attr_def) #1458

#£¥CZENIE Playerow

sd_player <- lDataFrames$Player

sd_fifa <- lDataFrames$Player_Attributes

sd_player1<- sqldf("
                   SELECT 
                      a.*,
                      b.player_fifa_api_id,
b.overall_rating, b.potential, b.preferred_foot, b.attacking_work_rate, b.defensive_work_rate, b.crossing,
b.finishing, b.heading_accuracy, b.short_passing, b.volleys, b.dribbling, b.curve, b.free_kick_accuracy, b.long_passing, 
b.ball_control, b.acceleration, b.sprint_speed, b.agility, b.reactions, b.balance, b.shot_power, b.jumping, b.stamina, b.strength,
b.long_shots, b.aggression, b.interceptions, b.positioning, b.vision, b.penalties, b.marking, b.standing_tackle, b.sliding_tackle,
b.gk_diving, b.gk_handling, b.gk_kicking, b.gk_positioning, b.gk_reflexes


                     from  sd_match_def as a
                   left join sd_fifa as b
                      on a.home_player_1=b.player_api_id
                   ")

unique(sd_player1$short_passing)

summary(sd_player1$short_passing)

sd_player2<- sqldf("
                   SELECT 
                   a.*,
                   b.player_fifa_api_id
                   from  sd_match_def as a
                   left join sd_player as b
                   on a.home_player_1=b.player_api_id
                   ")


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



sd_team_attr_def_dates <- sqldf("select a.*, b.date date_end
                                from sd_team_attr_def a
                                left join sd_team_attr_def b
                                on a.rank=b.rank-1 and a.team_api_id=b.team_api_id" 
)


sd_team_attr_def_dates$date_end[is.na(sd_team_attr_def_dates$date_end)] <- c("2020-12-31 00:00:00")
sd_team_attr_def_dates$date_end <- as.Date(sd_team_attr_def_dates$date_end)
sd_team_attr_def_dates$date <- as.Date(sd_team_attr_def_dates$date)
sd_glm$match_date <- as.Date(sd_glm$match_date)

data.class(sd_team_attr_def_dates$date_end)
data.class(sd_team_attr_def_dates$date)
data.class(sd_glm$match_date)


library(lubridate)
sd_team_attr_def_dates$date_end <- sd_team_attr_def_dates$date_end -days(1)



nrow(sd_team_attr_def_dates)
head(sd_team_attr_def_dates)
sd_team_attr_def_dates[, c("ID", "team_api_id", "date", "date_end")]

unique(sd_team_attr_def_dates[, c("team_api_id", "date", "date_end")])


library(xlsx)
write.xlsx(sd_team_attr_def_dates, "C:/Users/kostoj/Documents/Professional_documentation/EDUCATION/Mikroekonometria/db/aa.xlsx")

sd_glm_merged <- sqldf("select a.*, b.*
                       from sd_glm a
                       left join sd_team_attr_def_dates b
                       on a.team_api_id=b.team_api_id 
                       and a.match_date between b.date and b.date_end" )


write.csv(sd_glm_merged, "C:/Users/kostoj/Documents/Professional_documentation/EDUCATION/Mikroekonometria/db/aa.csv")


tail(sd_glm_merged)

nrow(sd_glm)
nrow(sd_glm_merged) ##usun¹æ duplikaty z sd_glm_merged

