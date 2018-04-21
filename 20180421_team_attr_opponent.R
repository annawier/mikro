setwd("C:/Users/kostoj/Documents/Professional_documentation/EDUCATION/Mikroekonometria/db")
# ZMIENIECIE wd pod siebie. 
#Musi tam bya plik z baz1 sqlite wtedy dziala wszystko


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

#Tutaj bardzo luzne sprawdzanie mozecie usunaa

unique(lDataFrames[["Match"]]["home_player_3"])
hist(lDataFrames[["Match"]]["shoton"])

summary(lDataFrames[["Match"]]["home_player_3"])
summary(lDataFrames[["Match"]]["shoton"])

lDataFrames$Match[lDataFrames[["Match"]]["id"]==1000,]





###KO


colnames(lDataFrames$Match)
#aby przejoa do sql - osobne tabele
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
                goal, shoton, shotoff, foulcommit, card, cross, corner, possession, away_team_api_id opponent_team_api_id
                from sd_match_def
                UNION ALL
                SELECT id, 
                country_id, league_id, season, stage, match_date, match_api_id, away_result result,
                'away' as home_or_away, away_team_api_id as team_api_id, away_team_goal goals,
                goal, shoton, shotoff, foulcommit, card, cross, corner, possession, home_team_api_id opponent_team_api_id
                from sd_match_def
                ")


nrow(sd_glm) #51958
nrow(sd_team_attr_def) #1458

#LYCZENIE Playerow

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


#nie 31czymy bo brak unikalnego identyfikatora
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


#library(xlsx)
#write.xlsx(sd_team_attr_def_dates, "C:/Users/kostoj/Documents/Professional_documentation/EDUCATION/Mikroekonometria/db/aa.xlsx")

sd_glm_merged <- sqldf("select a.*, b.*
                       from sd_glm a
                       left join sd_team_attr_def_dates b
                       on a.team_api_id=b.team_api_id 
                       and a.match_date between b.date and b.date_end")



sd_glm_merged <- sqldf("select a.*, 
b.date, b.date_end, 
b.buildUpPlaySpeed, b.buildUpPlaySpeedClass, b.buildUpPlayDribbling,
b.buildUpPlayDribblingClass, b.buildUpPlayPassing, b.buildUpPlayPassingClass,       
b.buildUpPlayPositioningClass, b.chanceCreationPassing, b.chanceCreationPassingClass,    
b.chanceCreationCrossing, b.chanceCreationCrossingClass, b.chanceCreationShooting,        
b.chanceCreationShootingClass, b.chanceCreationPositioningClass, b.defencePressure,               
b.defencePressureClass, b.defenceAggression, b.defenceAggressionClass,        
b.defenceTeamWidth, b.defenceTeamWidthClass, b.defenceDefenderLineClass
from sd_glm a
left join sd_team_attr_def_dates b
on a.team_api_id=b.team_api_id 
and a.match_date between b.date and b.date_end")


summary(sd_glm)
summary(sd_team_attr_def_dates)
summary(sd_glm_merged)

#Sekcja zmienne oponenta ####
sd_glm_merged_2 <- sqldf("select 
a.id, a.country_id, a.league_id, a.season, a.stage, a.match_date,
                         a.match_api_id, a.result, a.home_or_away, a.team_api_id, a.goals,
                         a.goal, a.shoton, a.shotoff, a.foulcommit, a.card, a.cross, a.corner,
                         a.possession, a.opponent_team_api_id,
                         a.date, a.buildUpPlaySpeed, a.buildUpPlaySpeedClass, a.buildUpPlayDribbling, 
                         a.buildUpPlayDribblingClass, a.buildUpPlayPassing, a.buildUpPlayPassingClass,
                         a.buildUpPlayPositioningClass, a.chanceCreationPassing, a.chanceCreationPassingClass,
                         a.chanceCreationCrossing, a.chanceCreationCrossingClass, a.chanceCreationShooting, 
                         a.chanceCreationShootingClass, a.chanceCreationPositioningClass, a.defencePressure,
                         a.defencePressureClass, a.defenceAggression, a.defenceAggressionClass, a.defenceTeamWidth,
                         a.defenceTeamWidthClass, a.defenceDefenderLineClass,
                         c.buildUpPlaySpeedClass as O_buildUpPlaySpeedClass,
                         c.buildUpPlayDribblingClass as O_buildUpPlayDribblingClass,
                        c.buildUpPlayPassingClass as O_buildUpPlayPassingClass,
                        c.buildUpPlayPositioningClass as O_buildUpPlayPositioningClass,
                         c.chanceCreationPassingClass as O_chanceCreationPassingClass,
                         c.chanceCreationCrossingClass as O_chanceCreationCrossingClass,
                         c.chanceCreationShootingClass as O_chanceCreationShootingClass,
                         c.chanceCreationPositioningClass as O_chanceCreationPositioningClass,
                         c.defencePressureClass as O_defencePressureClass,
                         c.defenceAggressionClass as O_defenceAggressionClass,
                         c.defenceTeamWidthClass as O_defenceTeamWidthClass,
                         c.defenceDefenderLineClass as O_defenceDefenderLineClass
                         from sd_glm_merged a
                         left join sd_team_attr_def_dates c
                         on a.opponent_team_api_id=c.team_api_id 
                         and a.match_date between c.date and c.date_end")

#####

# Sekcja CONCAT zmienna dru¿yny vs zmienna oponenta####

sd_glm_merged_3 <- sd_glm_merged_2

sd_glm_merged_3$O_buildUpPlaySpeedClass <- paste(sd_glm_merged_3$buildUpPlaySpeedClass,sd_glm_merged_3$O_buildUpPlaySpeedClass)
sd_glm_merged_3$O_buildUpPlayDribblingClass <- paste(sd_glm_merged_3$buildUpPlayDribblingClass,sd_glm_merged_3$O_buildUpPlayDribblingClass)
sd_glm_merged_3$O_buildUpPlayPassingClass <- paste(sd_glm_merged_3$buildUpPlayPassingClass,sd_glm_merged_3$O_buildUpPlayPassingClass)
sd_glm_merged_3$O_buildUpPlayPositioningClass <- paste(sd_glm_merged_3$buildUpPlayPositioningClass,sd_glm_merged_3$O_buildUpPlayPositioningClass)
sd_glm_merged_3$O_chanceCreationPassingClass <- paste(sd_glm_merged_3$chanceCreationPassingClass,sd_glm_merged_3$O_chanceCreationPassingClass)
sd_glm_merged_3$O_chanceCreationCrossingClass <- paste(sd_glm_merged_3$chanceCreationCrossingClass,sd_glm_merged_3$O_chanceCreationCrossingClass)
sd_glm_merged_3$O_chanceCreationShootingClass <- paste(sd_glm_merged_3$chanceCreationShootingClass,sd_glm_merged_3$O_chanceCreationShootingClass)
sd_glm_merged_3$O_chanceCreationPositioningClass <- paste(sd_glm_merged_3$chanceCreationPositioningClass,sd_glm_merged_3$O_chanceCreationPositioningClass)
sd_glm_merged_3$O_defencePressureClass <- paste(sd_glm_merged_3$defencePressureClass,sd_glm_merged_3$O_defencePressureClass)
sd_glm_merged_3$O_defenceAggressionClass <- paste(sd_glm_merged_3$defenceAggressionClass,sd_glm_merged_3$O_defenceAggressionClass)
sd_glm_merged_3$O_defenceTeamWidthClass <- paste(sd_glm_merged_3$defenceTeamWidthClass,sd_glm_merged_3$O_defenceTeamWidthClass)
sd_glm_merged_3$O_defenceDefenderLineClass <- paste(sd_glm_merged_3$defenceDefenderLineClass,sd_glm_merged_3$O_defenceDefenderLineClass)

tail(sd_glm_merged_3)


#####OPIS BAZ GLM 
#sd_glm_merged - atrybuty druzyn
#sd_glm_merged_2 - atrybuty druzyn + atrybuty odpowiednich druzyn przeciwnych (tutaj mozna zrobic model z interakcja)
#sd_glm_merged_3 - atrybuty druzyn + concat(atrybuty druzyn, atrybuty odpowiednich druzyn przeciwnych)

####