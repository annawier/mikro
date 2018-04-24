#########################################################################################
# TESTY c.d.
#########################################################################################

for(i in team_attr_list) {
  sd_glm_merged_2[[i]] <- as.factor(sd_glm_merged_2[[i]])
  sd_glm_merged_3[[i]] <- as.factor(sd_glm_merged_3[[i]])
}
sd_glm_merged_2$result <- as.factor(sd_glm_merged_2$result)
sd_glm_merged_3$result <- as.factor(sd_glm_merged_3$result)



#install.packages("stats")
#library(stats)

require(MASS)



#MODEL WYJŒCIOWY DO BACKWARD SELECTION: ####
#home_or_away
#cechy druzyny
#cechy przeciwnikow
#interakcje
soccer_model_init <- soccer_model
soccer_model  <- 
  polr(result ~ home_or_away
       + buildUpPlaySpeedClass
       + buildUpPlayDribblingClass
       + buildUpPlayPassingClass
       + buildUpPlayPositioningClass
       + chanceCreationPassingClass
       + chanceCreationCrossingClass
       + chanceCreationShootingClass
       + chanceCreationPositioningClass
       + defencePressureClass
       + defenceAggressionClass
       + defenceTeamWidthClass
       + defenceDefenderLineClass
       
       + O_buildUpPlaySpeedClass
       + O_buildUpPlayDribblingClass
       + O_buildUpPlayPassingClass
       + O_buildUpPlayPositioningClass
       + O_chanceCreationPassingClass
       + O_chanceCreationCrossingClass
       + O_chanceCreationShootingClass
       + O_chanceCreationPositioningClass
       + O_defencePressureClass
       + O_defenceAggressionClass
       + O_defenceTeamWidthClass
       + O_defenceDefenderLineClass
       
       + O_buildUpPlaySpeedClass*buildUpPlaySpeedClass
       + O_buildUpPlayDribblingClass*buildUpPlayDribblingClass
       + O_buildUpPlayPassingClass*buildUpPlayPassingClass
       + O_buildUpPlayPositioningClass*buildUpPlayPositioningClass
       + O_chanceCreationPassingClass*chanceCreationPassingClass
       + O_chanceCreationCrossingClass*chanceCreationCrossingClass
       + O_chanceCreationShootingClass*chanceCreationShootingClass
       + O_chanceCreationPositioningClass*chanceCreationPositioningClass
       + O_defencePressureClass*defencePressureClass
       + O_defenceAggressionClass*defenceAggressionClass
       + O_defenceTeamWidthClass*defenceTeamWidthClass
       + O_defenceDefenderLineClass*defenceDefenderLineClass
       
       , data = sd_glm_merged_2[rowSums(is.na(sd_glm_merged_2[ , c(10:11,58:77,136:147)])) == 0, ]
       , Hess=TRUE)

#REZULTAT WSTEPNEGO BACKWARD SELECTION: ####
# DO TEGO ZESTAWY DORZUCAMY PLAYEROW (NIE WSZYSTKICH NARAZ, LECZ KOLEJNO2,3,4,5)
soccer_model_selected  <- 
  polr(result ~ home_or_away
       + buildUpPlaySpeedClass
       + buildUpPlayDribblingClass
       + buildUpPlayPassingClass
       #+ buildUpPlayPositioningClass
       #+ chanceCreationPassingClass
       + chanceCreationCrossingClass
       + chanceCreationShootingClass
       + chanceCreationPositioningClass
       + defencePressureClass
       + defenceAggressionClass
       #+ defenceTeamWidthClass
       #+ defenceDefenderLineClass
       
       + O_buildUpPlaySpeedClass
       + O_buildUpPlayDribblingClass
       + O_buildUpPlayPassingClass
       #+ O_buildUpPlayPositioningClass
       #+ O_chanceCreationPassingClass
       + O_chanceCreationCrossingClass
       + O_chanceCreationShootingClass
       + O_chanceCreationPositioningClass
       + O_defencePressureClass
       + O_defenceAggressionClass
       #+ O_defenceTeamWidthClass
       #+ O_defenceDefenderLineClass
       
       #+ O_buildUpPlaySpeedClass*buildUpPlaySpeedClass
       #+ O_buildUpPlayDribblingClass*buildUpPlayDribblingClass
       #+ O_buildUpPlayPassingClass*buildUpPlayPassingClass
       #+ O_buildUpPlayPositioningClass*buildUpPlayPositioningClass
       #+ O_chanceCreationPassingClass*chanceCreationPassingClass
       + O_chanceCreationCrossingClass*chanceCreationCrossingClass
       #+ O_chanceCreationShootingClass*chanceCreationShootingClass
       #+ O_chanceCreationPositioningClass*chanceCreationPositioningClass
       #+ O_defencePressureClass*defencePressureClass
       #+ O_defenceAggressionClass*defenceAggressionClass
       #+ O_defenceTeamWidthClass*defenceTeamWidthClass
       #+ O_defenceDefenderLineClass*defenceDefenderLineClass
       
       
       , data = sd_glm_merged_2[rowSums(is.na(sd_glm_merged_2[ , c(10:11,58:77,136:147)])) == 0, ]
       , Hess=TRUE)


#REZULTAT BACKWARD SELECTION PO DORZUCENIU KOLEJNYCH PLAYEROW:  ####

soccer_model_ind  <- 
  polr(result ~ home_or_away
       + buildUpPlaySpeedClass
       + buildUpPlayDribblingClass
       + buildUpPlayPassingClass
       + chanceCreationCrossingClass
       + chanceCreationShootingClass
       + chanceCreationPositioningClass
       + defencePressureClass
       + defenceAggressionClass

       + O_buildUpPlaySpeedClass
       + O_buildUpPlayDribblingClass
       + O_buildUpPlayPassingClass
       + O_chanceCreationCrossingClass
       + O_chanceCreationShootingClass
       + O_chanceCreationPositioningClass
       + O_defencePressureClass
       + O_defenceAggressionClass
    
       + O_chanceCreationCrossingClass*chanceCreationCrossingClass
       
       #+ player_2_overall_rating
       #+ player_2_long_passing
       #+ player_2_acceleration
       #+ player_2_sprint_speed
       #+ player_2_agility
       + player_2_jumping
       #+ player_2_strength
       + player_2_interceptions
       #+ player_2_positioning
       #+ player_2_marking
       + player_2_standing_tackle
       + player_2_sliding_tackle
       #+ player_2_short_passing
       #+ player_2_long_shots
       
       #+ player_3_overall_rating
       + player_3_long_passing
       #+ player_3_acceleration
       #+ player_3_sprint_speed
       + player_3_agility
       #+ player_3_jumping
       #+ player_3_strength
       #+ player_3_interceptions
       #+ player_3_positioning
       + player_3_marking
       #+ player_3_standing_tackle
       #+ player_3_sliding_tackle
       #+ player_3_short_passing
       + player_3_long_shots
       
       + player_4_overall_rating
       #+ player_4_long_passing
       + player_4_acceleration
       #+ player_4_sprint_speed
       #+ player_4_agility
       #+ player_4_jumping
       #+ player_4_strength
       #+ player_4_interceptions
       #+ player_4_positioning
       #+ player_4_marking
       #+ player_4_standing_tackle
       #+ player_4_sliding_tackle
       + player_4_short_passing
       #+ player_4_long_shots
       
       + player_5_overall_rating
       + player_5_long_passing
       #+ player_5_acceleration
       + player_5_sprint_speed
       #+ player_5_agility
       #+ player_5_jumping
       + player_5_strength
       #+ player_5_interceptions
       #+ player_5_positioning
       #+ player_5_marking
       #+ player_5_standing_tackle
       + player_5_sliding_tackle
       #+ player_5_short_passing
       #+ player_5_long_shots
       
       
       
       , data = sd_glm_merged_2[rowSums(is.na(sd_glm_merged_2[ , c(10:11,58:77,80:147)])) == 0, ]
       , Hess=TRUE)


#install.packages("leaps")
#library(leaps)

#model_choice_1 <- stepAIC(soccer_model, direction="backward", trace=TRUE) 
warnings()
model_choice_2 <- stepAIC(soccer_model_ind, direction="backward", trace=TRUE) 
model_choice_3 <- stepAIC(soccer_model_ind, direction="backward", trace=TRUE) 
model_choice_4 <- stepAIC(soccer_model_ind, direction="backward", trace=TRUE) 
model_choice_5 <- stepAIC(soccer_model_ind, direction="backward", trace=TRUE) 

model_choice_6 <- stepAIC(soccer_model_ind, direction="backward", trace=TRUE) 





clnms <- as.data.frame(colnames(sd_glm_merged_2))



soccer_model <- soccer_model_selected


summary(soccer_model)
#liczba parametrów bez progów
p=length(coef(soccer_model))
#liczba parametrów z programi
P=length(coef(soccer_model))+2  


install.packages("brant")
library(brant)
install.packages("DescTools")
library(DescTools)
R2s<- PseudoR2(soccer_model, which="all")
R2s <- as.data.frame(t(R2s))
install.packages("DAMisc")
library(DAMisc)
other_fit <- ordfit(soccer_model)
other_fit <- t(other_fit)


##checklist:
#O. brant test
brant(soccer_model_selected)


#1. laczna istotnosc wszystkich zmiennych - test ilorazu wiarygodnosci
#test moze sluzyc do porownywania modeli zagniezdzonych (forward/backward variable selection)
#test ten moze sluzyc do oceny homogenicznosci grupy
#zrodlo: https://stats.stackexchange.com/questions/7720/how-to-understand-output-from-rs-polr-function-ordered-logistic-regression
pchisq(deviance(soccer_model), df.residual(soccer_model))
#UWAGA! Podejrzany wynik... 
TIW <- 2*(subset(R2s, ,select=logLik) - subset(R2s, ,select=logLik0))
pchisq(as.numeric(TIW), p, ncp = 0)
#wynik 1 = odrzucenie H0 o braku istotnosci


#2. pseudo R2
# https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/PseudoR2


# - McKelvey and Zavoina 
subset(R2s, ,select=McKelveyZavoina)
other_fit[[6]]
# - McFadden
subset(R2s, ,select=McFadden)
# - McFadden adj
1-(subset(R2s, ,select=logLik)-p)/subset(R2s, ,select=logLik0)
# - Maximum Likelihood R2 (Cox Snell)
subset(R2s, ,select=CoxSnell)
# - Cragg and Uhler (Nagelkerke)
subset(R2s, ,select=Nagelkerke)



#3. kryteria informacyjne AIC BIC
# AIC - output polr
subset(R2s, ,select=AIC)
#BIC
subset(R2s, ,select=BIC)
#BIC corrected


#zdolnosc predykcyjna modelu
# count R2
other_fit[[1]]
# count R2 Adj
other_fit[[2]]





