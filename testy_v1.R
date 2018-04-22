################################################################################
# MODELOWANIE
################################################################################

# rm(list = ls())

require(readr); require(MASS); require(ordinal); require(dplyr); require(nnet)
require(brant); require(lmtest)
# require(erer)

soccer_df_raw <- read_csv(paste0(path, "sd_glm_merged_3.csv"))
colnames(soccer_df_raw)
# soccer_df_raw[, 1] <- NULL # X1 col 



# USUWAM BRAKI DANYCH - dwie grupy 23524 & 12118
sapply(soccer_df_raw, function(x) any(is.na(x)))
soccer_df_raw_NAs_sum <- sapply(soccer_df_raw, function(x) sum(is.na(x)))
soccer_df_raw_NAs <- sapply(soccer_df_raw, function(x) is.na(x))
na_id_12118 <- which(soccer_df_raw_NAs_sum == 12118)
na_id_23524 <- which(soccer_df_raw_NAs_sum == 23524)

all(soccer_df_raw_NAs[, na_id_12118[1]] == soccer_df_raw_NAs[, na_id_12118[-1]])
# braki danych w tych samych miejscach

indeks <- !soccer_df_raw_NAs[, na_id_12118[1]]

# SOCCER DATAFRAME
soccer_df <- soccer_df_raw[indeks, ]

# braki danych w zmiennych 0_ ... (np. NA NA, NA Cover, Cover NA, ...)
sapply(soccer_df[, grep("O_", colnames(soccer_df))], 
       function(x) unique(x))
sapply(soccer_df[, grep("O_", colnames(soccer_df))], 
       function(x) sum(grepl("NA", x)))
soccer_df[, grep("O_", colnames(soccer_df))] <- 
  sapply(soccer_df[, grep("O_", colnames(soccer_df))], 
         function(x) {
           x <- ifelse(grepl(" NA", x), NA, x)
           ifelse(grepl("NA ", x), NA, x)
         })

# rm(list = setdiff(ls(), "soccer_df"))

sapply(soccer_df, function(x) sum(is.na(x)))
# goal, shoton, shotoff, foulcommit, card, cross, corner, possession - du¿o NAs

soccer_df$buildUpPlayDribbling <- NULL # b du¿o NAs



################################################################################
# ZMIENNE OBJAŒNIAJ¥CE
colnames(soccer_df)

soccer_df1 <- soccer_df %>%
  select(result, 
         season, stage, home_or_away, goals, # shoton, shotoff, # match data
         # foulcommit, card, cross, corner, possession, # match data
         colnames(soccer_df)[grep("Class", colnames(soccer_df))])

colnames(soccer_df1)

sapply(soccer_df1, function(x) table(x))
sapply(soccer_df1[, -1], function(x) table(soccer_df1$result, x))

soccer_df1 <- data.frame(sapply(soccer_df1, function(x) factor(x)))
str(soccer_df1)

# referencje
soccer_df1$home_or_away_ref <- relevel(soccer_df1$home_or_away, ref = "away")



################################################################################
# MODEL

soccer_df1$result_ref <- relevel(soccer_df1$result, ref = "won")

# soccer_model <- clm(result ~ home_or_away, data = soccer_df1)
# summary(soccer_model)
# 
# soccer_model <- multinom(result ~ home_or_away, data = soccer_df1, Hess = T)
# summary(soccer_model)

# soccer_model <- polr(result_ref ~ home_or_away, data = soccer_df1, Hess = T)
# summary(soccer_model)

soccer_model <- polr(result ~ home_or_away 
                     + buildUpPlayPassingClass
                     + chanceCreationShootingClass
                     + defenceDefenderLineClass
                     
                     , data = soccer_df1, Hess = T)
summary(soccer_model)

coef_table <- coef(summary(soccer_model))
coef_table <- cbind(coef_table,
                    "p value" = pnorm(abs(coef_table[, "t value"]), lower.tail = FALSE) * 2)

exp_coef <- exp(coef(soccer_model))
l


# TESTY
brant(soccer_model)

soccer_model_null <- polr(result ~ +1
                          , data = soccer_df1, Hess = T)
lrtest(soccer_model, soccer_model_null)

#########################################################################################
# TESTY c.d.
#########################################################################################

for(i in team_attr_list) {
  sd_glm_merged_2[[i]] <- as.factor(sd_glm_merged_2[[i]])
  sd_glm_merged_3[[i]] <- as.factor(sd_glm_merged_3[[i]])
}
sd_glm_merged_2$result <- as.factor(sd_glm_merged_2$result)
sd_glm_merged_3$result <- as.factor(sd_glm_merged_3$result)





soccer_model  <- 
  polr(result ~ home_or_away
       + buildUpPlaySpeedClass
       #+ buildUpPlayDribblingClass
       #+ buildUpPlayPassingClass
       #+ buildUpPlayPositioningClass
       #+ chanceCreationPassingClass
       #+ chanceCreationCrossingClass
       #+ chanceCreationShootingClass
       #+ chanceCreationPositioningClass
       #+ defencePressureClass
       #+ defenceAggressionClass
       #+ defenceTeamWidthClass
       #+ defenceDefenderLineClass
       , data = sd_glm_merged_2
       , Hess=TRUE)







summary(soccer_model)
#liczba parametrów bez progów
p=length(coef(soccer_model))
#liczba parametrów z programi
P=length(coef(soccer_model))+2  

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
brant(soccer_model)


#1. laczna istotnosc wszystkich zmiennych - test ilorazu wiarygodnosci
#test moze sluzyc do porownywania modeli zagniezdzonych (forward/backward variable selection)
#test ten moze sluzyc do oceny homogenicznosci grupy
#zrodlo: https://stats.stackexchange.com/questions/7720/how-to-understand-output-from-rs-polr-function-ordered-logistic-regression
pchisq(deviance(soccer_model), df.residual(soccer_model))
#UWAGA! Podejrzany wynik... 


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



df = data.frame(n, s, b)  

data.frame(Date=as.Date("01/01/2000", format="%m/%d/%Y"), 
           )
