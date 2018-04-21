path <- ""

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
# goal, shoton, shotoff, foulcommit, card, cross, corner, possession - dużo NAs

soccer_df$buildUpPlayDribbling <- NULL # b dużo NAs



################################################################################
# ZMIENNE OBJAŚNIAJĄCE
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
