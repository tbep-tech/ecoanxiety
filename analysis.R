library(tidyverse)
library(MASS)
library(pscl)

# Load data

mydata <- read.csv("tbcs_quant_2024.csv")
quantdata <- mydata[,-1]
rownames(quantdata) <- mydata[,1]

########## DATA PREP & CLEANING ##################

### Select data of interest

# Audiences
publicdata <- subset(quantdata, AUDIENCE == "Public")
innetworkdata <- subset(quantdata, AUDIENCE == "In-network")

# Variables of interest
publicdata <- publicdata[, c("POINT_X","POINT_Y","ZIPCODE","YEARS","PROPERTY","OWNERSHIP",
                             "AGE","GENDER","EDUCATION","HHINCOME","RACE_WHITE","RACE_HISPANIC","CONSERVATIVE",
                             "INFO_TV","INFO_SOCIAL","INFO_GOV","INFO_NGO","INFO_WORD","INFO_OWN",
                             "GROUP_INVOLVEMENT","GROUP_ENVPRT","ACTIVE_AVG",
                             "BEHAVIOR_NODRIV","BEHAVIOR_RECYCL","BEHAVIOR_PLNTFF","BEHAVIOR_RESTOR",
                             "ATTITUDE_OPPSAT","ATTITUDE_CHNGFW","ATTITUDE_CHNGRN","ATTITUDE_CHNGTP","ATTITUDE_CCTHRT",
                             "PRIORITIZE_ENVIRON","JUSTICE","NRELATEDNESS","PIDENTITY","SOLASTALGIA",
                             "HOPEPATH_SELFEFF","HOPEPATH_RESPEFF","SWELLBEING","MHI_SCORE","LIFESATISFACTION",
                             "ECOANXIETY_SCORE","ECOANXIETY_CAT","KNOWLEDGE_SCORE")]
innetworkdata <- innetworkdata[, c("POINT_X","POINT_Y","ZIPCODE","YEARS","PROPERTY","OWNERSHIP",
                                   "AGE","GENDER","EDUCATION","HHINCOME","RACE_WHITE","RACE_HISPANIC","CONSERVATIVE",
                                   "INFO_TV","INFO_SOCIAL","INFO_GOV","INFO_NGO","INFO_WORD","INFO_OWN",
                                   "GROUP_INVOLVEMENT","GROUP_ENVPRT","ACTIVE_AVG",
                                   "BEHAVIOR_NODRIV","BEHAVIOR_RECYCL","BEHAVIOR_PLNTFF","BEHAVIOR_RESTOR",
                                   "ATTITUDE_OPPSAT","ATTITUDE_CHNGFW","ATTITUDE_CHNGRN","ATTITUDE_CHNGTP","ATTITUDE_CCTHRT",
                                   "PRIORITIZE_ENVIRON","JUSTICE","NRELATEDNESS","PIDENTITY","SOLASTALGIA",
                                   "HOPEPATH_SELFEFF","HOPEPATH_RESPEFF","SWELLBEING","MHI_SCORE","LIFESATISFACTION",
                                   "ECOANXIETY_SCORE","ECOANXIETY_CAT","REFERRAL","KNOWLEDGE_SCORE")]
# Remove NAs
publicdata$GENDER <- ifelse(publicdata$GENDER == "", NA, publicdata$GENDER)
publicdata_complete <- publicdata %>%
  drop_na(ZIPCODE,YEARS,PROPERTY,OWNERSHIP,
          AGE,GENDER,EDUCATION,HHINCOME,RACE_WHITE,RACE_HISPANIC,CONSERVATIVE,
          INFO_TV,INFO_SOCIAL,INFO_GOV,INFO_NGO,INFO_WORD,INFO_OWN,
          GROUP_INVOLVEMENT,GROUP_ENVPRT,ACTIVE_AVG,
          BEHAVIOR_NODRIV,BEHAVIOR_RECYCL,BEHAVIOR_PLNTFF,BEHAVIOR_RESTOR,
          ATTITUDE_OPPSAT,ATTITUDE_CHNGFW,ATTITUDE_CHNGRN,ATTITUDE_CHNGTP,ATTITUDE_CCTHRT,
          PRIORITIZE_ENVIRON,JUSTICE,NRELATEDNESS,PIDENTITY,SOLASTALGIA,
          HOPEPATH_SELFEFF,HOPEPATH_RESPEFF,SWELLBEING,MHI_SCORE,LIFESATISFACTION,
          ECOANXIETY_SCORE,ECOANXIETY_CAT,KNOWLEDGE_SCORE)
innetworkdata$GENDER <- ifelse(innetworkdata$GENDER == "", NA, innetworkdata$GENDER)
innetworkdata_complete <- innetworkdata %>%
  drop_na(ZIPCODE,YEARS,PROPERTY,OWNERSHIP,
          AGE,GENDER,EDUCATION,HHINCOME,RACE_WHITE,RACE_HISPANIC,CONSERVATIVE,
          INFO_TV,INFO_SOCIAL,INFO_GOV,INFO_NGO,INFO_WORD,INFO_OWN,
          GROUP_INVOLVEMENT,GROUP_ENVPRT,ACTIVE_AVG,
          BEHAVIOR_NODRIV,BEHAVIOR_RECYCL,BEHAVIOR_PLNTFF,BEHAVIOR_RESTOR,
          ATTITUDE_OPPSAT,ATTITUDE_CHNGFW,ATTITUDE_CHNGRN,ATTITUDE_CHNGTP,ATTITUDE_CCTHRT,
          PRIORITIZE_ENVIRON,JUSTICE,NRELATEDNESS,PIDENTITY,SOLASTALGIA,
          HOPEPATH_SELFEFF,HOPEPATH_RESPEFF,SWELLBEING,MHI_SCORE,LIFESATISFACTION,
          ECOANXIETY_SCORE,ECOANXIETY_CAT,KNOWLEDGE_SCORE)

### Reclassify/Set levels for categorical variables

# Reclassify
publicdata_final <- publicdata_complete %>%
  mutate(YEARS = ifelse(YEARS == "Less than a year" | YEARS == "1 - 4 years","Less than 5 years",YEARS),
         PROPERTY = ifelse(PROPERTY == "Apartment" | PROPERTY == "Condominium" | PROPERTY == "Townhome or duplex","Multi-family home",PROPERTY),
         OWN = ifelse(OWNERSHIP == "Own",1,0),
         MALE = ifelse(GENDER == "Male",1,0),
         EDUCATION = ifelse(EDUCATION == "Did not complete high school" | EDUCATION == "High school diploma or GED","High school or less",
                            ifelse(EDUCATION == "Associate degree" | EDUCATION == "Bachelor's degree","Undergraduate degree",
                                   ifelse(EDUCATION == "Master's degree" | EDUCATION == "Doctoral degree","Graduate degree","Post-secondary degree"))),
         ECOANXIETY_CAT3 = ifelse(ECOANXIETY_CAT == "Moderate" | ECOANXIETY_CAT == "Severe","Moderate or Severe",ECOANXIETY_CAT),
         ECOANXIETY = ifelse(ECOANXIETY_CAT == "Moderate" | ECOANXIETY_CAT == "Severe",1,0),
         GROUP_ENVPRT = ifelse(GROUP_ENVPRT > 0,1,0),
         PEB_AVG = (BEHAVIOR_NODRIV + BEHAVIOR_RECYCL + BEHAVIOR_PLNTFF + BEHAVIOR_RESTOR)/4,
         CHNG_AVG = (ATTITUDE_CHNGFW + ATTITUDE_CHNGRN + ATTITUDE_CHNGTP)/3,
         SWELLBEING_SCORE = MHI_SCORE + LIFESATISFACTION)
innetworkdata_final <- innetworkdata_complete %>%
  mutate(YEARS = ifelse(YEARS == "Less than a year" | YEARS == "1 - 4 years","Less than 5 years",YEARS),
         PROPERTY = ifelse(PROPERTY == "Apartment" | PROPERTY == "Condominium" | PROPERTY == "Townhome or duplex","Multi-family home",PROPERTY),
         OWN = ifelse(OWNERSHIP == "Own",1,0),
         MALE = ifelse(GENDER == "Male",1,0),
         EDUCATION = ifelse(EDUCATION == "Did not complete high school" | EDUCATION == "High school diploma or GED","High school or less",
                            ifelse(EDUCATION == "Associate degree" | EDUCATION == "Bachelor's degree","Undergraduate degree",
                                   ifelse(EDUCATION == "Master's degree" | EDUCATION == "Doctoral degree","Graduate degree","Post-secondary degree"))),
         ECOANXIETY_CAT3 = ifelse(ECOANXIETY_CAT == "Moderate" | ECOANXIETY_CAT == "Severe","Moderate or Severe",ECOANXIETY_CAT),
         ECOANXIETY = ifelse(ECOANXIETY_CAT == "Moderate" | ECOANXIETY_CAT == "Severe",1,0),
         GROUP_ENVPRT = ifelse(GROUP_ENVPRT > 0,1,0),
         PEB_AVG = (BEHAVIOR_NODRIV + BEHAVIOR_RECYCL + BEHAVIOR_PLNTFF + BEHAVIOR_RESTOR)/4,
         CHNG_AVG = (ATTITUDE_CHNGFW + ATTITUDE_CHNGRN + ATTITUDE_CHNGTP)/3,
         SWELLBEING_SCORE = MHI_SCORE + LIFESATISFACTION)

# Set factor levels
publicdata_final <- publicdata_final %>%
  mutate(YEARS = factor(YEARS, levels = c("Less than 5 years","5 - 9 years","10 - 19 years","20 years or more")),
         PROPERTY = factor(PROPERTY, levels = c("Single-family home","Multi-family home","Mobile home")),
         AGE = factor(AGE, levels = c("18 - 24","25 - 34","35 - 44","45 - 54","55 - 64","65 or older")),
         EDUCATION = factor(EDUCATION, levels = c("High school or less","Post-secondary degree","Undergraduate degree","Graduate degree")),
         HHINCOME = factor(HHINCOME, levels = c("Less than $25,000","$25,000 - $49,999","$50,000 - $74,999","$75,000 - $99,999","$100,000 or more")),
         ECOANXIETY_CAT = factor(ECOANXIETY_CAT, levels = c("Low","Mild","Moderate","Severe")),
         ECOANXIETY_CAT3 = factor(ECOANXIETY_CAT3, levels = c("Low","Mild","Moderate or Severe")))
innetworkdata_final <- innetworkdata_final %>%
  mutate(YEARS = factor(YEARS, levels = c("Less than 5 years","5 - 9 years","10 - 19 years","20 years or more")),
         PROPERTY = factor(PROPERTY, levels = c("Single-family home","Multi-family home","Mobile home")),
         AGE = factor(AGE, levels = c("18 - 24","25 - 34","35 - 44","45 - 54","55 - 64","65 or older")),
         EDUCATION = factor(EDUCATION, levels = c("High school or less","Post-secondary degree","Undergraduate degree","Graduate degree")),
         HHINCOME = factor(HHINCOME, levels = c("Less than $25,000","$25,000 - $49,999","$50,000 - $74,999","$75,000 - $99,999","$100,000 or more")),
         ECOANXIETY_CAT = factor(ECOANXIETY_CAT, levels = c("Low","Mild","Moderate","Severe")),
         ECOANXIETY_CAT3 = factor(ECOANXIETY_CAT3, levels = c("Low","Mild","Moderate or Severe")))

# Remove outdated variables
finaldata_public <- publicdata_final[, c("POINT_X","POINT_Y","ZIPCODE","YEARS","PROPERTY","OWN",
                                         "AGE","MALE","EDUCATION","HHINCOME","RACE_WHITE","RACE_HISPANIC","CONSERVATIVE",
                                         "INFO_TV","INFO_SOCIAL","INFO_GOV","INFO_NGO","INFO_WORD","INFO_OWN",
                                         "GROUP_INVOLVEMENT","GROUP_ENVPRT","ACTIVE_AVG","PEB_AVG","CHNG_AVG",
                                         "ATTITUDE_OPPSAT","ATTITUDE_CHNGFW","ATTITUDE_CHNGRN","ATTITUDE_CHNGTP","ATTITUDE_CCTHRT",
                                         "PRIORITIZE_ENVIRON","JUSTICE","NRELATEDNESS","PIDENTITY","SOLASTALGIA",
                                         "HOPEPATH_SELFEFF","HOPEPATH_RESPEFF","SWELLBEING","SWELLBEING_SCORE","MHI_SCORE",
                                         "ECOANXIETY_SCORE","ECOANXIETY_CAT","ECOANXIETY_CAT3","ECOANXIETY","KNOWLEDGE_SCORE")]
finaldata_innetwork <- innetworkdata_final[, c("POINT_X","POINT_Y","ZIPCODE","YEARS","PROPERTY","OWN",
                                               "AGE","MALE","EDUCATION","HHINCOME","RACE_WHITE","RACE_HISPANIC","CONSERVATIVE",
                                               "INFO_TV","INFO_SOCIAL","INFO_GOV","INFO_NGO","INFO_WORD","INFO_OWN",
                                               "GROUP_INVOLVEMENT","GROUP_ENVPRT","ACTIVE_AVG","PEB_AVG","CHNG_AVG",
                                               "ATTITUDE_OPPSAT","ATTITUDE_CHNGFW","ATTITUDE_CHNGRN","ATTITUDE_CHNGTP","ATTITUDE_CCTHRT",
                                               "PRIORITIZE_ENVIRON","JUSTICE","NRELATEDNESS","PIDENTITY","SOLASTALGIA",
                                               "HOPEPATH_SELFEFF","HOPEPATH_RESPEFF","SWELLBEING","SWELLBEING_SCORE","MHI_SCORE",
                                               "ECOANXIETY_SCORE","ECOANXIETY_CAT","ECOANXIETY_CAT3","ECOANXIETY","REFERRAL","KNOWLEDGE_SCORE")]

# In-network channels
channels <- finaldata_innetwork %>%
  group_by(REFERRAL) %>%
  summarise(count = n()) %>%
  mutate(pct = count/sum(count)*100)


########## PUBLIC ##################


########## * MULTIPLE REGRESSION ##################


##### *-- Eco-anxiety Score #####

# original full model
EAmodelraw <- lm(ECOANXIETY_SCORE ~ YEARS + PROPERTY + OWN + AGE + MALE + EDUCATION + HHINCOME + RACE_WHITE + 
                   RACE_HISPANIC + CONSERVATIVE + GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_CCTHRT + ATTITUDE_OPPSAT + 
                   JUSTICE + NRELATEDNESS + PIDENTITY + SOLASTALGIA + HOPEPATH_SELFEFF + HOPEPATH_RESPEFF + KNOWLEDGE_SCORE + SWELLBEING_SCORE, data = finaldata_public)
summary(EAmodelraw)
# adj R2 = 0.5369  
par(mfrow = c(2,2))
plot(EAmodelraw)
# very right skewed

# transformed full model
EAmodelfull <- lm(ECOANXIETY_SCORE^(1/2) ~ YEARS + PROPERTY + OWN + AGE + MALE + EDUCATION + HHINCOME + RACE_WHITE + 
                    RACE_HISPANIC + CONSERVATIVE + GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_CCTHRT + ATTITUDE_OPPSAT + 
                    JUSTICE + NRELATEDNESS + PIDENTITY + SOLASTALGIA + HOPEPATH_SELFEFF + HOPEPATH_RESPEFF + KNOWLEDGE_SCORE + SWELLBEING_SCORE, data = finaldata_public)
summary(EAmodelfull)
# adj R2 = 0.5313  
par(mfrow = c(2,2))
plot(EAmodelfull)
# much better!

##### *--- Sensitivity Check (SW excluded) #####

# transformed full model
EAmodelfull2 <- lm(ECOANXIETY_SCORE^(1/2) ~ YEARS + PROPERTY + OWN + AGE + MALE + EDUCATION + HHINCOME + RACE_WHITE + 
                     RACE_HISPANIC + CONSERVATIVE + GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_CCTHRT + ATTITUDE_OPPSAT + 
                     JUSTICE + NRELATEDNESS + PIDENTITY + SOLASTALGIA + HOPEPATH_SELFEFF + HOPEPATH_RESPEFF + KNOWLEDGE_SCORE, data = finaldata_public)
summary(EAmodelfull2)
# adj R2 = 0.2935  



##### *-- Subjective Wellbeing Score #####

# original full model
SWmodelraw <- lm(SWELLBEING_SCORE ~ YEARS + PROPERTY + OWN + AGE + MALE + EDUCATION + HHINCOME + RACE_WHITE + 
                   RACE_HISPANIC + CONSERVATIVE + GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_CCTHRT + ATTITUDE_OPPSAT + 
                   JUSTICE + NRELATEDNESS + PIDENTITY + SOLASTALGIA + HOPEPATH_SELFEFF + HOPEPATH_RESPEFF + KNOWLEDGE_SCORE, data = finaldata_public)
summary(SWmodelraw)
# adj R2 = 0.2582  
par(mfrow = c(2,2))
plot(SWmodelraw)
# very left skewed

# transformed full model
SWmodelfull <- lm((SWELLBEING_SCORE)^2 ~ YEARS + PROPERTY + OWN + AGE + MALE + EDUCATION + HHINCOME + RACE_WHITE + 
                    RACE_HISPANIC + CONSERVATIVE + GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_CCTHRT + ATTITUDE_OPPSAT + 
                    JUSTICE + NRELATEDNESS + PIDENTITY + SOLASTALGIA + HOPEPATH_SELFEFF + HOPEPATH_RESPEFF + KNOWLEDGE_SCORE, data = finaldata_public)
summary(SWmodelfull)
# adj R2 = 0.2851  
par(mfrow = c(2,2))
plot(SWmodelfull)
# much better!

##### *--- Sensitivity Check (EA included) #####

# transformed full model
SWmodelfull2 <- lm((SWELLBEING_SCORE)^2 ~ YEARS + PROPERTY + OWN + AGE + MALE + EDUCATION + HHINCOME + RACE_WHITE + 
                     RACE_HISPANIC + CONSERVATIVE + GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_CCTHRT + ATTITUDE_OPPSAT + 
                     JUSTICE + NRELATEDNESS + PIDENTITY + SOLASTALGIA + HOPEPATH_SELFEFF + HOPEPATH_RESPEFF + KNOWLEDGE_SCORE + ECOANXIETY_SCORE, data = finaldata_public)
summary(SWmodelfull2)
# adj R2 = 0.5227  




########## * SYMPTOM SEVERITY ##################

##### *- CORRELATION ##### 

corr <- cor.test(x = finaldata_public$SWELLBEING_SCORE, y = finaldata_public$ECOANXIETY_SCORE, method = "spearman")
corr
sink()

plot_scatter <- ggplot(finaldata_public, aes(x = SWELLBEING_SCORE, y = ECOANXIETY_SCORE)) +
  geom_count() +
  scale_size_area() +
  geom_smooth(method = lm, se = FALSE) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank())


# Classify groups of people:
# - 1) High Eco-anxiety (Score >= 10) & Low Eco-anxiety (Score <= 9)

finaldata_public <- finaldata_public %>%
  mutate(EAclass = ifelse(ECOANXIETY_SCORE >= 10,"High","Low"))

finaldata_public <- finaldata_public %>%
  mutate(HighEA = ifelse(EAclass == "High",1,0))


##### *-- LOGISTIC REGRESSION ##### 

library(car)
library(sjPlot)

#### *--- Residents with High EA ####

# Full model
Hfullmodel <- glm(HighEA ~ YEARS + PROPERTY + OWN + AGE + MALE + EDUCATION + HHINCOME + RACE_WHITE + RACE_HISPANIC + CONSERVATIVE +
                    GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_CCTHRT + ATTITUDE_OPPSAT + JUSTICE + NRELATEDNESS + PIDENTITY + 
                    SOLASTALGIA + HOPEPATH_SELFEFF + HOPEPATH_RESPEFF + KNOWLEDGE_SCORE + SWELLBEING_SCORE, data = finaldata_public, family = binomial(link = "logit"))
summary(Hfullmodel)
#AIC: 614.78
exp(cbind("Odds ratio" = coef(Hfullmodel), confint.default(Hfullmodel, level = 0.95)))
pR2(Hfullmodel)
#McFadden pseudo-R2: 0.4338474
vif(Hfullmodel)

### Reduced models ###
Hmodel1.1 <- glm(HighEA ~ YEARS + PROPERTY + OWN + AGE + MALE + EDUCATION + RACE_WHITE + RACE_HISPANIC + CONSERVATIVE +
                   GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_CCTHRT + ATTITUDE_OPPSAT + JUSTICE + NRELATEDNESS + PIDENTITY + 
                   SOLASTALGIA + HOPEPATH_SELFEFF + HOPEPATH_RESPEFF + KNOWLEDGE_SCORE + SWELLBEING_SCORE, data = finaldata_public, family = binomial(link = "logit"))
summary(Hmodel1.1)
#AIC: 608.65

Hmodel1.2 <- glm(HighEA ~ YEARS + OWN + AGE + MALE + EDUCATION + RACE_WHITE + RACE_HISPANIC + CONSERVATIVE +
                   GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_CCTHRT + ATTITUDE_OPPSAT + JUSTICE + NRELATEDNESS + PIDENTITY + 
                   SOLASTALGIA + HOPEPATH_SELFEFF + HOPEPATH_RESPEFF + KNOWLEDGE_SCORE + SWELLBEING_SCORE, data = finaldata_public, family = binomial(link = "logit"))
summary(Hmodel1.2)
#AIC: 605.47

Hmodel1.3 <- glm(HighEA ~ YEARS + OWN + AGE + MALE + RACE_WHITE + RACE_HISPANIC + CONSERVATIVE +
                   GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_CCTHRT + ATTITUDE_OPPSAT + JUSTICE + NRELATEDNESS + PIDENTITY + 
                   SOLASTALGIA + HOPEPATH_SELFEFF + HOPEPATH_RESPEFF + KNOWLEDGE_SCORE + SWELLBEING_SCORE, data = finaldata_public, family = binomial(link = "logit"))
summary(Hmodel1.3)
#AIC: 603.02

Hmodel1.4 <- glm(HighEA ~ YEARS + OWN + MALE + RACE_WHITE + RACE_HISPANIC + CONSERVATIVE +
                   GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_CCTHRT + ATTITUDE_OPPSAT + JUSTICE + NRELATEDNESS + PIDENTITY + 
                   SOLASTALGIA + HOPEPATH_SELFEFF + HOPEPATH_RESPEFF + KNOWLEDGE_SCORE + SWELLBEING_SCORE, data = finaldata_public, family = binomial(link = "logit"))
summary(Hmodel1.4)
#AIC: 601.02

Hmodel1.5 <- glm(HighEA ~ YEARS + OWN + MALE + RACE_WHITE + RACE_HISPANIC + CONSERVATIVE +
                   GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_OPPSAT + JUSTICE + NRELATEDNESS + PIDENTITY + 
                   SOLASTALGIA + HOPEPATH_SELFEFF + HOPEPATH_RESPEFF + KNOWLEDGE_SCORE + SWELLBEING_SCORE, data = finaldata_public, family = binomial(link = "logit"))
summary(Hmodel1.5)
#AIC: 599.02

Hmodel1.6 <- glm(HighEA ~ YEARS + MALE + RACE_WHITE + RACE_HISPANIC + CONSERVATIVE +
                   GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_OPPSAT + JUSTICE + NRELATEDNESS + PIDENTITY + 
                   SOLASTALGIA + HOPEPATH_SELFEFF + HOPEPATH_RESPEFF + KNOWLEDGE_SCORE + SWELLBEING_SCORE, data = finaldata_public, family = binomial(link = "logit"))
summary(Hmodel1.6)
#AIC: 597.17

Hmodel1.7 <- glm(HighEA ~ YEARS + MALE + RACE_WHITE + RACE_HISPANIC + CONSERVATIVE +
                   GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_OPPSAT + JUSTICE + NRELATEDNESS + 
                   SOLASTALGIA + HOPEPATH_SELFEFF + HOPEPATH_RESPEFF + KNOWLEDGE_SCORE + SWELLBEING_SCORE, data = finaldata_public, family = binomial(link = "logit"))
summary(Hmodel1.7)
#AIC: 595.48

Hmodel1.8 <- glm(HighEA ~ YEARS + MALE + RACE_WHITE + RACE_HISPANIC + CONSERVATIVE +
                   GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_OPPSAT + JUSTICE + 
                   SOLASTALGIA + HOPEPATH_SELFEFF + HOPEPATH_RESPEFF + KNOWLEDGE_SCORE + SWELLBEING_SCORE, data = finaldata_public, family = binomial(link = "logit"))
summary(Hmodel1.8)
#AIC: 593.96

Hmodel1.9 <- glm(HighEA ~ YEARS + MALE + RACE_WHITE + RACE_HISPANIC + CONSERVATIVE +
                   GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_OPPSAT + JUSTICE + 
                   SOLASTALGIA + HOPEPATH_SELFEFF + HOPEPATH_RESPEFF + SWELLBEING_SCORE, data = finaldata_public, family = binomial(link = "logit"))
summary(Hmodel1.9)
#AIC: 592.59

Hmodel1.10 <- glm(HighEA ~ YEARS + MALE + RACE_WHITE + RACE_HISPANIC + CONSERVATIVE +
                    GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_OPPSAT + JUSTICE + 
                    SOLASTALGIA + HOPEPATH_RESPEFF + SWELLBEING_SCORE, data = finaldata_public, family = binomial(link = "logit"))
summary(Hmodel1.10)
#AIC: 591.27

Hmodel1.11 <- glm(HighEA ~ YEARS + RACE_WHITE + RACE_HISPANIC + CONSERVATIVE +
                    GROUP_ENVPRT + ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_OPPSAT + JUSTICE + 
                    SOLASTALGIA + HOPEPATH_RESPEFF + SWELLBEING_SCORE, data = finaldata_public, family = binomial(link = "logit"))
summary(Hmodel1.11)
#AIC: 590.22

Hmodel1.12 <- glm(HighEA ~ YEARS + RACE_WHITE + RACE_HISPANIC + CONSERVATIVE +
                    ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_OPPSAT + JUSTICE + 
                    SOLASTALGIA + HOPEPATH_RESPEFF + SWELLBEING_SCORE, data = finaldata_public, family = binomial(link = "logit"))
summary(Hmodel1.12)
#AIC: 589.68

Hmodel1.13 <- glm(HighEA ~ YEARS + RACE_HISPANIC + CONSERVATIVE +
                    ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_OPPSAT + JUSTICE + 
                    SOLASTALGIA + HOPEPATH_RESPEFF + SWELLBEING_SCORE, data = finaldata_public, family = binomial(link = "logit"))
summary(Hmodel1.13)
#AIC: 589.53

# Optimal model
Hmodel1.13 <- glm(HighEA ~ YEARS + CONSERVATIVE +
                    ACTIVE_AVG + PEB_AVG + CHNG_AVG + ATTITUDE_OPPSAT + JUSTICE + 
                    SOLASTALGIA + HOPEPATH_RESPEFF + SWELLBEING_SCORE, data = finaldata_public, family = binomial(link = "logit"))
summary(Hmodel1.13)
#AIC: 588.51
exp(cbind("Odds ratio" = coef(Hmodel1.13), confint.default(Hmodel1.13, level = 0.95)))
pR2(Hmodel1.13)
#McFadden pseudo-R2: 0.4110975
vif(Hmodel1.13)





########## PATH ANALYSIS ##################

library(lavaan)
library(semPlot)

########## * Public ##################

finaldata_public$ECOANXIETY_SCORE_SQRT <- finaldata_public$ECOANXIETY_SCORE^(1/2)
finaldata_public$ECOANXIETY_SCORE_SQRT_SCALED <- scale(finaldata_public$ECOANXIETY_SCORE_SQRT)
finaldata_public$SWELLBEING_SCORE2 <- finaldata_public$SWELLBEING_SCORE^(2)
finaldata_public$SWELLBEING_SCORE2_SCALED <- scale(finaldata_public$SWELLBEING_SCORE2)
finaldata_public$AGEx <- ifelse(finaldata_public$AGE == "18 - 24", 1,
                                ifelse(finaldata_public$AGE == "25 - 34", 2,
                                       ifelse(finaldata_public$AGE == "35 - 44", 3,
                                              ifelse(finaldata_public$AGE == "45 - 54", 4,
                                                     ifelse(finaldata_public$AGE == "55 - 64", 5,
                                                            ifelse(finaldata_public$AGE == "65 or older", 6,NA))))))
finaldata_public$HHINCOMEx <- ifelse(finaldata_public$HHINCOME == "Less than $25,000", 1,
                                     ifelse(finaldata_public$HHINCOME == "$25,000 - $49,999", 2,
                                            ifelse(finaldata_public$HHINCOME == "$50,000 - $74,999", 3,
                                                   ifelse(finaldata_public$HHINCOME == "$75,000 - $99,999", 4,
                                                          ifelse(finaldata_public$HHINCOME == "$100,000 or more", 5,NA)))))
finaldata_public$EDUCATIONx <- ifelse(finaldata_public$EDUCATION == "High school or less", 1,
                                      ifelse(finaldata_public$EDUCATION == "Post-secondary degree", 2,
                                             ifelse(finaldata_public$EDUCATION == "Undergraduate degree", 3,
                                                    ifelse(finaldata_public$EDUCATION == "Graduate degree", 4,NA))))


# based on original regressions
model1 <- '
ECOANXIETY_SCORE_SQRT_SCALED ~ c1*HHINCOMEx + c2*CHNG_AVG + c3*GROUP_ENVPRT + c4*PEB_AVG + c5*JUSTICE + c6*NRELATEDNESS + b1*SWELLBEING_SCORE2_SCALED
SWELLBEING_SCORE2_SCALED ~ a1*HHINCOMEx + a2*CHNG_AVG + a3*AGEx + a4*EDUCATIONx + a5*OWN + a6*RACE_WHITE + a7*PIDENTITY + a8*SOLASTALGIA + a9*HOPEPATH_SELFEFF + a10*ATTITUDE_OPPSAT
indirect1 := a1*b1
indirect2 := a2*b1
'
fit1 <- sem(model1, data = finaldata_public)
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

par(mfrow = c(1,1))
semPaths(fit1, what='std', 
         edge.label.cex=1.25, curvePivot = TRUE, 
         fade=FALSE)


# based on sensitivity tests (amended SW, with bidirectional EA & SW)
model2 <- '
ECOANXIETY_SCORE_SQRT_SCALED ~ c1*HHINCOMEx + c2*CHNG_AVG + c3*GROUP_ENVPRT + c4*PEB_AVG + c5*JUSTICE + c6*NRELATEDNESS + b1*SWELLBEING_SCORE2_SCALED
SWELLBEING_SCORE2_SCALED ~ a1*PEB_AVG + a2*CHNG_AVG + a3*AGEx + a4*EDUCATIONx + a5*OWN + a7*PIDENTITY + a8*SOLASTALGIA + a10*ATTITUDE_OPPSAT + b2*ECOANXIETY_SCORE_SQRT_SCALED
indirect1 := a1*b1
indirect2 := a2*b1
'
fit2 <- sem(model2, data = finaldata_public)
summary(fit2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

par(mfrow = c(1,1))
semPaths(fit2, what='std', 
         edge.label.cex=1.25, curvePivot = TRUE, 
         fade=FALSE)





########## * In-network ##################

finaldata_innetwork <- finaldata_innetwork[-(153),]
finaldata_innetwork$ECOANXIETY_SCORE_SQRT <- finaldata_innetwork$ECOANXIETY_SCORE^(1/2)
finaldata_innetwork$ECOANXIETY_SCORE_SQRT_SCALED <- scale(finaldata_innetwork$ECOANXIETY_SCORE_SQRT)
finaldata_innetwork$SWELLBEING_SCORE2 <- finaldata_innetwork$SWELLBEING_SCORE^(2)
finaldata_innetwork$SWELLBEING_SCORE2_SCALED <- scale(finaldata_innetwork$SWELLBEING_SCORE2)
finaldata_innetwork$AGEx <- ifelse(finaldata_innetwork$AGE == "18 - 24", 1,
                                   ifelse(finaldata_innetwork$AGE == "25 - 34", 2,
                                          ifelse(finaldata_innetwork$AGE == "35 - 44", 3,
                                                 ifelse(finaldata_innetwork$AGE == "45 - 54", 4,
                                                        ifelse(finaldata_innetwork$AGE == "55 - 64", 5,
                                                               ifelse(finaldata_innetwork$AGE == "65 or older", 6,NA))))))
finaldata_innetwork$HHINCOMEx <- ifelse(finaldata_innetwork$HHINCOME == "Less than $25,000", 1,
                                        ifelse(finaldata_innetwork$HHINCOME == "$25,000 - $49,999", 2,
                                               ifelse(finaldata_innetwork$HHINCOME == "$50,000 - $74,999", 3,
                                                      ifelse(finaldata_innetwork$HHINCOME == "$75,000 - $99,999", 4,
                                                             ifelse(finaldata_innetwork$HHINCOME == "$100,000 or more", 5,NA)))))
finaldata_innetwork$EDUCATIONx <- ifelse(finaldata_innetwork$EDUCATION == "High school or less", 1,
                                         ifelse(finaldata_innetwork$EDUCATION == "Post-secondary degree", 2,
                                                ifelse(finaldata_innetwork$EDUCATION == "Undergraduate degree", 3,
                                                       ifelse(finaldata_innetwork$EDUCATION == "Graduate degree", 4,NA))))

model3 <- '
ECOANXIETY_SCORE_SQRT_SCALED ~ c1*HHINCOMEx + c2*CHNG_AVG + c3*GROUP_ENVPRT + c4*PEB_AVG + c5*JUSTICE + c6*NRELATEDNESS + b1*SWELLBEING_SCORE2_SCALED
SWELLBEING_SCORE2_SCALED ~ a1*HHINCOMEx + a2*CHNG_AVG + a3*AGEx + a4*EDUCATIONx + a5*OWN + a6*RACE_WHITE + a7*PIDENTITY + a8*SOLASTALGIA + a9*HOPEPATH_SELFEFF + a10*ATTITUDE_OPPSAT
indirect1 := a1*b1
indirect2 := a2*b1
'
fit3 <- sem(model3, data = finaldata_innetwork)
summary(fit3, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

semPaths(fit3, what='std', 
         edge.label.cex=1.25, curvePivot = TRUE, 
         fade=FALSE)




########## PUBLIC & IN-NETWORK COMPARISONS ##################

finaldata_public <- publicdata_final[, c("POINT_X","POINT_Y","ZIPCODE","YEARS","PROPERTY","OWN",
                                         "AGE","MALE","EDUCATION","HHINCOME","RACE_WHITE","RACE_HISPANIC","CONSERVATIVE",
                                         "INFO_TV","INFO_SOCIAL","INFO_GOV","INFO_NGO","INFO_WORD","INFO_OWN",
                                         "GROUP_INVOLVEMENT","GROUP_ENVPRT","ACTIVE_AVG","PEB_AVG","CHNG_AVG",
                                         "ATTITUDE_OPPSAT","ATTITUDE_CHNGFW","ATTITUDE_CHNGRN","ATTITUDE_CHNGTP","ATTITUDE_CCTHRT",
                                         "PRIORITIZE_ENVIRON","JUSTICE","NRELATEDNESS","PIDENTITY","SOLASTALGIA",
                                         "HOPEPATH_SELFEFF","HOPEPATH_RESPEFF","SWELLBEING","SWELLBEING_SCORE","MHI_SCORE",
                                         "ECOANXIETY_SCORE","ECOANXIETY_CAT","ECOANXIETY_CAT3","ECOANXIETY","KNOWLEDGE_SCORE")]
finaldata_innetwork <- innetworkdata_final[, c("POINT_X","POINT_Y","ZIPCODE","YEARS","PROPERTY","OWN",
                                               "AGE","MALE","EDUCATION","HHINCOME","RACE_WHITE","RACE_HISPANIC","CONSERVATIVE",
                                               "INFO_TV","INFO_SOCIAL","INFO_GOV","INFO_NGO","INFO_WORD","INFO_OWN",
                                               "GROUP_INVOLVEMENT","GROUP_ENVPRT","ACTIVE_AVG","PEB_AVG","CHNG_AVG",
                                               "ATTITUDE_OPPSAT","ATTITUDE_CHNGFW","ATTITUDE_CHNGRN","ATTITUDE_CHNGTP","ATTITUDE_CCTHRT",
                                               "PRIORITIZE_ENVIRON","JUSTICE","NRELATEDNESS","PIDENTITY","SOLASTALGIA",
                                               "HOPEPATH_SELFEFF","HOPEPATH_RESPEFF","SWELLBEING","SWELLBEING_SCORE","MHI_SCORE",
                                               "ECOANXIETY_SCORE","ECOANXIETY_CAT","ECOANXIETY_CAT3","ECOANXIETY","KNOWLEDGE_SCORE")]
finaldata_public$AUDIENCE <- "Public"
finaldata_innetwork$AUDIENCE <- "In-network"
finaldata_innetwork <- finaldata_innetwork[-(153),]
finaldata_all <- rbind(finaldata_public,finaldata_innetwork)

library(gridExtra)

box_SWELLBEING_SCORE <- ggplot(finaldata_all, aes(x=AUDIENCE, y=SWELLBEING_SCORE)) + 
  geom_boxplot(notch=TRUE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

box_ECOANXIETY_SCORE <- ggplot(finaldata_all, aes(x=AUDIENCE, y=ECOANXIETY_SCORE)) + 
  geom_boxplot(notch=TRUE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

box_PEB_AVG <- ggplot(finaldata_all, aes(x=AUDIENCE, y=PEB_AVG)) + 
  geom_boxplot(notch=TRUE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

box_JUSTICE <- ggplot(finaldata_all, aes(x=AUDIENCE, y=JUSTICE)) + 
  geom_boxplot(notch=TRUE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

box_NRELATEDNESS <- ggplot(finaldata_all, aes(x=AUDIENCE, y=NRELATEDNESS)) + 
  geom_boxplot(notch=TRUE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

box_CHNG_AVG <- ggplot(finaldata_all, aes(x=AUDIENCE, y=CHNG_AVG)) + 
  geom_boxplot(notch=TRUE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

box_PIDENTITY <- ggplot(finaldata_all, aes(x=AUDIENCE, y=PIDENTITY)) + 
  geom_boxplot(notch=TRUE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

box_SOLASTALGIA <- ggplot(finaldata_all, aes(x=AUDIENCE, y=SOLASTALGIA)) + 
  geom_boxplot(notch=TRUE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

box_HOPEPATH_SELFEFF <- ggplot(finaldata_all, aes(x=AUDIENCE, y=HOPEPATH_SELFEFF)) + 
  geom_boxplot(notch=TRUE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

box_ATTITUDE_OPPSAT <- ggplot(finaldata_all, aes(x=AUDIENCE, y=ATTITUDE_OPPSAT)) + 
  geom_boxplot(notch=TRUE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

box_comparisons <- grid.arrange(box_ECOANXIETY_SCORE,box_SWELLBEING_SCORE,box_PEB_AVG,box_JUSTICE,box_NRELATEDNESS,box_CHNG_AVG,box_PIDENTITY,box_SOLASTALGIA,box_HOPEPATH_SELFEFF,box_ATTITUDE_OPPSAT, ncol=5)




all_AGE <- finaldata_all %>%
  group_by(AUDIENCE,AGE) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/sum(count))*100)

graph_AGE <- ggplot(all_AGE, aes(x=AGE, y=pct, fill=AUDIENCE)) + 
  geom_bar(position="dodge", stat="identity") + 
  ylim(0,100) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

all_HHINCOME <- finaldata_all %>%
  group_by(AUDIENCE,HHINCOME) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/sum(count))*100)

graph_HHINCOME <- ggplot(all_HHINCOME, aes(x=HHINCOME, y=pct, fill=AUDIENCE)) + 
  geom_bar(position="dodge", stat="identity") + 
  ylim(0,100) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

all_EDUCATION <- finaldata_all %>%
  group_by(AUDIENCE,EDUCATION) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/sum(count))*100)

graph_EDUCATION <- ggplot(all_EDUCATION, aes(x=EDUCATION, y=pct, fill=AUDIENCE)) + 
  geom_bar(position="dodge", stat="identity") + 
  ylim(0,100) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

all_OWN <- finaldata_all %>%
  group_by(AUDIENCE,OWN) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/sum(count))*100)

graph_OWN <- ggplot(all_OWN, aes(x=OWN, y=pct, fill=AUDIENCE)) + 
  geom_bar(position="dodge", stat="identity") + 
  ylim(0,100) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

all_RACE_WHITE <- finaldata_all %>%
  group_by(AUDIENCE,RACE_WHITE) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/sum(count))*100)

graph_RACE_WHITE <- ggplot(all_RACE_WHITE, aes(x=RACE_WHITE, y=pct, fill=AUDIENCE)) + 
  geom_bar(position="dodge", stat="identity") + 
  ylim(0,100) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

all_GROUP_ENVPRT <- finaldata_all %>%
  group_by(AUDIENCE,GROUP_ENVPRT) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/sum(count))*100)

graph_GROUP_ENVPRT <- ggplot(all_GROUP_ENVPRT, aes(x=GROUP_ENVPRT, y=pct, fill=AUDIENCE)) + 
  geom_bar(position="dodge", stat="identity") + 
  ylim(0,100) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())



kruskal.test(ECOANXIETY_SCORE ~ AUDIENCE, data = finaldata_all)
kruskal.test(SWELLBEING_SCORE ~ AUDIENCE, data = finaldata_all)
kruskal.test(PEB_AVG ~ AUDIENCE, data = finaldata_all)
kruskal.test(JUSTICE ~ AUDIENCE, data = finaldata_all)
kruskal.test(NRELATEDNESS ~ AUDIENCE, data = finaldata_all)
kruskal.test(CHNG_AVG ~ AUDIENCE, data = finaldata_all)
kruskal.test(PIDENTITY ~ AUDIENCE, data = finaldata_all)
kruskal.test(SOLASTALGIA ~ AUDIENCE, data = finaldata_all)
kruskal.test(HOPEPATH_SELFEFF ~ AUDIENCE, data = finaldata_all)
kruskal.test(ATTITUDE_OPPSAT ~ AUDIENCE, data = finaldata_all)

AGEcat <- table(finaldata_all$AUDIENCE,finaldata_all$AGE)
AGEcat
fisher.test(AGEcat,simulate.p.value=TRUE)
EDUCATIONcat <- table(finaldata_all$AUDIENCE,finaldata_all$EDUCATION)
EDUCATIONcat
fisher.test(EDUCATIONcat)
HHINCOMEcat <- table(finaldata_all$AUDIENCE,finaldata_all$HHINCOME)
HHINCOMEcat
fisher.test(HHINCOMEcat,simulate.p.value=TRUE)
OWNcat <- table(finaldata_all$AUDIENCE,finaldata_all$OWN)
OWNcat
fisher.test(OWNcat)
RACE_WHITEcat <- table(finaldata_all$AUDIENCE,finaldata_all$RACE_WHITE)
RACE_WHITEcat
fisher.test(RACE_WHITEcat)
GROUP_ENVPRTcat <- table(finaldata_all$AUDIENCE,finaldata_all$GROUP_ENVPRT)
GROUP_ENVPRTcat
fisher.test(GROUP_ENVPRTcat)


########## SPATIAL ##################

zipsummaries <- finaldata_all %>%
  group_by(ZIPCODE) %>%
  summarise(count = n())


zippublic <- finaldata_public %>%
  group_by(ZIPCODE) %>%
  summarise(count = n(),
            EAmean = mean(ECOANXIETY_SCORE),
            mod2sev = sum(HighEA)) %>%
  mutate(mod2sevPCT = mod2sev/count*100)
