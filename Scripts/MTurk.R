require(GGally)
require(reshape2)
require(lme4)
require(compiler)
require(parallel)
require(boot)
require(lattice)

library(car)
library(tidyverse) 
library(sjPlot)    
library(effects)   
library(lmerTest)  
library(report)   
library(emmeans)   
library(knitr)     
library(sjstats)   
library(caret)   
library(ggplot2)
library(broom)
library(arm)

#install.packages('lm')
library(readxl)
Mturk<- read_excel("Dropbox (ASU)/Code/8.3/MtutkP.xlsx")
View(Mturk)

Mturk$decision = MTurkPerformanceData$`Final Decision`

Mturk$`Correct Prediction` <- ifelse(Mturk$`Correct Prediction`=='Error', 1, 0)

colnames(Mturk)[3] <- 'assgID'
colnames(Mturk)[4] <- 'interaction'
colnames(Mturk)[5] <- 'deferral'
colnames(Mturk)[6] <- 'task'
colnames(Mturk)[11] <- 'error'

Mturk = MTurkPerformanceData

Mturk$decision <- ifelse(Mturk$decision=='different', 1, 0)


mod_m = glmer(decision ~ (1 | assgID) + (1 | TID)  + (1 | assgID:TID)  + (1 | assgID:task) + (1 | task/TID),  
           data = Mturk, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(mod_m)



colnames(Mturk)

m1 = glmer(error ~ (1 | assgID) + (1 | TID), 
             data = Mturk, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(m1)


m2 = glmer(error ~ (1 | assgID) + (1 | TID) + (1 | task/TID), 
             data = Mturk, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(m2)
anova(mod4, mod6)


m3 = glmer(error ~ (1 | assgID)  + (1 | task/TID), 
           data = Mturk, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(m3)


m4 = glmer(error ~ (1 | assgID) + (1 | TID)  + (1 | assgID:TID)  + (1 | assgID:task) + (1 | task/TID),  
             data = Mturk, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(m4)


m5 = glmer(error ~ (1 | assgID) + (1 | TID)  + (1 | assgID:TID)  + (1 | assgID:task) + (1 | task/TID) + (1 | interaction/assgID ) + (1 | interaction : TID ),  
           data = Mturk, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(m5)



mod3 = glmer(error ~ (1 | assgID) + (1 | TID) + (1 | task/TID) + (1 | deferral/assgID), 
             data = Mturk, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(mod3)


mod4 = glmer(error ~ (1 | assgID) + (1 | TID) + (1 | interaction) + (1 | task/TID),  
             data = Mturk, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(mod4)


mod5 = glmer(error ~ (1 | assgID) + (1 | TID) + (1 | interaction) + (1 | task/TID:assgID),  
             data = Mturk, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(mod5)


mod6 = glmer(error ~ (1 | assgID) + (1 | TID) + (1 | interaction) + (1 | task/TID:TID),  
             data = Mturk, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(mod6)


mod7 = glmer(error ~ (1 | assgID) + (1 | TID) + (1 | interaction) + (1 | assgID:TID) + (1 | TID:interaction) + (1 | assgID:interaction) + (1 | assgID:task) + (1 | interaction:task) + (1 | task/TID),  
             data = Mturk, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(mod7)


mod8 = glmer(error ~ (1 | assgID) + (1 | TID)  + (1 | assgID:TID)  + (1 | assgID:task) + (1 | task/TID),  
             data = Mturk, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(mod8)


3.29/(77*160) + 0.1071/320 + 0.1198/160 + 0.2444/(77*2) + 1.2032/2
# ICC
0.5188/0.6045375

#intra-rater
(0.5188 + 0) / (0.5188 + 0 + 1.2032 + 3.29)

#inter-rater
(0.5188 + 0.2444) / (0.5188 + 0 + 0.2444 + 3.29)

