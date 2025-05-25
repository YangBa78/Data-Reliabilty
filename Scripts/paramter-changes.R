library(lme4)
library(readxl)
library(influence.ME)
library(plyr)
library(lmerTest)
#library(DHARMa)
library(car)
library(partR2)
library(geosphere)
library(ade4)
library(dplyr)



########################## Airport data ##################################

df = read.csv("DF_Facewise_Airports1.csv")
#View(df)
dim(df)

df$acc = ifelse(df$True.Label==df$Final.Decision, 1, 0)


RM.id = c(119,
       217,
       227,
       234,
       248,
       251,
       305,
       314,
       316,
       320,
       349,
       350,
       352,
       355,
       374,
       381)


RM.df = subset(df, !(ID %in% RM.id)) # use 1


p = glmer(as.factor(acc) ~ Easyness*Condition.Facewise + (1 | ID ) + (1 | Pair),      
          data = df, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)

aov = anova(p)
show_tests(aov)

Anova(p,type="III")
Anova(Rp,type="III")


Rp = glmer(as.factor(acc) ~ Easyness*Condition.Facewise + (1 | ID ) + (1 | Pair),      
           data = RM.df, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(Rp)


#anova(p, Rp, test="Chisq")


p.t = sum(data.frame(VarCorr(Rp))$vcov)
aii = data.frame(VarCorr(Rp))$vcov[2] / p.t
aii # 0.1663119

#decrease  SI


#F test
RSS1 = sum(residuals(p)^2)
RSS2 = sum(residuals(Rp)^2)
df1 = df.residual(p)
df2 = df.residual(Rp)

fstat = ((RSS1 - RSS2)/(df1-df2)) / (RSS2/df2) 

pf(fstat, df1, df2, lower.tail = FALSE)

AIC(p)
AIC(Rp)
BIC(p)
BIC(Rp)


#################################################### MTurk data ###################################


mt <- read.csv("Data/MTurkPerformanceData.csv")

colnames(mt)

mt = mt[,c('Assignment.ID','Completion.Time','Final.Decision', 'Coded.Prediction', 'Task.difficulty', 'TID', 'Condition', 'Condition.Type', 'Interaction.Structure', 'Class')]
colnames(mt)[1] = 'workerid'
colnames(mt)[2] = 'time'
colnames(mt)[3] = 'decision'
colnames(mt)[4] = 'accuracy'
colnames(mt)[5] = 'task'
colnames(mt)[6] = 'image'
#mt


RM.id1 = c(
  0,   1,   2,   3,  10,  11,  30,  35,  44,  50,  53,  55,  56,
  59,  62,  65,  67,  72,  73,  83, 100, 106, 108, 109, 113, 118,
  121, 123, 124, 126, 137, 141, 143, 151, 155
)



RM.id2 = c(
  1,  62,  72,  83, 118, 143, 124, 109, 155
)


RM.id3 = c(
  0,  11, 151, 141, 123,  73,  59, 137, 126
)

RM.id = setdiff(RM.id1, RM.id3)


p = glmer(as.factor(accuracy) ~ task*Interaction.Structure +  (1 | workerid ) + (1 | image),      
          data = mt, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)




anova(p)
#'Condition', 'Condition.Type', 'Interaction.Structure', 'Class'

RM.df = subset(mt, !(workerid %in% RM.id))
#RM.df = subset(mt, !(workerid %in% RM.id1))
#RM.df = subset(mt, !(workerid %in% RM.id2))

Rp = glmer(as.factor(accuracy) ~  task*Interaction.Structure +  (1 | workerid ) + (1 | image),      
          data = RM.df, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(Rp)


anova(Rp)

Anova(p,type="III")
Anova(Rp,type="III")

# mt$Class = mapvalues(mt$Class, from = c('difficulty match'), to = c('difficult match'))
# table(mt$Class)


#F test
RSS1 = sum(residuals(p)^2)
RSS2 = sum(residuals(Rp)^2)
df1 = df.residual(p)
df2 = df.residual(Rp)

fstat = ((RSS1 - RSS2)/(df1-df2)) / (RSS2/df2) 

pf(fstat, df1, df2, lower.tail = FALSE)

AIC(p)
AIC(Rp)
BIC(p)
BIC(Rp)


########################################################## Prolific data ###########################

pl <- read_excel("Data/Prolific.xlsx")
pl = pl[pl['WorkerId']!='2f33f23f23',]

colnames(pl)

pl = pl[,c('WorkerId','Time Spend','Final Decision', 'error', 'task', 'Image 1', 'Difficulty', 'Condition Type')]
colnames(pl)[1] = 'workerid'
colnames(pl)[2] = 'time'
colnames(pl)[3] = 'decision'
colnames(pl)[6] = 'image'
colnames(pl)[8] = 'condition'

pl$Difficulty = mapvalues(pl$Difficulty, from = c('difficulty match'), to = c('difficult match'))
table(pl$Difficulty)


pl$diff = mapvalues(pl$Difficulty, from = c('difficult match', 'difficult mismatch', 'easy match',  'easy mismatch'), 
                                   to = c('difficult', 'difficult', 'easy', 'easy'))
table(pl$diff)



p = glmer(as.factor(error) ~ diff*condition + (1 | workerid ) + (1 | image),      
          data = pl, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)


RM.id = c(102,  74, 101, 136,  14, 130, 107, 117,  3)


RM.df = subset(pl, !(workerid %in% RM.id))


Rp = glmer(as.factor(error) ~ diff*condition + (1 | workerid ) + (1 | image),      
          data = RM.df, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(Rp)



Anova(p,type="III")
Anova(Rp,type="III")

RSS1 = sum(residuals(p)^2)
RSS2 = sum(residuals(Rp)^2)
df1 = df.residual(p)
df2 = df.residual(Rp)

fstat = ((RSS1 - RSS2)/(df1-df2)) / (RSS2/df2) 

pf(fstat, df1, df2, lower.tail = FALSE)

AIC(p)
AIC(Rp)
BIC(p)
BIC(Rp)


p.t = sum(data.frame(VarCorr(Rp))$vcov)
aii = data.frame(VarCorr(Rp))$vcov[1] / p.t
aii # 0.1663119

p.t = sum(data.frame(VarCorr(p))$vcov)
aii = data.frame(VarCorr(p))$vcov[1] / p.t
aii # 0.1663119



#Pl: 0.01674252 - 0.007096836 - 0.576119
#MT: 0.1636128 - 0.1075257 - 0.3428039
#fw: 0.1356645 -  0.1101016 - 0.1884273



mantel.rtest(dist(vcov(p)), dist(vcov(Rp)),nrepet = 9999)



##################### interaction plot MTurk 


p = glmer(as.factor(accuracy) ~ task*Interaction.Structure +  (1 | workerid ) + (1 | image),      
          data = mt, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)


Rp = glmer(as.factor(accuracy) ~  task*Interaction.Structure +  (1 | workerid ) + (1 | image),      
           data = RM.df, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(Rp)





interaction.plot(
  x.factor = mt$Interaction.Structure,
  trace.factor = mt$task,
  response = mt$accuracy,
  fun = mean,
  ylab = "Accuracy",
  xlab = "Interaction Structure",
  trace.label = "Task Difficult",
  col = c("#0198f9", "#f95801"),
  lyt = 2,
  lwd = 4
)
title(main = "MTurk before" )
  
interaction.plot(
  x.factor = RM.df$Interaction.Structure,
  trace.factor = RM.df$task,
  response = RM.df$accuracy,
  fun = mean,
  ylab = "Accuracy",
  xlab = "Interaction.Structure",
  trace.label = "Task Difficulty",
  col = c("#0198f9", "#f95801"),
  lyt = 2,
  lwd = 4
)

title(main = "MTurk after" )
