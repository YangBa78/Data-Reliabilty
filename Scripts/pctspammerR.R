library(lme4)
library(influence.ME)
require(Matrix)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(tidyverse)


######## simulation for each type
set.seed(222)
spam11 = sample_random(50)
spam11$workerid = rep(201:250, each=n_image)
dim(spam11)


spam21 = sample_primary1(40)
dim(spam21)[1]/80
spam21$workerid = rep(201:238, each=n_image)

spam31 = sample_primary2(40)
dim(spam31)[1]/80
spam31$workerid = rep(301:335, each=n_image)


spam41 = sample_strong_pattern1(25)
spam41$workerid = rep(401:425, each=n_image)
# 
spam51 = sample_strong_pattern2(25)
spam51$workerid = rep(501:525, each=n_image)

norm = sample_normal(108)
norm$workerid = rep(13:120, each=n_image)

data = rbind(spam11, spam21, spam31, spam41, spam51, norm)

data = rbind(norm, spam41)


p = glmer(as.factor(decision) ~ (1 | workerid ) + (1 | imageid),      
          data = data, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)


p = glmer(as.factor(decision) ~ (1 | workerid ) + (1 | imageid),      
          data = norm, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)

p.t = sum(data.frame(VarCorr(p))$vcov)
ai = data.frame(VarCorr(p))$vcov[1] / p.t
ai  # 0.01595842




# 
n <- seq(1,30, by=1)
aii.rg <- c()
aii.pc <- c()
aii.sp <- c()


for (x in n) {
  i = round((x/100*108)/(1-x/100))
  r = 80*i
  rg = spam11[c(1:r),]
  if (i%%2 ==0){
    r = r/2
    pc1 = spam21[c(1:r),]
    pc2 = spam31[c(1:r),]
    sp1 = spam41[c(1:r),]
    sp2 = spam51[c(1:r),]
  }else{
    r1 = (round(i/2)+1)*80
    r2 = ifelse((round(i/2)-1)*80<0, 0, (round(i/2)-1)*80)
    pc1 = spam21[c(1:r1), ]
    pc2 = spam31[c(1:r2), ]
    sp1 = spam41[c(1:r1), ]
    sp2 = spam51[c(1:r2), ]
  }

  data1  = rbind(norm, rg)
  data2  = rbind(norm, pc1, pc2)
  data3  = rbind(norm, sp1, sp2)
  
  p1 = glmer(as.factor(decision) ~ (1 | workerid ) + (1 | imageid),      
            data = data1, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
  p.t1 = sum(data.frame(VarCorr(p1))$vcov)
  aii.rg =append(aii.rg, data.frame(VarCorr(p1))$vcov[1] / p.t1)
  
  
  p2 = glmer(as.factor(decision) ~ (1 | workerid ) + (1 | imageid),      
             data = data2, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
  p.t2 = sum(data.frame(VarCorr(p2))$vcov)
  aii.pc =append(aii.pc, data.frame(VarCorr(p2))$vcov[1] / p.t2)
  
  
  p3 = glmer(as.factor(decision) ~ (1 | workerid ) + (1 | imageid),      
             data = data3, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
  p.t3 = sum(data.frame(VarCorr(p3))$vcov)
  aii.sp =append(aii.sp, data.frame(VarCorr(p3))$vcov[1] / p.t3)
}



df.c = data.frame(n, aii.rg, aii.pc, aii.sp)
df.c = rbind(c(0, 0.1072939, 0.1072939, 0.1072939), df.c)

df.c

colnames(df.c) = c('% of spammers', 'randome guessing', 'primary choice', 'repeated pattern')

df.cc <- df.c%>%
  select('% of spammers', 'randome guessing', 'primary choice', 'repeated pattern') %>%
  gather(key = "spammers", value = "spammer index", -"% of spammers")

df.cc


ggplot(df.cc, aes(x = `% of spammers`, y = `spammer index`)) + 
  geom_line(aes(color = spammers)) +         
  scale_color_manual(name= 'spammers', labels=c('primary choice', 'randome guessing', 'repeated pattern'),
                      values=c("#FC4E07", "#0073C2FF", "#00AFBB"))


write.xlsx(data, file = "~/Dropbox (ASU)/Code/Data-Reliability/Simulated data/pct_dataSimu.xlsx", append = FALSE)



