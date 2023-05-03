require(lme4)
library(readxl)
#install.packages('readxl')
#library(brms)
Prolific <- read_excel("E:Dropbox (ASU)/Code/8.3/Prolific.xlsx")
View(Prolific)
install.packages("DHARMa")
library(DHARMa)



#Mturk$`Correct Prediction` <- ifelse(Mturk$`Correct Prediction`=='Error', 1, 0)

#colnames(Mturk)[7] <- 'assgID'
#colnames(Mturk)[8] <- 'interaction'
#colnames(Mturk)[5] <- 'deferral'
#colnames(Mturk)[6] <- 'task'
colnames(Prolific)[8] <- 'image1'
colnames(Prolific)[18] <- 'decision'


colnames(Prolific)
Prolific$decision <- ifelse(Prolific$decision=='different', 1, 0)


pr = prior(normal(0, 1), class = 'b')

bayesian_mixed = brm(
  gpa ~ occasion + (1 + occasion | student), 
  data  = gpa,
  prior = pr,
  cores = 4
)

mod = lmer(decision ~ (1 | WorkerId) + (1 | image1)  + (1 | task/image1),  
              data = Prolific)
summary(mod)

mod_p = glmer(decision ~ (1 | WorkerId) + (1 | image1) + (1 | image1:WorkerId) + (1 | task:WorkerId)  + (1 | task:image1) + (1 | task),  
             data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(mod_p)


mod_1 = glmer(decision ~ (1 | image1),  
              data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(mod_1)



plot(dfbetas(estex.m0))
estex.pimage <- influence(mod_p, "image1")
plot(estex.p1, which="cook",
          #cutoff=.17, sort=TRUE,
          xlab="Cook´s Distance",
          ylab="image1")


plot(estex.p1,
           which="dfbetas",
          #parameters=c(2,3),
           xlab="DFbetaS",
           ylab="image1")

dfbetas(estex.p1, parameters=c(3))

??dfbetas()

cooks.distance(estex.p1, sort=TRUE)
      
         #parameter=3,


require(lattice)
dotplot(ranef(mod_p, condVar = TRUE))
dotplot(ranef(mod_m, condVar = TRUE))
typeof(x)
unlist(ranef(mod0))



p6 = glmer(error ~ (1 | WorkerId) + (1 | image1) + (1 | image1:WorkerId) + (1 | task:WorkerId)  + (1 | task/image1) + (1 | TID/WorkerId) + (1 |TID :image1),  
           data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p6)




mod0 = glmer(decision ~ (1 | image1), 
             data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(mod0)

ranef(mod0)$image1

p1 = glmer(error ~ (1 | WorkerId) + (1 | image1), 
             data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p1)

p3 = glmer(error ~ (1 | WorkerId)  + (1 | image1) + (1 | image1:task), 
             data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)

summary(p3)




p4 = glmer(error ~ (1 | WorkerId)  + (1 | image1) + (1 | task/image1), 
           data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)

summary(p4)


p3 = glmer(error ~ (1 | WorkerId) + (1 | image1) + (1 | task), 
           data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)




w1 = glmer(decision ~ (1 | WorkerId), 
           data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(w1)


p3 = glmer(error ~ (1 | WorkerId) + (1 | task/image1), 
           data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)

summary(p3)


p4 = glmer(error ~ (1 | WorkerId) + (1 | image1) + (1 | image1:WorkerId) + (1 | task:WorkerId)  + (1 | task/image1),  
           data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p4)



#p5 = glmer(error ~ (1 | WorkerId)  + (1 | image1:WorkerId) + (1 | task:WorkerId)  + (1 | task/image1),  
 #            data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
#summary(p5)




mod4 = glmer(error ~ (1 | WorkerId) + (1 | image1) + (1 | TID) + (1 | task/image1), ,  
             data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(mod4)
anova(mod1, mod2)

#mod5 = glmer(error ~ (1 | WorkerId) + (1 | image1) + (1 | image1:WorkerId) + (1 | TID) + (1 | task/image1), ,  
#             data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
#summary(mod5)


mod5 = glmer(error ~ (1 | WorkerId) + (1 | image1) + (1 | image1:WorkerId) + (1 | task:WorkerId) + (1 | TID) + (1 | task/image1),  
             data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(mod5)


mod6 = glmer(error ~ (1 | WorkerId) + (1 | image1) + (1 | image1:WorkerId) + (1 | task:WorkerId)  + (1 | task/image1) + (1 | ),  
             data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(mod6)








3.29/(72*139) + 3.308e-13/9938 + 1.948e-01/227 + 1.063e-01/139 + 9.699e-03/72 + 2.080e-10/2

# ICC  
1.067e+01 / (1.067e+01 + 0.002086343)

#intra-rater
(1.067e+01 + 3.308e-13) / (1.067e+01 + 3.308e-13 + 2.080e-10 + 3.29) 

#inter-rater
(1.067e+01 + 9.699e-03) / (1.067e+01 + 9.699e-03 + 3.308e-13 + 3.29) 

# ICC(Intra-class correlation coefficient)
# MTurk: 0.8581767
# Prolific: 0.9998045
# Intra-rater reliability vs Inter-rater reliability
# Intra-rater reliability: MTurk: 0.1035116; Prolific: 0.7643266
# Inter-rater reliability: MTurk: 0.1882957; Prolific: 0.7644903


