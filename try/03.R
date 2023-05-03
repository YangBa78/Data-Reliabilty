library(simstudy)
library(lme4)
library(gmodels)
library(tidyverse)
library(msm)

# logit curve
x <- seq(-4, 4, length.out = 100)
p.1 <- 1/(1 + exp(-x))
plot(x, p.1, type = "l")



# logit(y_ij) = random_eff_i + random_eff_j + random_eff_ij 
# list of behaviors of spammers - define spammers
# 1---0----
# "easy vs hard", "not related"
# deletion residuals regression 
# test,  



#set.seed(123)
n_worker <- 30
n_image <-40


# image
def.image <- defData(varname = "image_eff", dist = "normal", formula = 0, variance = 10,
                     id = "imageid")
def.image <- defData(def.image, varname = "nWorker", dist = "nonrandom", formula = n_worker)
#def.image <- defData(def.image, varname = 'trueLable', dist = "binary", formula = 0.5)

#set.seed(123)
dtImage <- genData(n_image, def.image)

dtImage$task <- ifelse(abs(dtImage$image_eff)< median(abs(dtImage$image_eff)), 'hard', 'easy')

dtImage$trueLable <- ifelse(dtImage$image_eff<0, 0, 1)

#dt$true <- ifelse(0>=dt$image_eff>=-1.098612, 0)

table(dtImage$task)
table(dtImage$trueLable)
table(dtImage$trueLable, dtImage$task)


dtImage

dtTime <- addPeriods(dtImage,
                     nPeriods = n_worker, idvars = "imageid",
                     timevarName = "t")
dtTime
head(dtTime, 31)

dtTime$workerid <- rep(1:n_worker, n_image)
colnames(dtTime)

dtTime <- dtTime[, c('imageid', 'image_eff', 'nWorker', 'trueLable', 'task', 'workerid')]

dtTime

# worker
def.worker <- defData(varname = "worker_eff", dist = "normal", formula = 0, variance = 3, 
                      id = "workerid")

dtWorker <- genData(n_worker, def.worker)
dtWorker
#
dtWorker[order(dtWorker$worker_eff, decreasing = TRUE), ]



dd <- dtTime%>% left_join(dtWorker, by = "workerid")
head(dd, 31)


# response variable
def.d <- defDataAdd(varname = "decision", formula = "-0.5 + image_eff + worker_eff", 
                    dist="binary", link = "logit")

dt <- addColumns(def.d, dd)
dt


# lazy spammer no.21
dt[dt$workerid==20, ]
dt[dt$workerid==1, ]
dt[dt$workerid==3, ]


fit <- glmer(as.factor(decision) ~ (1 | workerid) + (1 | imageid),   
             data = dt, family = binomial)
summary(fit)


# analysis 
table(dt$imageid, dt$decision)
table(dt$workerid, dt$decision)
table(dt$worker_eff, dt$decision)



############### introduce spammers ##########################



# random guess
newT <- dtImage
p.0 <- 0.3
newT$decision <- NA
#newT$correct <- NA
newT$decision[newT$trueLable == "same"] <- 
  sample(c(0,1), sum(newT$trueLable == "same"), prob=c(0.3, 1-0.3), rep=TRUE)
newT$decision[newT$trueLable == "different"] <- 
  sample(c(0,1), sum(newT$trueLable == "different"), prob=c(0.3, 1-0.3), rep=TRUE)

newT
xtabs(~decision + trueLable, data = newT) %>%
  prop.table() %>%
  round(4) %>%
  addmargins()

#test if P(decision = 1, different) = P(decision=1)*P(different)
0.225*0.5 == 0.125
newT$workerid <- 31
newT$worker_eff <- NA
newT <- newT[, c("imageid", "image_eff", "nWorker", "trueLable", "task", "workerid",  "worker_eff",
                 "decision" )]

newdt <- rbind(dt, newT)


fit1<- glmer(as.factor(decision) ~ (1 | workerid) + (1 | imageid),   
             data = newdt, family = binomial)
summary(fit1)


# repeat pattern
newT.1 <- dtImage
newT.1$workerid <- 32
newT.1$worker_eff <- NA
newT.1$decision <- rep(c(0,1), times = n_image/2)  

newT.1

newT.1 <- newT.1[, c("imageid", "image_eff", "nWorker", "trueLable", "task", "workerid",  "worker_eff",
                     "decision" )]

newdt.2 <- rbind(dt, newT.1)


fit2<- glmer(as.factor(decision) ~ (1 | workerid) + (1 | imageid),   
             data = newdt.2, family = binomial)
summary(fit2)



# fast deceivers as filters
e_norm <- 10
v_norm <- 3
e_spam <- 2
v_spam <- 1
dt$duration <- NA
dt <-select(dt, -duration)


for (x in unique(dt$imageid)){
  if (abs(unique(dt[dt$workerid==x, ]$worker_eff))>2){
    dt[dt$workerid==x,]$duration <- rtnorm(n = n_image, 
                                           mean = e_spam,                                              
                                           sd = sqrt(v_spam),
                                           lower = 0.8) %>%
      round(3)
  }else{
    dt[dt$workerid==x,]$duration <- rtnorm(n = n_image, 
                                           mean = e_norm,
                                           sd = sqrt(v_norm), 
                                           lower = 6) %>%
      round(3)
  }
}





newdtC <- rbind(newdt, newT.1)

#spammer: 1, 3, 20, 31, 32



#############Jan 31############ 
# spammer dist, hypothesis testing 
# append(disagreerate, min(table(simulated_df[simulated_df$workerid==x, ]$decision))/sum(table(simulated_df[simulated_df$workerid==x, ]$decision)))

dt$converted_decision <- ifelse(dt$decision==1, 'different', 'same')
newdtC$converted_decision <- ifelse(newdtC$decision==1, 'different', 'same')


table(dt[dt$workerid == 21, ]$decision)/sum(table(dt[dt$workerid == 21, ]$decision))



# formula: -0.5 + image_eff + worker_eff

sum(diag(table(dt[dt$workerid == 1, ]$trueLable, dt[dt$workerid == 1, ]$converted_decision))) / sum(table(dt[dt$workerid == 1, ]$trueLable, dt[dt$workerid == 1, ]$converted_decision))

sum(table(dt[dt$workerid == 1, ]$trueLable, dt[dt$workerid == 1, ]$converted_decision))

dt[31,]$trueLable == dt[31,]$converted_decision


# correct: 1, wrong: 0
newdtC$error<- ifelse(newdtC$trueLable == newdtC$converted_decision, 0, 1)
dt$error<- ifelse(dt$trueLable == dt$converted_decision, 0, 1)


plot(dt)

plot(dt[dt$workerid == 17, ]$image_eff, dt[dt$workerid == 17, ]$error)

plot(dt[dt$workerid == 30, ]$image_eff, dt[dt$workerid == 30, ]$error2)


# logistic value 
dt$b <- -0.5 + dt$image_eff + dt$worker_eff
dt$prob <- 1/(1 + exp(-dt$b))
dt[dt$decision==1,]


dt$mod_decision <- ifelse(dt$prob>=0.5, 1, 0)
dt$conv_mod_decision <- ifelse(dt$mod_decision ==1, 'different', 'same')
dt


table(dt$imageid, dt$decision)
table(dt$workerid, dt$conv_mod_decision)
table(dt$worker_eff, dt$decision)

dt[dt$prob<0.5, ]
dt[dt$prob>=0.5, ]
dt[dt$decision==1,][dt[dt$decision==1,]$prob>=0.5, ]
dt[dt$decision==1,]



##### work on newdtC
newdtC$b <- -0.5 + newdtC$image_eff + newdtC$worker_eff
newdtC$prob <- 1/(1 + exp(-newdtC$b))
newdtC[newdtC$decision==1,]


# back to defult decision
dt$conv_label <- ifelse(dt$trueLable=='different', 1, 0)
dt$error3 <- abs(dt$conv_label - dt$prob)

# spammers 
plot(dt[dt$workerid == 30, ]$image_eff, dt[dt$workerid == 30, ]$error3)
plot(dt[dt$workerid == 23, ]$image_eff, dt[dt$workerid == 23, ]$error3)
plot(dt[dt$workerid == 28, ]$image_eff, dt[dt$workerid == 28, ]$error3)

# potential spammers but with the opposite behavior style 
plot(dt[dt$workerid == 16, ]$image_eff, dt[dt$workerid == 16, ]$error3)

# normal worker
plot(dt[dt$workerid == 18, ]$image_eff, dt[dt$workerid == 18, ]$error3)
plot(dt[dt$workerid == 5, ]$image_eff, dt[dt$workerid == 5, ]$error3)



# x axis = prob: spammers
plot(1/(1 + exp(-dt[dt$workerid == 21, ]$image_eff)), dt[dt$workerid == 21, ]$error3)
plot(1/(1 + exp(-dt[dt$workerid == 23, ]$image_eff)), dt[dt$workerid == 23, ]$error3)
plot(1/(1 + exp(-dt[dt$workerid == 28, ]$image_eff)), dt[dt$workerid == 28, ]$error3)

# x axis = prob: not a spammer but with the similar/opposite behavior style 
plot(1/(1 + exp(-dt[dt$workerid == 16, ]$image_eff)), dt[dt$workerid == 16, ]$error3)
plot(1/(1 + exp(-dt[dt$workerid == 5, ]$image_eff)), dt[dt$workerid == 5, ]$error3)
plot(1/(1 + exp(-dt[dt$workerid == 2, ]$image_eff)), dt[dt$workerid == 2, ]$error3)


# normal worker
plot(1/(1 + exp(-dt[dt$workerid == 18, ]$image_eff)), dt[dt$workerid == 18, ]$error3)


df <- as.data.frame.matrix(table(dt$workerid, dt$decision))
df$workerid <- rownames(table(dt$workerid, dt$decision))
df$workerid <- as.integer(df$workerid )
df <- df %>% left_join(dtWorker, by = "workerid")
colnames(df) <- c("same", "different", "workerid", "worker_eff")


df[order(df$worker_eff),]


# perform Kolmogorov-Smirnov test
ks.test(dt[dt$workerid == 30, ]$error3, dt[dt$workerid == 15, ]$error3)


#chisq.test(dt[dt$workerid == 30, ]$error, dt[dt$workerid == 31, ]$error)

plot(newdtC[newdtC$workerid == 31, ]$image_eff, newdtC[newdtC$workerid == 31, ]$error)

chisq.test(newdtC[newdtC$workerid ==30, ]$error, newdtC[newdtC$workerid == 18, ]$error)


for(i in 1:30){
  print(sum(dt[dt$workerid==i,]$correct)/40)
}

sum(dt[dt$workerid==28,]$correct)/40


#### re-assign true label#####, -1.098612

dt$true <- NA
for (i in dt)
  dt$true <- ifelse(0>=dt$image_eff>=-1.098612, 0)