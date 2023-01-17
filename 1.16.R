library(simstudy)
library(lme4)
library(gmodels)
library(tidyverse)

# logit curve
x <- seq(-4, 4, length.out = 100)
p <- 1/(1 + exp(-x))
plot(x, p, type = "l")


#set.seed(123)
n_worker <- 30
n_image <-20

# image
def.image <- defData(varname = "image_eff", dist = "normal", formula = 0, variance = 2,
                     id = "imageid")
def.image <- defData(def.image, varname = "nWorker", dist = "nonrandom", formula = n_worker)

#set.seed(123)
dtImage <- genData(n_image, def.image)

dtImage$task <- ifelse(abs(dtImage$image_eff)< median(abs(dtImage$image_eff)), 'hard', 'easy')
dtImage


dtTime <- addPeriods(dtImage,
                     nPeriods = n_worker, idvars = "imageid",
                     timevarName = "t")
dtTime
head(dtTime, 31)

dtTime$workerid <- rep(1:n_worker, n_image)
colnames(dtTime)

dtTime <- dtTime[, c('imageid', 'image_eff', 'nWorker', 'task', 'workerid')]

# worker
def.worker <- defData(varname = "worker_eff", dist = "normal", formula = 0, variance = 10, 
                      id = "workerid")

dtWorker <- genData(n_worker, def.worker)
dtWorker
#
dtWorker[order(dtWorker$worker_eff, decreasing = TRUE), ]



dd <- dtTime%>% left_join(dtWorker, by = "workerid")
head(dd, 31)



# task: worker interaction
n_inter <- 2* n_worker
def.task <- defData(varname = "wk_tsk_eff", dist = "normal", formula = 0, variance = 1, 
                    id = "interactionid")

dtWkTk <- genData(n_inter, def.task)
dd$int_id <- paste0(dd$task, dd$workerid)
dd
dtWkTk$int_id <- sample(unique(dd$int_id))
dtWkTk

dd.in <- dd%>% left_join(dtWkTk, by = "int_id")
dt


# response variable
def.d <- defDataAdd(varname = "decision", formula = "-0.5 + image_eff + worker_eff + wk_tsk_eff", 
                    dist="binary", link = "logit")

dt <- addColumns(def.d, dd.in)



# analysis 
table(dt$imageid, dt$decision)
table(dt$workerid, dt$decision)
table(dt$worker_eff, dt$decision)

spm <-c(3.54372239382221, 2.80182904374328, 2.80182904374328, 1.9002425329571, 1.52315268062746)

dt[dt$worker_eff=='2.80182904374328', ]$workerid # id: 6
dt[dt$worker_eff=='3.54372239382221', ]$workerid # id: 9
dt[dt$worker_eff=='1.9002425329571', ]$workerid # id: 13
dt[dt$worker_eff=='1.52315268062746', ]$workerid # id: 15
dt[dt$worker_eff=='2.18907548465503', ]$workerid # id: 30


# potentials
spammer <- c(6, 9, 13, 15, 30)

for (x in spammer){
  print(unique(dt[dt$workerid==x, ]$wk_tsk_eff))
}


# compare if the same direction, exclude 9 and 15
# the spammer is 6, 13, 30

# 
im <- c(seq(1:30))
normal <- setdiff(im, spammer)
length(normal)
for (i in normal){
  print(unique(dt[dt$workerid==i, ]$wk_tsk_eff))
}

5/5: #100%
  11/25: #44%
  
  
  
  CrossTable(dt$workerid, dt$decision,  prop.t=TRUE)
CrossTable(dt$imageid, dt$decision,  prop.t=TRUE)
View(dt)

#dt$pro_1 <- 1 / (1 + exp(-(-0.5 + dt$s0 + dt$c0)))


#dt$task <- ifelse(dt$task=='easy', 'hard', 'easy')
# random effects model
fit <- glmer(as.factor(decision) ~ (1 | workerid) + (1 | imageid) + (1|task:workerid),   
             data = dt, family = binomial)
summary(fit)

fit1 <- glmer(as.factor(decision) ~ (1 | task/imageid) + (1 | workerid) + (1| task:workerid),    
              data = dt, family = binomial)
summary(fit1)

fit2 <- glmer(as.factor(decision) ~ task + (1 | imageid) + (1 | workerid) + (1|task:workerid),    
              data = dt, family = binomial)
summary(fit2)

fit2.2 <- glmer(as.factor(decision) ~ task + (1 | task/imageid) + (1 | workerid) + (1|task:workerid),    
                data = dt, family = binomial)
summary(fit2.2)


# add task as predictor
b1 <- -2.8068
dd.in$task_coef <- ifelse(dd.in$task =='hard', -2.8068, 0) 

def.d2 <- defDataAdd(varname = "decision", formula = "-0.5 + task_coef*(task == 'hard') + image_eff + worker_eff + wk_tsk_eff", 
                     dist="binary", link = "logit")

dt2 <- addColumns(def.d2, dd.in)

dt2  
fit3 <- glmer(as.factor(decision) ~ task + (1 | task/imageid) + (1 | workerid) + (1|task:workerid) ,    
              data = dt2, family = binomial)
summary(fit3)

fit4 <- glmer(as.factor(decision) ~ (1 | task/imageid) + (1 | workerid) + (1|task:workerid),    
              data = dt2, family = binomial)
summary(fit4)



# interaction
# assumption: if the worker interaction random effect follow the same direction 
# of its own random effect and doesn't deviate too much, this is another evidence to confirm if he/she is a spammer
def.int <- defDataAdd(varname = "i0", dist = "normal", formula = 0, variance = 1) # add id

dtInt <- addColumns(def.int, dt)
length(unique(dtInt$i0))

def.d2 <- defDataAdd(varname = "decision2", formula = "-0.5 + s0 + c0 + i0", 
                     dist="binary", link = "logit")

newDt <- addColumns(def.d2, dtInt)
newDt

CrossTable(newDt$workerid, newDt$decision2,  prop.t=TRUE)
CrossTable(newDt$imageid, newDt$decision2,  prop.t=TRUE)

# 4, 7, 8, 9, 10
# maybe No.10 lacks of ability
dtWorker

table(newDt$workerid, newDt$decision2)

for (x in 1:10){
  print(mean(newDt[newDt$workerid==x, ]$i0))
} 


newDt
newDt[newDt$workerid==4, ]
mean(newDt[newDt$workerid==4, ]$i0)


fit1 <- glmer(as.factor(decision) ~ (1 | workerid) + (1 | imageid) + (1 | imageid: workerid),   
              data = newDt, family = binomial)
summary(fit1)


# ?? define non-capable vs spammer
meani0 <- c()
for (x in 1:10){
  meani0 <- append(meani0, mean(newDt[newDt$workerid==x, ]$i0))
} 
meani0

data.frame(dtWorker$c0, meani0, as.data.frame.matrix(table(newDt$workerid, newDt$decision2)))