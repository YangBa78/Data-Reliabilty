library(simstudy)
library(lme4)
library(gmodels)
# this is a test line for github version control

# image
def.image <- defData(varname = "s0", dist = "normal", formula = 0, variance = 10,
                     id = "imageid")
def.image <- defData(def.image, varname = "nWorker", dist = "nonrandom", formula = 10)

#set.seed(123)
dtImage <- genData(15, def.image)

dtImage$task <- ifelse(abs(dtImage$s0)< median(abs(dtImage$s0)), 'hard', 'easy')
dtImage


dtTime <- addPeriods(dtImage,
                     nPeriods = 10, idvars = "imageid",
                     timevarName = "t")
dtTime
head(dtTime, 16)

dtTime$workerid <- rep(1:10, 15)
colnames(dtTime)

dtTime <- dtTime[, c('imageid', 's0', 'nWorker', 'task', 'workerid')]

# worker
def.worker <- defData(varname = "c0", dist = "normal", formula = 0, variance = 2, 
                      id = "workerid")

dtWorker <- genData(10, def.worker)
dtWorker

dd <- dtTime%>% left_join(dtWorker, by = "workerid")
head(dd, 16)


def.d <- defDataAdd(varname = "decision", formula = "-0.5 + s0 + c0", 
                 dist="binary", link = "logit")

dt <- addColumns(def.d, dd)


CrossTable(dt$workerid, dt$decision,  prop.t=TRUE)
CrossTable(dt$imageid, dt$decision,  prop.t=TRUE)
View(dt)

dt$pro_1 <- 1 / (1 + exp(-(-0.5 + dt$s0 + dt$c0)))

# logit curve
x <- seq(-4, 4, length.out = 100)
p <- 1/(1 + exp(-x))
plot(x, p, type = "l")

#dt$task <- ifelse(dt$task=='easy', 'hard', 'easy')
# random effects model
fit <- glmer(as.factor(decision) ~ (1 | workerid) + (1 | imageid),   
          data = dt, family = binomial)
summary(fit)



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


# metrics test: how they work in a high-quality data and how they work in a low-quality data


#newDt$task <- ifelse(newDt$task=='easy', 'hard', 'easy')


