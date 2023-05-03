library(simstudy)
library(lme4)
library(gmodels)
library(tidyverse)
#library(dplyr)
library(msm)


# bad: 
var_image <- 5
var_worker <- 5
aii <- 5/(5+5)
aii


n_worker <- 160
n_image <-80

# image
def.image <- defData(varname = "image_eff", dist = "normal", formula = 0, variance = var_image,
                     id = "imageid")
def.image <- defData(def.image, varname = "nWorker", dist = "nonrandom", formula = n_worker)
#def.image <- defData(def.image, varname = 'trueLable', dist = "binary", formula = 0.5)


# task
easy <- rep("easy", 40)
hard <- rep("hard", 40)
sample_task <- c(easy, hard)
sample_task <- sample(sample_task)
table(sample_task)


# GT 
same <- rep("same", 20)
diff <- rep("different", 20)
sample_vec <- c(same, diff)
sample_same <- sample(sample_vec)
sample_diff <- sample(sample_vec)
table(sample_same)


#set.seed(123)
dtImage <- genData(n_image, def.image)

dim(dtImage[dtImage$image_eff > median(dtImage$image_eff), ])

dtImage$task <- ifelse(abs(dtImage$image_eff)< median(abs(dtImage$image_eff)), 'hard', 'easy')
dtImage$trueLable <- ifelse(dtImage$image_eff > median(dtImage$image_eff), 'different', 'same')


#dtImage$task<- sample_task


# bad data max acc: 0.66
#dtImage$trueLable[dtImage$task=='easy'] <- sample_same
#dtImage$trueLable[dtImage$task=='hard'] <- sample_diff



# expert 
#dtImage$trueLable <- ifelse(dtImage$image_eff>0, 'different', 'same')
#dtImage$abs <- abs(dtImage$image_eff)
#dtImage$difficulty <- cut(dtImage$abs, breaks = 3, labels = c(0, 0.5, 1))
#dtImage$difficulty <- ifelse(dtImage$task=='easy', 0, 1)

#dtImage$trueLable <- ifelse(dtImage$trueLable==1, 'different', 'same')

# randomly assign true label
#row_indices <- 1:n_image
#selected_indices <- sample(row_indices, size = floor(n_image/2), replace = FALSE)
#label <- numeric(n_image)
#label[selected_indices] <- 'same'
#label[-selected_indices] <- 'different'
#table(label)
#dtImage$trueLable <- label

#dtImage[dtImage$image_eff>0,]
# check if data is balanced 
table(dtImage$task)
table(dtImage$trueLable)
table(dtImage$trueLable, dtImage$task)
table(dtImage$difficulty)


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
def.worker <- defData(varname = "worker_eff", dist = "normal", formula = 0, variance = var_worker, 
                      id = "workerid")

dtWorker <- genData(n_worker, def.worker)
dtWorker
#
dtWorker[order(dtWorker$worker_eff, decreasing = TRUE), ]



dd <- dtTime%>% left_join(dtWorker, by = "workerid")
head(dd, 31)


# response variable
def.d <- defDataAdd(varname = "decision", formula = "image_eff + worker_eff", 
                    dist="binary", link = "logit")
#def.d <- defDataAdd(varname = "decision", formula = "-0.5 + image_eff + worker_eff", 
#                    dist="binary", link = "logit")

dt <- addColumns(def.d, dd)
dt
sum(dt$decision)
dim(dt)

# lazy spammer no.1, 3, 20, 40/0
dt[dt$workerid==20, ]
dt[dt$workerid==1, ]
dt[dt$workerid==3, ]

# 2, 30, 113 
# 43, 101, 113

#fit <- glmer(as.factor(decision) ~ (1 | workerid) + (1 | task/imageid) + (1 | workerid: task),   
#              data = dt, family = binomial)
#summary(fit)
#fit1 <- glmer(as.factor(acc) ~ (1 | workerid) + (1 | imageid),   
#             data = dt, family = binomial)




# dt$acc <- ifelse(dt$trueLable=='same' & dt$decision ==0, 1, 0)

dt$convertT = ifelse(dt$trueLable=='same', 0, 1)

dt$acc <- ifelse(dt$convertT == dt$decision, 1, 0)


group = dt %>% group_by(workerid) %>% summarise(accuary = sum(acc)/80, 
                                                .groups = 'drop') %>%
  as.data.frame()


groupe = dt %>% group_by(task) %>% summarise(accuary = sum(acc)/80, 
                                             .groups = 'drop') %>%
  as.data.frame()
groupe

hist(group$accuary)
range(group$accuary)
mean(group$accuary)
