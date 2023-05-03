# put it all together 
# load packages
library(lme4)
library(tidyverse)
library(simstudy)
library(influence.ME)
library(ggplot2)
library(Matrix)


# length
sort(rle(c(1,1,1, 1, 1, 1, 1, 0, 1,1, 1, 1, 1, 1, 1))[1]$lengths, decreasing =TRUE)

tes = rep(c(0,1), times = 40) 
rle(c(1,1,1, 1, 1, 1, 1, 0, 1,1, 1, 1, 1))[1]$lengths
rle(tes)[1]$lengths
hist(rle(dt[dt$workerid==140,]$decision)$lengths)
table(rle(dt[dt$workerid==14,]$decision)$lengths)
as.numeric(table(rle(dt[dt$workerid==2,]$decision)$lengths))[1]
# maximum number: how long to choose one
# 
sum(dt[dt$workerid==32,]$accuracy)/80
sum(dt[dt$workerid==140,]$accuracy)/80


acc = c()
l1 = c()
l2 = c()
l3 = c()
fre = c()
for ( i in unique(dt$workerid)){
  a = sum(dt[dt$workerid==i,]$accuracy)/80
  l = sort(rle(dt[dt$workerid==i,]$decision)$lengths, decreasing =TRUE)
  v = rle(dt[dt$workerid==i,]$decision)$lengths
  acc = append(acc, a)
  l1 = append(l1, l[1])
  l2 = append(l2, l[2])
  l3 = append(l3, l[3])
  fre = append(fre, as.numeric(table(rle(dt[dt$workerid==i,]$decision)$lengths))[1] + as.numeric(table(rle(dt[dt$workerid==i,]$decision)$lengths))[2] + as.numeric(table(rle(dt[dt$workerid==i,]$decision)$lengths))[3] + as.numeric(table(rle(dt[dt$workerid==i,]$decision)$lengths))[4])
}

fre[20]
l[20]
l
hist(l1)
l = l1+ l2+ l3
plot(acc, fre)
plot(acc, l)
cor(acc, l)
cor(acc, fre)
summary(lm(acc~ l))
abline(lm(acc~ l))


# variance
var_image <- 6
var_worker <- 0.5
aii <- 0.5/(0.5+6)
aii

n_worker <- 160
n_image <-80

#set.seed(123)
# define image task
def.image <- defData(varname = "image_eff", dist = "normal", formula = 0, variance = var_image,
                     id = "imageid")
def.image <- defData(def.image, varname = "nWorker", dist = "nonrandom", formula = n_worker)

# generate task data
dtImage <- genData(n_image, def.image)

# assign task difficulty and true label
dtImage$task <- ifelse(abs(dtImage$image_eff)< median(abs(dtImage$image_eff)), 'hard', 'easy')
# different: 1, same: 0
dtImage$trueLable <- ifelse(dtImage$image_eff > median(dtImage$image_eff), 1, 0)

# define workerid
dtTime <- addPeriods(dtImage,
                     nPeriods = n_worker, idvars = "imageid",
                     timevarName = "t")
dtTime$workerid <- rep(1:n_worker, n_image)
dtTime <- dtTime[, c('imageid', 'image_eff', 'trueLable', 'task', 'workerid')]

# generate worker data
def.worker <- defData(varname = "worker_eff", dist = "normal", formula = 0, variance = var_worker, 
                      id = "workerid")
dtWorker <- genData(n_worker, def.worker)

# combine task and worker data
dd <- dtTime%>% left_join(dtWorker, by = "workerid")

# add interaction( for random guess data, strong pattern data doesn't include this)
# define sd range
int_sd = sample(runif(160, 0.5, 2))
int_eff = c()

# for each worker i int_eff: (worker_i: easy, worker_i: hard) should be mean: 0, sd: random(range(int_sd))
for(x in int_sd){
  int_eff <- append(int_eff, rnorm(n = 2, mean = 0, sd = x)) 
}

interact = data.frame( 'workerid' = rep(1:n_worker, each = 2) ,
                        'task' = c('easy', 'hard'),
                        'int_eff' = int_eff)
# check int_eff: 
# sd(int_eff)
# mean(int_eff)
dd <- dd %>% left_join(interact, by = c("workerid", "task"))

# response variable
def.d <- defDataAdd(varname = "decision", formula = "image_eff + worker_eff + int_eff", 
                    dist="binary", link = "logit")
dt <- addColumns(def.d, dd)

dt$accuracy <- ifelse(dt$trueLable == dt$decision, 1, 0)

devide = function(a, b){
  if (b == 0){
    value = 0
  }else{
    value = a/b
  }
  return(value)
}

# define time
# cumulative accuracy 
dt$accuracy_hard = NA
dt$accuracy_easy = NA
for (i in unique(dt$workerid)){
  cnt_easy = c()
  cnt_hard = c()
  n_hard = 0
  n_easy = 0
  acc_easy = c()
  acc_hard = c()
  easy = 0
  hard = 0
  dt_temp = dt[dt$workerid==i,]
  for (x in 1:nrow(dt_temp)){
    if (dt_temp[x, ]$task =='hard'){
      n_hard = n_hard +1
      if(dt_temp[x, ]$accuracy == 1){
        hard <- hard + 1
      }
    }else{
      n_easy = n_easy +1
      if(dt_temp[x, ]$accuracy == 1){
        easy <- easy + 1
      }
    }
    cnt_easy = append(cnt_easy, n_easy)
    cnt_hard = append(cnt_hard, n_hard)
    acc_easy = append(acc_easy, devide(easy, n_easy))
    acc_hard = append(acc_hard, devide(hard, n_hard))
  }
  dt$accuracy_hard[dt$workerid==i] = acc_hard
  dt$accuracy_easy[dt$workerid==i] = acc_easy
}


# deletion analysis 
p <- glmer(as.factor(decision) ~  (1 | workerid) + (1 | imageid) + (1 | workerid: task),   
             data = dt, family = binomial)
summary(p)
#report aii
p.t = sum(data.frame(VarCorr(p))$vcov)
p_randomeffects = as.data.frame(VarCorr(p))
p_randomeffects = p_randomeffects[, c(1, 4)]
colnames(p_randomeffects) = c('group', 'variance')
p_randomeffects[4, ] = c('aii', (p_randomeffects[1,2] + p_randomeffects[2, 2])/p.t)


p.vcov = VarCorr(p)
v.matrix = as.matrix(Matrix::bdiag(p.vcov))
rankMatrix(v.matrix )[1]  # rank of matrix

# another method for trace
p.t = sum(data.frame(VarCorr(p))$vcov)
p.var.image = data.frame(VarCorr(p))$vcov[1] / p.t

# find spammers
# total acc
group_acc = dt %>% group_by(workerid) %>% summarise(accuracy = sum(accuracy)/n_image, 
                                                .groups = 'drop') %>%
  as.data.frame()
hist(group_acc$accuracy)
range(group_acc$accuracy)
mean(group_acc$accuracy)
group_acc[group_acc$accuracy==min(group_acc$accuracy),]
low_acc = c()
for (i in unique(group_acc$workerid)){
  if (group_acc[group_acc$workerid==i,][2][1,]<=0.65){
    low_acc = append(low_acc, i)
  }
}

low_acc
# difference easy vs diff
group_task = dt %>% group_by(workerid, task) %>% summarise(accuracy = sum(accuracy)/(n_image/2), 
                                                       .groups = 'drop') %>%
  as.data.frame()

spammers = c()
for (i in unique(group_task$workerid)){
  if (((group_task[group_task$workerid==i,][3][1,] - group_task[group_task$workerid==i,][3][2,]) <= 0.1) & ((group_task[group_task$workerid==i,][3][1,]<=0.65) | (group_task[group_task$workerid==i,][3][2,]<=0.65))){
    spammers = append(spammers, i)
  }
}
spammers

# find examples

high_acc = c()
for (i in unique(group_acc$workerid)){
  if (group_acc[group_acc$workerid==i,][2][1,]>=0.85){
    high_acc = append(high_acc, i)
  }
}
high_acc


normal = c()
for (i in unique(group_task$workerid)){
  if (((group_task[group_task$workerid==i,][3][1,] - group_task[group_task$workerid==i,][3][2,]) >= 0.15) & ((group_task[group_task$workerid==i,][3][1,]>=0.75) & (group_task[group_task$workerid==i,][3][1,]+ group_task[group_task$workerid==i,][3][2,])/2 >=0.85)){
    normal = append(normal, i)
  }
}
normal

# threshold = 10 each l1
# fre1 -4
# uniform
# length_tail 1-3

# chi-squest frequency test
hist(rle(dt[dt$workerid==78,]$decision)$lengths)
table(rle(dt[dt$workerid==129,]$decision)$lengths)
group_task[group_task$workerid==12,]
group_acc[group_acc$workerid==12,]
group_acc[group_acc$workerid%in% c(12, 25, 85, 129, 151),]
group_task[group_task$workerid%in% c(12, 25, 85, 129, 151),]
group_acc[group_acc$workerid%in% c(9, 12, 14, 19, 25, 32, 46, 79, 83, 107, 129),]
group_acc[group_acc$accuracy<0.65,]
dfl[dfl$workerid%in% s,]


# perform Kolmogorov-Smirnov test
ks.test(dt[dt$workerid == 29, ]$accuracy, dt[dt$workerid == 9, ]$accuracy)

ks.test(dt[dt$workerid == 29, ]$accuracy_easy, dt[dt$workerid == 9, ]$accuracy_easy)
ks.test(dt[dt$workerid == 29, ]$accuracy_hard, dt[dt$workerid == 9, ]$accuracy_hard)
ks.test(dt[dt$workerid == 29, ]$accuracy_hard, dt[dt$workerid == 9, ]$accuracy_hard)
table(rle(dt[dt$workerid==12,]$decision)$lengths)

table(dt$workerid, dt$decision)
chisq.test(table(rle(dt[dt$workerid==14,]$decision)$lengths), table(rle(dt[dt$workerid==78,]$decision)$lengths))

m = t(as.data.frame(table(rle(dt[dt$workerid==23,]$decision)$lengths)))
colnames(m) = c(1, 2, 3, 4, 5, 6)
rownames(m)

data.frame()


