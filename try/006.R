# put it all together 
# load packages
library(lme4)
library(tidyverse)
library(simstudy)
library(influence.ME)
library(ggplot2)
library(Matrix)

##################################################################################
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


t = c()
for (i in 1:160){
  t[i] = as.numeric(table(rle(dt[dt$workerid==i,]$decision)$lengths))[1]
}
Fn <- ecdf(t)


library(fitdistrplus)
fit.w <- fitdist(t, "weibull")
summary(fit.w)
plot(fit.w)





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
###################################################################################


# variance
var_image <- 6
var_worker <- 3
# aii <- 0.5/(0.5+6)
#aii

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

dd$RE <- dd$image_eff + dd$worker_eff
dd$logre <- plogis(dd$RE)

dd[dd$workerid==1,]$RE = rep(0, 80)

dif = c()
for (i in 1:160){
  #print(range(dd[dd$workerid==i,]$logre))
  diff = range(dd[dd$workerid==i,]$logre)[2] - range(dd[dd$workerid==i,]$logre)[1]
  dif[i] = diff
}

min(dif)

dim(dtm1)
summary(p)
dtm1$e = residuals(p)
dtm1 

# var(worker): 6
# var(worker_eff): 5.123436
############ random guess########## p = 0.5
sample_random = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                     "re",  "p",  "decision", "correct" )
  for (x in 1:nworker){
    d1 = dtImage
    d1$worker_eff = runif(1, -0.5, 0.5)
    d1$int_eff = runif(1, -0.01, 0.01) - (d1$image_eff + d1$worker_eff)
    d1$re = d1$image_eff + d1$worker_eff +d1$int_eff
    
    var(d1$int_eff)
    
    d1$p = 1/(1+(exp(-d1$re)))
    
    for (i in 1:n_image){
      d1$decision[i] = rbinom(1, 1, prob = d1$p[i])
    }
    
    d1$correct = ifelse(d1$decision == d1$trueLable, 1, 0)
    df = rbind(df, d1)
  }
  return(df)
}


############# primary choice1 ###############
sample_primary1 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  for (x in 1:nworker){
    d2 = dtImage
    d2$int_eff = runif(1, -0.3, 0.3)
    d2$worker_eff = 2 + runif(1, -0.1, 0.1) - (d2$image_eff + d2$int_eff)
    d2$re = d2$image_eff + d2$worker_eff +d2$int_eff
    
    var(d2$int_eff)
    var(d2$worker_eff)
    
    d2$p = 1/(1+(exp(-d2$re)))
    
    for (i in 1:n_image){
      d2$decision[i] = rbinom(1, 1, prob = d2$p[i])
    }
    
    d2$correct = ifelse(d2$decision == d2$trueLable, 1, 0)
    df = rbind(df, d2)
  }
  return(df)
}

########## primary choice2 #################
############# primary choice 1###############
sample_primary2 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  for (x in 1:nworker){
    d3 = dtImage
    d3$int_eff = runif(1, -0.3, 0.3)
    d3$worker_eff = -2 + runif(1, -0.1, 0.1) - (d3$image_eff + d3$int_eff)
    d3$re = d3$image_eff + d3$worker_eff +d3$int_eff
    
    var(d3$int_eff)
    var(d3$worker_eff)
    
    d3$p = 1/(1+(exp(-d3$re)))
    
    for (i in 1:n_image){
      d3$decision[i] = rbinom(1, 1, prob = d3$p[i])
    }
    
    d3$correct = ifelse(d3$decision == d3$trueLable, 1, 0)
    df = rbind(df, d3)
  }
  return(df)
}

############ strong pattern ###############
sample_pattern1 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  
  for (x in 1:nworker){
    for (x in 1:100){
      d4 = dtImage
      d4$worker_eff = runif(1, -0.5, 0.5)
      
      for (i in 1:n_image){
        if (i%%2==0){
          d4$int_eff[i] = 4 + runif(1, -0.01, 0.01) - (d4$image_eff[i] + d4$worker_eff[i])
        }else{
          d4$int_eff[i] = -4 + runif(1, -0.01, 0.01) - (d4$image_eff[i] + d4$worker_eff[i])
        }
      }
      d4$re = d4$image_eff + d4$worker_eff +d4$int_eff
      
      var(d4$int_eff)
      
      d4$p = 1/(1+(exp(-d4$re)))
      
      for (j in 1:n_image){
        d4$decision[j] = rbinom(1, 1, prob = d4$p[j])
      }
      
      d4$correct = ifelse(d4$decision == d4$trueLable, 1, 0)
      len = length(unique(rle(d4$decision)[1]$lengths))
      u = unique(rle(d4$decision)[1]$lengths)[1]
      if ((len ==1)&(u==1)){
        df = rbind(df, d4)
        break
      }
    } 
  }
  return(df)
}


########## strong pattern2#############
sample_pattern2 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  
  for (x in 1:nworker){
    for (x in 1:100){
      d5 = dtImage
      d5$worker_eff = runif(1, -0.5, 0.5)
      
      for (i in 1:n_image){
        if (i%%2==0){
          d5$int_eff[i] = -4 + runif(1, -0.01, 0.01) - (d5$image_eff[i] + d5$worker_eff[i])
        }else{
          d5$int_eff[i] = 4 + runif(1, -0.01, 0.01) - (d5$image_eff[i] + d5$worker_eff[i])
        }
      }
      d5$re = d5$image_eff + d5$worker_eff +d5$int_eff
      
      var(d5$int_eff)
      
      d5$p = 1/(1+(exp(-d5$re)))
      
      for (j in 1:n_image){
        d5$decision[j] = rbinom(1, 1, prob = d5$p[j])
      }
      
      d5$correct = ifelse(d5$decision == d5$trueLable, 1, 0)
      len = length(unique(rle(d5$decision)[1]$lengths))
      u = unique(rle(d5$decision)[1]$lengths)[1]
      if ((len ==1)&(u==1)){
        df = rbind(df, d5)
        break
      }
    } 
  }
  return(df)
}


############ normal worker ################
sample_normal = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  
  for(t in 1:nworker){
    d6 = dtImage
    d6$worker_eff = runif(1, -0.5, 0.5)
    for (i in 1:n_image){
      d6$int_eff[i] = runif(1, -0.5, 0.5) 
    }
    
    d6$re = d6$image_eff + d6$worker_eff + d6$int_eff
    
    d6$p = 1/(1+(exp(-d6$re)))
    
    for (i in 1:n_image){
      d6$decision[i] = rbinom(1, 1, prob = d6$p[i])
    }
    
    d6$correct = ifelse(d6$decision == d6$trueLable, 1, 0)
    df = rbind(df, d6)
  }
  return(df)
}


###### combine together #######
spam1 = sample_random(2)
spam1$workerid = rep(1:2, each=n_image)

spam2 = sample_primary1(2)
spam2$workerid = rep(3:4, each=n_image)

spam3 = sample_primary2(2)
spam3$workerid = rep(5:6, each=n_image)

spam4 = sample_pattern1(2)
spam4$workerid = rep(7:8, each=n_image)

spam5 = sample_pattern2(2)
spam5$workerid = rep(9:10, each=n_image)

norm = sample_normal(75) # 75 normal
norm$workerid = rep(11:85, each=80)

data = rbind(spam1, spam2, spam3, spam4, spam5, norm)


################### model
p <- glmer(as.factor(decision) ~  (1 | workerid) + (1 | imageid),   
           data = data, family = binomial)
summary(p)


p.t = sum(data.frame(VarCorr(p))$vcov)
aii = data.frame(VarCorr(p))$vcov[1] / p.t
aii
# 0.0260462

################### 1
spam1 = sample_random(4)
spam1$workerid = rep(1:4, each=n_image)

spam2 = sample_primary1(4)
spam2$workerid = rep(5:8, each=n_image)

spam3 = sample_primary2(4)
spam3$workerid = rep(9:12, each=n_image)

spam4 = sample_pattern1(4)
spam4$workerid = rep(13:16, each=n_image)

spam5 = sample_pattern2(4)
spam5$workerid = rep(17:20, each=n_image)

norm = sample_normal(75) # 75 normal
norm$workerid = rep(21:95, each=80)

data1 = rbind(spam1, spam2, spam3, spam4, spam5, norm)


p1 <- glmer(as.factor(decision) ~  (1 | workerid) + (1 | imageid),   
           data = data1, family = binomial)
summary(p1)



spam1 = sample_random(1)
spam1$workerid = 1

spam2 = sample_primary1(1)
spam2$workerid = 2

spam3 = sample_primary2(1)
spam3$workerid = 3

spam4 = sample_pattern1(1)
spam4$workerid = 4

spam5 = sample_pattern2(1)
spam5$workerid = 5

norm = sample_normal(75) # 75 normal
norm$workerid = rep(6:80, each=80)

data2 = rbind(spam1, spam2, spam3, spam4, spam5, norm)


p2 <- glmer(as.factor(decision) ~  (1 | workerid) + (1 | imageid),   
            data = data2, family = binomial)
summary(p2)



p3 <- glmer(as.factor(decision) ~  (1 | workerid) + (1 | imageid),   
            data = norm, family = binomial)
summary(p3)


p1.t = sum(data.frame(VarCorr(p1))$vcov)
data.frame(VarCorr(p1))$vcov[1] + data.frame(VarCorr(p1))$vcov[2]/ p1.t
# 0.03475978
# 0.0265716


#################
d1 = dtImage
d1$worker_eff = runif(1, -0.5, 0.5)
d1$int_eff = runif(1, -0.01, 0.01) - (d1$image_eff + d1$worker_eff)
d1$re = d1$image_eff + d1$worker_eff +d1$int_eff

var(d1$int_eff)

d1$p = 1/(1+(exp(-d1$re)))

for (i in 1:n_image){
  d1$decision[i] = rbinom(1, 1, prob = d1$p[i])
}

d1$correct = ifelse(d1$decision == d1$trueLable, 1, 0)

sum(d1$correct)

table(d1$correct, d1$task)


d2 = dtImage
d2$int_eff = runif(1, -0.3, 0.3)
d2$worker_eff = 2 + runif(1, -0.1, 0.1) - (d2$image_eff + d2$int_eff)
d2$re = d2$image_eff + d2$worker_eff +d2$int_eff

var(d2$int_eff)
var(d2$worker_eff)

d2$p = 1/(1+(exp(-d2$re)))

for (i in 1:n_image){
  d2$decision[i] = rbinom(1, 1, prob = d2$p[i])
}

d2$correct = ifelse(d2$decision == d2$trueLable, 1, 0)

sum(d2$correct)

table(d2$correct, d2$task)




d3 = dtImage
d3$int_eff = runif(1, -0.3, 0.3)
d3$worker_eff = -2 + runif(1, -0.1, 0.1) - (d3$image_eff + d3$int_eff)
d3$re = d3$image_eff + d3$worker_eff +d3$int_eff

var(d3$int_eff)
var(d3$worker_eff)

d3$p = 1/(1+(exp(-d3$re)))

for (i in 1:n_image){
  d3$decision[i] = rbinom(1, 1, prob = d3$p[i])
}

d3$correct = ifelse(d3$decision == d3$trueLable, 1, 0)

sum(d3$correct)

table(d3$correct, d3$task)


for (x in 1:100){
  #while (tail(len, n=1)!=1) {
  d4 = dtImage
  d4$worker_eff = runif(1, -0.5, 0.5)
  
  for (i in 1:n_image){
    if (i%%2==0){
      d4$int_eff[i] = 4 + runif(1, -0.01, 0.01) - (d4$image_eff[i] + d4$worker_eff[i])
    }else{
      d4$int_eff[i] = -4 + runif(1, -0.01, 0.01) - (d4$image_eff[i] + d4$worker_eff[i])
    }
  }
  d4$re = d4$image_eff + d4$worker_eff +d4$int_eff
  
  var(d4$int_eff)
  
  d4$p = 1/(1+(exp(-d4$re)))
  
  for (j in 1:n_image){
    d4$decision[j] = rbinom(1, 1, prob = d4$p[j])
  }
  
  d4$correct = ifelse(d4$decision == d4$trueLable, 1, 0)
  len = length(unique(rle(d4$decision)[1]$lengths))
  u = unique(rle(d4$decision)[1]$lengths)[1]
  if ((len ==1)&(u==1)){
    
  }
  #}
} # loop has an issue 



d4 = dtImage
d4$worker_eff = runif(1, -0.5, 0.5)

for (i in 1:n_image){
  if (i%%2==0){
    d4$int_eff[i] = 4 + runif(1, -0.01, 0.01) - (d4$image_eff[i] + d4$worker_eff[i])
  }else{
    d4$int_eff[i] = -4 + runif(1, -0.01, 0.01) - (d4$image_eff[i] + d4$worker_eff[i])
  }
}
d4$re = d4$image_eff + d4$worker_eff +d4$int_eff

var(d4$int_eff)

d4$p = 1/(1+(exp(-d4$re)))

for (i in 1:n_image){
  d4$decision[i] = rbinom(1, 1, prob = d4$p[i])
}

d4$correct = ifelse(d4$decision == d4$trueLable, 1, 0)

unique(rle(d4$decision)[1]$lengths)

sum(d4$correct)

table(d4$correct, d4$task)



d5 = dtImage
d5$worker_eff = runif(1, -0.5, 0.5)

for (i in 1:n_image){
  if (i%%2==0){
    d5$int_eff[i] = -4 + runif(1, -0.01, 0.01) - (d5$image_eff[i] + d5$worker_eff[i])
  }else{
    d5$int_eff[i] = 4 + runif(1, -0.01, 0.01) - (d5$image_eff[i] + d5$worker_eff[i])
  }
}
d5$re = d5$image_eff + d5$worker_eff +d5$int_eff

var(d5$int_eff)

d5$p = 1/(1+(exp(-d5$re)))

for (i in 1:n_image){
  d5$decision[i] = rbinom(1, 1, prob = d5$p[i])
}

d5$correct = ifelse(d5$decision == d5$trueLable, 1, 0)

unique(rle(d5$decision)[1]$lengths)

sum(d5$correct)

table(d5$correct, d5$task)
#############

ggplot(d1, aes(image_eff)) +
  geom_line(aes(y=worker_eff), colour="black") 
#geom_line(aes(y=correct), colour="red")


plot(d1$image_eff, d1$int_eff)



# response variable
def.d <- defDataAdd(varname = "decision", formula = "RE", 
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

dt
# deletion analysis 
p <- glmer(as.factor(decision) ~  (1 | workerid) + (1 | imageid), # + (1 | workerid: task),   
           data = dtm1, family = binomial)
summary(p)

table(dt$workerid, dt$decision)
##### involve spammers
sample(1:160, 10, replace = FALSE) # randomly select workerid to replace
# [1] 126  89  33  84
# [1] 145  90  41  48  17  24 158 152  50  57
# random guess
############# 3.19 ##############################
re = c(145, 90, 41, 48, 17, 24, 158, 152, 50, 57)


# generate worker data
def.worker.re <- defData(varname = "worker_eff", dist = "normal", formula = 0, variance = 2, 
                         id = "workerid")
dtWorker.re <- genData(10, def.worker.re)

dtWorker.re$workerid = re

for(id in re){
  dd[dd$workerid==id, ]$worker_eff = dtWorker.re[dtWorker.re$workerid==id,]$worker_eff
}

def.d <- defDataAdd(varname = "decision", formula = "image_eff + worker_eff + int_eff", 
                    dist="binary", link = "logit")
dt <- addColumns(def.d, dd)

######### 158: loglik: 67.01520
dt[dt$workerid==158, ]$decision = rbinom(80, 1, prob = 0.3)


####################################################################
dt[dt$workerid ==126, ][dt[dt$workerid ==126, ]$trueLable=='same',]$decision <- 
  sample(c(0,1), dim(dt[dt$workerid ==126, ][dt[dt$workerid ==126, ]$trueLable=='same',])[1], prob=c(0.3, 1-0.3), rep=TRUE)
dt[dt$workerid ==126, ][dt[df$workerid ==126, ]$trueLable=='different',]$decision <- 
  sample(c(0,1), dim(dt[dt$workerid ==126, ][dt[dt$workerid ==126, ]$trueLable=='different',])[1], prob=c(0.3, 1-0.3), rep=TRUE)


dt[dt$workerid ==89, ][dt[dt$workerid ==89, ]$task=='easy',]$decision <- 
  sample(c(0,1), dim(dt[dt$workerid ==89, ][dt[dt$workerid ==89, ]$task=='easy',])[1], prob=c(0.5, 1-0.5), rep=TRUE)
dt[dt$workerid ==89, ][dt[dt$workerid ==89, ]$task=='hard',]$decision <- 
  sample(c(0,1), dim(dt[dt$workerid ==89, ][dt[dt$workerid ==89, ]$task=='hard',])[1], prob=c(0.5, 1-0.5), rep=TRUE)

# repeat pattern
dt[dt$workerid==33,]$decision <- rep(c(0,1), times = n_image/2)  

dt[dt$workerid==84,]$decision <- rep(c(1,0), times = n_image/2)  

table(dt[dt$workerid==126,]$decision, dt[dt$workerid==126,]$task)
################


trace <- function(A) {
  n <- dim(A)[1] # get dimension of matrix
  tr <- 0 # initialize trace value
  
  # Loop over the diagonal elements of the supplied matrix and add the element to tr
  for (k in 1:n) {
    l <- A[k,k]
    tr <- tr + l
  }
  return(tr[[1]])
}


# variance -covariance matrix 
p.vcov = VarCorr(p)
v.matrix = as.matrix(Matrix::bdiag(p.vcov))
rankMatrix(v.matrix )[1]  # rank of matrix


# another method for trace
p.t = sum(data.frame(VarCorr(p))$vcov)

p.var.image = data.frame(VarCorr(p))$vcov[1] / p.t


workerid <- c()
loglik <- c()
total.var <- c()
var.image <- c()
varint.image <- c()
vartrace <- c()
vartrace2 <- c()
vartrace3 <- c()
total.var.change <- c()
var.image.change <- c()
varint.image.change <- c()
accuary <- c()
p_value <- c()
chiindep <- c()
c <- c()


table(dt$workerid,dt$decision)
min(table(dt$workerid,dt$decision)[, 1])

for (x in unique(data$workerid)) {
  workerid <- append(workerid, x)
  p_temp <- exclude.influence(p, "workerid", x)
  loglik <- append(loglik, -2*(logLik(p)[1]- logLik(p_temp)[1]))
  
  var.image  <- append(var.image, data.frame(VarCorr(p_temp))$vcov[1])
  #varint.image <- append(varint.image , data.frame(VarCorr(p_temp))$vcov[1] + data.frame(VarCorr(p_temp))$vcov[5] + data.frame(VarCorr(p_temp))$vcov[4])
  total.var <- append(total.var, sum(data.frame(VarCorr(p_temp))$vcov))
  
  var.image.change <- append(var.image.change, data.frame(VarCorr(p_temp))$vcov[1]/ sum(data.frame(VarCorr(p_temp))$vcov) - p.var.image )
  #varint.image.change <- append(varint.image.change, (data.frame(VarCorr(p_temp))$vcov[1] + data.frame(VarCorr(p_temp))$vcov[3] + data.frame(VarCorr(p_temp))$vcov[4]) /sum(data.frame(VarCorr(p_temp))$vcov) - p.var.imageint)
  
  total.var.change <- append(total.var.change, sum(data.frame(VarCorr(p_temp))$vcov) / p.t)
  
  vartrace <- append(vartrace,  trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp)))))
  vartrace2 <- append(vartrace2, trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp))))- rankMatrix(v.matrix)[1])
  vartrace3 <- append(vartrace3, abs(trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp))))- rankMatrix(v.matrix)[1]))
  #disagreerate <- append(disagreerate, min(table(Prolific[Prolific$imageid==x, ]$decision))/sum(table(Prolific[Prolific$imageid==x, ]$decision)))
  #p_value <- append(p_value, pchisq(-2*(logLik(p)[1]- logLik(p_temp)[1]), df=80, lower.tail=F))
  #chi2 = chisq.test(table(dt[dt$workerid==x,]$task, dt[dt$workerid==x,]$decision), correct=F)
  #c <- append(c, as.numeric(sqrt(chi2$statistic / sum(table(dt[dt$workerid==x,]$task, dt[dt$workerid==x,]$decision)))))
  #chiindep <- append(chiindep, chi2$p.value)
}


length(workerid)
length(loglik)
length(total.var)
length(var.image)
length(varint.image)
length(vartrace)
length(vartrace2)
length(vartrace3)
length(total.var.change)
length(var.image.change)
length(varint.image.change)
length(accuary) 
length(p_value) 
length(c)
length(chiindep)


df.p = data.frame(workerid,loglik, total.var, var.image, vartrace, vartrace2, vartrace3,
                  total.var.change, var.image.change)

# concatenate
dfl = df.p[, c('workerid', 'loglik', 'vartrace', 'total.var.change')]
head(dfl)

head(dfl[order(dfl$total.var.change),], 10)


dt[dt$workerid==1,]
tail(dfl[order(dfl$loglik),], 20)
head(dfl[order(dfl$total.var.change),], 20)
dfl[dfl$workerid%in%re, ]

dfl

dfl$rankl = rank(dfl$loglik)
dfl$rankabtr = rank(dfl$vartrace)
dfl$ranktr = rank(abs(dfl$vartrace))
dfl$ranktrratio = rank(dfl$total.var.change)
#dfl$ranke = rank(dfl$res)
#dfl$rankabse = rank(dfl$reab)


dt$accuracy = ifelse(dt$trueLable==dt$decision, 1, 0)
group_acc = data %>% group_by(workerid) %>% summarise(accuracy = sum(correct)/n_image, 
                                                    .groups = 'drop') %>%as.data.frame()

group_task = data %>% group_by(workerid, task) %>% summarise(accuracy = sum(correct)/(n_image/2),
                                                    .groups = 'drop') %>%as.data.frame()

dfl = dfl %>% left_join(group_acc, by = "workerid")



View(ranef(p)$imageid)
View(ranef(p)$workerid)
View(ranef(p)$`workerid:imageid`)

tail(dfl, 10)

range(group_acc$accuracy)
hist(group_acc$accuracy)

table(dt$workerid, dt$decision)
head(dfl[order(abs(dfl$vartrace)), ])
tail(dfl[order(abs(dfl$vartrace)), ])

head(dfl[order(dfl$vartrace), ])
tail(dfl[order(dfl$vartrace), ], 10)


head(dfl[order(dfl$total.var.change), ])
tail(dfl[order(dfl$total.var.change), ])

dtImage$color = as.numeric(dtImage$color)
dtImage[(dtImage$task=='easy') & (dtImage$trueLable==1),]$color = 1
dtImage[(dtImage$task=='easy') & (dtImage$trueLable==0),]$color = 2
dtImage[(dtImage$task=='hard') & (dtImage$trueLable==1),]$color = 3
dtImage[(dtImage$task=='hard') & (dtImage$trueLable==0),]$color = 4

range(dtImage[(dtImage$task=='easy') & (dtImage$trueLable==1),]$image_eff)
range(dtImage[(dtImage$task=='easy') & (dtImage$trueLable==0),]$image_eff)
range(dtImage[(dtImage$task=='hard') & (dtImage$trueLable==1),]$image_eff)
range(dtImage[(dtImage$task=='hard') & (dtImage$trueLable==0),]$image_eff)


rects <- data.frame(xstart = c(-5.110628, -1.9301443, -0.08896502,  1.930532), 
                    xend = c(-1.974526, -0.1900873, 1.84805054, 6.693479), col = c('easy$same', 'hard&same', 'hard$same', 'easy$diff'))

dat = data.frame(x = sort(dtImage$image_eff), y = 1/(1+exp(-sort(dtImage$image_eff))))

ggplot() + 
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
  geom_line(data = dat, aes(x,y))


dt$int_eff


data.frame(VarCorr(p_temp))$vcov[1]/sum(data.frame(VarCorr(p_temp))$vcov)
data.frame(VarCorr(p))$vcov[1]/sum(data.frame(VarCorr(p))$vcov)

p_temp

for (i in c(84, 89, 33, 158, 7, 95)){
  p_temp <- exclude.influence(p, "workerid", i)
  print(data.frame(VarCorr(p_temp))$vcov[1]/sum(data.frame(VarCorr(p_temp))$vcov))
  data <- dtm1[!dtm1$workerid==x,]
  p <- glmer(as.factor(decision) ~ (1 | workerid) + (1 | imageid),   
             data = data, family = binomial)
} 





p_temp <- exclude.influence(p_temp, "workerid", )
loglik <- append(loglik, -2*(logLik(p)[1]- logLik(p_temp)[1]))

data.frame(VarCorr(p))$vcov

if(!require('snpar')) {
  install.packages('snpar')
  library('DescTools')
}

install.packages('DescTools')

#118, 84, 82, 77,33, 31, 9 
for ( i in 1:35){
  print(i)
  print(RunsTest(dt[dt$workerid==i,]$decision, exact=TRUE))
}

RunsTest(dt[dt$workerid==9,]$decision, exact=TRUE)
RunsTest(append(rep(0,  79), rep(c(1, 0), 10)))

dt[dt$workerid==89,]$decision


tail(dfl[order(dfl$rankl),])


hist(rle(dt[dt$workerid==9,]$decision)[1]$lengths)

#31, 84


rep(c(0, 1),  40)

append(rep(0,  79), 1)



t = c()
for (i in 1:160){
  t[i] = as.numeric(table(rle(df[df$workerid==i,]$decision)$lengths))[1]
}
#Fn <- ecdf(t)


library(fitdistrplus)
fit.w <- fitdist(t, "weibull")
summary(fit.w)
plot(fit.w)

dtm1 = dt[!dt$workerid==1,]


group_acc = dtm1 %>% group_by(workerid) %>% summarise(
                                                    res = sum(e), 
                                                    reab = sum(abs(e)),
                                                    .groups = 'drop') %>%as.data.frame()
dfl = dfl %>% left_join(group_acc, 'workerid')

dfl







###########
x <- c(1,2,1,1,3,4,4,1,2,4,1,4,3,4,4,4,3,1,3,2,3,3,3,4,2,2,3)
xChar<-as.character(x)
library(markovchain)
mcX<-markovchainFit(xChar)$estimate
mcX


for(i in 1:15){
  xChar<-as.character(data[data$workerid==i, ]$decision)
  mcX<-markovchainFit(xChar)$estimate
  print(i)
  print(mcX) 
}



dt
dd
dt 



library(markovchain)
# Create a transition matrix
P <- matrix(c(0.8, 0.2, 0.4, 0.6), nrow = 2, byrow = TRUE)
rownames(P) <- c("A", "B")
mc <- new("markovchain", states = c("A", "B"), transitionMatrix = P)

# Test for significance
library(boot)
set.seed(1234)
boot.mc <- function(data, indices) {
  mc <- markovchainFit(data[indices])
  return(mc@transitionMatrix)
}
boot.out <- boot(data = c("A", "B", "B", "A", "B", "A"), statistic = boot.mc, R = 1000)
boot.ci(boot.out, type = "bca")




dat<-data.frame(replicate(20,sample(c("A", "B", "C","D"), size = 100, replace=TRUE)))

Markovmatrix <- function(X,l=1){
  tt <- table(X[,-c((ncol(X)-l+1):ncol(X))] , c(X[,-c(1:l)]))
  tt <- tt / rowSums(tt)
  return(tt)
}


Markovmatrix(as.matrix(dat),1)
Markovmatrix(as.matrix(dat),1)

Markovmatrix(as.matrix(sample1),1)

myMatr1<-matrix(c(0,1,1,0),byrow=TRUE, nrow=2)
myMatr2<-matrix(c(0.9,.1,.1,.9),byrow=TRUE, nrow=2)
mc1<-as(myMatr1,"markovchain")
mc2<-as(myMatr2,"markovchain")
mc1
mc2
sample1<-rmarkovchain(n=100, object=mc1)
sample2<-rmarkovchain(n=200, object=mc2)
# should reject
verifyHomogeneity(inputList = list(sample1,sample2))
#should accept
sample2<-rmarkovchain(n=200, object=mc1)
verifyHomogeneity(inputList = list(sample1,sample2))




