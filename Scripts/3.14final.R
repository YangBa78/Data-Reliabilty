# put it all together 
# load packages
library(lme4)
library(tidyverse)
library(simstudy)
library(influence.ME)
library(ggplot2)
library(Matrix)
library(DescTools)

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
var_image <- 3
var_worker <- 9
# aii <- 0.5/(0.5+6)
# aii

n_worker <- 60
n_image <-50

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
#int_sd = sample(runif(160, 0.01, 0.1))
#int_eff = c()

# for each worker i int_eff: (worker_i: easy, worker_i: hard) should be mean: 0, sd: random(range(int_sd))
#for(x in int_sd){
 # int_eff <- append(int_eff, rnorm(n = 2, mean = 0, sd = x)) 
#}

for (i in int_eff){
  if (abs(i)<0.6 & abs(i+1)<0.6){
    print(i)
  }
}


for (i in 1:320){
  if (acc_task[i, 3]<0.65){
    print(i)
  }
}


interact = data.frame( 'workerid' = rep(1:n_worker, each = 2) ,
                       'task' = c('easy', 'hard'),
                       'int_eff' = int_eff)
# check int_eff: 
# sd(int_eff)
# mean(int_eff)
#dd <- dd %>% left_join(interact, by = c("workerid", "task"))

# response variable
def.d <- defDataAdd(varname = "decision", formula = "image_eff + worker_eff", 
                    dist="binary", link = "logit")
dt <- addColumns(def.d, dd)

dt$accuracy <- ifelse(dt$trueLable == dt$decision, 1, 0)

acc = dt %>% group_by(workerid) %>% summarise(accuracy = sum(accuracy)/(n_image), 
                                                    .groups = 'drop') %>%as.data.frame()


acc_task = dt %>% group_by(workerid, task) %>% summarise(accuracy = sum(accuracy)/(n_image/2), 
                                              .groups = 'drop') %>%as.data.frame()
# 148


dt$re = dt$image_eff + dt$worker_eff
dt$p = 1/(1+(exp(-dt$re)))

acc

which(int_eff== - min(abs(int_eff)))


ggplot(ddd, aes(image_eff)) +
  geom_line(aes(y=p_i), colour="black") +  # accuracy 
  geom_line(aes(y=p1), colour="red") +  # 0.7333
  geom_line(aes(y=p2), colour="green") + # 0.7
  geom_line(aes(y=p3), colour="blue") + # 0.6
  geom_line(aes(y=p4), colour="pink") + # 0.666
  geom_line(aes(y=p5), colour="brown") + # 0.9
  geom_line(aes(y=p6), colour="yellow") + #+ # 0.73
  #geom_vline(xintercept = -0.9413365, linetype="dashed") + 
  #geom_vline(xintercept = -0.2652010, linetype="dashed") + 
  #geom_vline(xintercept = 0.4848510, linetype="dashed") 
  geom_abline(slope = coef(l1)[["temp$image_eff"]], 
              intercept = coef(l1)[["(Intercept)"]], colour = 'black') + 
  geom_abline(slope = coef(l1)[["temp$image_eff"]], 
              intercept = coef(l1)[["(Intercept)"]], colour = 'black') + 
  geom_abline(slope = coef(l1)[["temp$image_eff"]], 
              intercept = coef(l1)[["(Intercept)"]], colour = 'black')
  



l1 = lm(temp$p_i~temp$image_eff)
lm(temp$p1~temp$image_eff)
lm(temp$p2~temp$image_eff)
lm(temp$p3~temp$image_eff)
lm(temp$p4~temp$image_eff)
lm(temp$p4~temp$image_eff)
lm(temp$p4~temp$image_eff)



ddd = ddd %>% left_join(dtImage, by = 'image_eff')
ddd

range(dt[(dt$task=='easy')&(dt$trueLable==1),]$image_eff)
range(dt[(dt$task=='easy')&(dt$trueLable==0),]$image_eff)
range(dt[(dt$task=='hard')&(dt$trueLable==1),]$image_eff)
range(dt[(dt$task=='hard')&(dt$trueLable==0),]$image_eff)


e1 = AUC(ddd$image_eff, ddd$p5, from = min(-3.2945440), to = max(-0.9413365)) - AUC(ddd$image_eff, ddd$p_i, from = min(-3.2945440), to = max(-0.9413365))
h1 = AUC(ddd$image_eff, ddd$p5, from = min(-0.6412339), to = max(-0.2652010)) - AUC(ddd$image_eff, ddd$p_i, from = min(-0.6412339), to = max(-0.2652010))
h2 = AUC(ddd$image_eff, ddd$p5, from = min(-0.2522977), to = max(0.4848510)) - AUC(ddd$image_eff, ddd$p_i, from = min(-0.2522977), to = max(0.4848510))
e2 = AUC(ddd$image_eff, ddd$p5, from = min(0.6883254), to = max(2.6703394)) - AUC(ddd$image_eff, ddd$p_i, from = min(0.6883254), to = max(2.6703394))

e1+ e2
h1+ h2


###########
id = data.frame(ranef(p)$workerid)
id = cbind(newColName = rownames(id), id)
rownames(id) <- 1:nrow(id)
colnames(id) = c('workerid', 'worker_eff')
id
id$workerid = as.integer(id$workerid)


pair = data.frame(ranef(p)$imageid)
pair = cbind(newColName = rownames(pair), pair)
rownames(pair) <- 1:nrow(pair)
colnames(pair) = c('imageid', 'image_eff')
pair$imageid = as.integer(pair$imageid)

#dt = df %>% left_join(id, by='ID')
#df = df %>% left_join(pair, by='Pair')
pair$worker_eff1 = rep(-1.7924816, 30)
pair$worker_eff2 = rep( 0.8709213, 30)
pair$worker_eff3 = rep(-2.6798581, 30)
pair$worker_eff4 = rep( 1.6748542, 30)
pair$worker_eff5 = rep(-0.2932835, 30)
pair$worker_eff6 = rep( 1.6748542, 30)



df$re = df$worker_eff + df$image_eff
df$p = 1/(1+(exp(-df$re)))


t1 = df[df$ID==355, ][, c('ID', 'Pair', 'image_eff', 'p')]
t2 = df[df$ID==373, ][, c('ID', 'Pair', 'image_eff', 'p')]
t3 = df[df$ID==234, ][, c('ID', 'Pair', 'image_eff', 'p')]
t4 = df[df$ID==248, ][, c('ID', 'Pair', 'image_eff', 'p')]
t5 = df[df$ID==243, ][, c('ID', 'Pair', 'image_eff', 'p')]
t6 = df[df$ID==309, ][, c('ID', 'Pair', 'image_eff', 'p')]
colnames(t2)[4] = 'p2'
colnames(t3)[4] = 'p3'
colnames(t4)[4] = 'p4'
colnames(t5)[4] = 'p5'
colnames(t6)[4] = 'p6'

t = t1 %>% left_join(t2, by='Pair')
t = t %>% left_join(t3, by='Pair')
t = t %>% left_join(t4, by='Pair')
t = t %>% left_join(t5, by='Pair')
t = t %>% left_join(t6, by='Pair')
t$p_i = 1/(1+(exp(-t$image_eff.x)))



ggplot(t, aes(image_eff.x)) +
  geom_line(aes(y=p_i), colour="black") + # accuracy 100% 
  geom_line(aes(y=p), colour="red") +  # 0.6625
  geom_line(aes(y=p2), colour="green") + # 
  geom_line(aes(y=p3), colour="blue") + # 0.566
  geom_line(aes(y=p4), colour="yellow") + # 0.633
  geom_line(aes(y=p5), colour="brown") + # 0.6667
  geom_line(aes(y=p6), colour="orange") # 0.6667
#geom_vline(xintercept = -0.9413365, linetype="dashed") + 
#geom_vline(xintercept = -0.2652010, linetype="dashed") + 
#geom_vline(xintercept = 0.4848510, linetype="dashed") 








ddd
AUC(ddd[ddd$task == 'hard',]$image_eff, ddd[ddd$task == 'hard',]$p1) - AUC(ddd[ddd$task == 'hard',]$image_eff, ddd[ddd$task == 'hard',]$p_i)
AUC(ddd[ddd$task == 'easy',]$image_eff, ddd[ddd$task == 'easy',]$p_i)


range(ddd$image)

# geom_hline(yintercept=20, linetype="dashed", 
          # color = "red", size=2)





#1         1 0.6666667.  25  5
#2         2 0.7000000.  10 20 
#3         3 0.5666667.  28 2
#4         4 0.6333333   6 24
#5         5 0.6666667. 17 13

# 


ggplot() + 
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, 
                              ymin = -Inf, ymax = Inf, fill = col),
            alpha = 0.4) +
  geom_line(data = dat, aes(x,y))



head(table(dt$workerid, dt$decision), 5)

ddd$p_i = 1/(1+(exp(-ddd$image_eff)))

head(acc_task,10)

ddd = data.frame(dt[dt$workerid==1,]$image_eff, dt[dt$workerid==1,]$p, dt[dt$workerid==2,]$p, dt[dt$workerid==3,]$p, dt[dt$workerid==4,]$p, dt[dt$workerid==5,]$p, dt[dt$workerid==6,]$p)


colnames(ddd) = c('image_eff', 'p1', 'p2', 'p3','p4','p5', 'p6')


ggplot(temp1, aes(dt.image_eff)) +                    # basic graphical object
  geom_line(aes(y=p), colour="red") +  # first layer
  geom_line(aes(y=p2), colour="green")  # second layer

#+  # first layer
 # geom_line(aes(y=p2), colour="green")  # second layer


dt[dt$workerid==148,]$


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
p <- glmer(as.factor(decision) ~  (1 | workerid) + (1 | imageid), #+ (1|workerid: imageid),   
           data = dt, family = binomial)
summary(p)

table(dt$imageid, dt$decision)
View(ranef(p)$imageid)


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

for (x in unique(dt$workerid)) {
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
  p_value <- append(p_value, pchisq(-2*(logLik(p)[1]- logLik(p_temp)[1]), df=80, lower.tail=F))
  chi2 = chisq.test(table(dt[dt$workerid==x,]$task, dt[dt$workerid==x,]$decision), 1rect=F)
  c <- append(c, as.numeric(sqrt(chi2$statistic / sum(table(dt[dt$workerid==x,]$task, dt[dt$workerid==x,]$decision)))))
  chiindep <- append(chiindep, chi2$p.value)
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
                  total.var.change, var.image.change, p_value,  c, chiindep)

# concatenate
dfl = df.p[, c('workerid', 'loglik', 'vartrace', 'total.var.change', 'p_value', 'c', 'chiindep')]
head(dfl)

head(dfl[order(dfl$loglik),]) #119
dfl[dfl$workerid%in%re, ]

dt$accuracy = ifelse(dt$trueLable==dt$decision, 1, 0)
group_acc = dt %>% group_by(workerid) %>% summarise(accuracy = sum(accuracy)/n_image, 
                                                    .groups = 'drop') %>%as.data.frame()

table(dt$workerid, dt$decision)
head(dfl[order(abs(dfl$vartrace)), ])
tail(dfl[order(abs(dfl$vartrace)), ])

head(dfl[order(dfl$vartrace), ])
tail(dfl[order(dfl$vartrace), ], 10)


head(dfl[order(dfl$total.var.change), ])
tail(dfl[order(dfl$total.var.change), ])




##########################################

p #report aii
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
                                                    .groups = 'drop') %>%as.data.frame()
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
# [1]  32 119 128 133
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
  if (group_acc[group_acc$workerid==i,][2][1,]>=0.80){
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

sample(1:160, 4, replace = FALSE)
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


temp = data.frame(dtImage$image_eff, 1/(1+(exp(-dtImage$image_eff))))
#temp = data.frame(temp1$dt.image_eff, 1/(1+(exp(-temp1$dt.image_eff))))


colnames(temp) = c('image_eff', 'pi')

temp$worker_eff = rep(0.1, 30)
temp$task = ifelse(abs(temp$image_eff)< median(abs(temp$image_eff)), 'hard', 'easy')
temp$true <- ifelse(temp$image_eff > median(temp$image_eff), 1, 0)
temp$int_eff = ifelse(temp$task=='easy', 1, -1)
temp

temp$worker_eff2 = rep(2, 30)
temp$worker_eff3 = rep(-2, 30)
temp$int_eff2 = ifelse(temp$task=='easy', 0.1, -0.1)
temp$int_eff3 = ifelse(temp$task=='easy', 0.5, -0.5)

temp$p1 = 1/(1+(exp(-(temp$image_eff + temp$worker_eff + temp$int_eff))))
temp$p1.2 = 1/(1+(exp(-(temp$image_eff + temp$worker_eff + temp$int_eff2))))
temp$p1.3 = 1/(1+(exp(-(temp$image_eff + temp$worker_eff + temp$int_eff3))))
temp$p2 = 1/(1+(exp(-(temp$image_eff + temp$worker_eff2 + temp$int_eff))))
temp$p2.2 = 1/(1+(exp(-(temp$image_eff + temp$worker_eff2 + temp$int_eff2))))
temp$p2.3 = 1/(1+(exp(-(temp$image_eff + temp$worker_eff2 + temp$int_eff3))))
temp$p3 = 1/(1+(exp(-(temp$image_eff + temp$worker_eff3 + temp$int_eff))))
temp$p3.2 = 1/(1+(exp(-(temp$image_eff + temp$worker_eff3 + temp$int_eff2))))
temp$p3.3 = 1/(1+(exp(-(temp$image_eff + temp$worker_eff3 + temp$int_eff3))))

temp$pw1 = 1/(1+(exp(-(temp$worker_eff ))))
temp$pw2 = 1/(1+(exp(-(temp$worker_eff2 ))))
temp$pw3 = 1/(1+(exp(-(temp$worker_eff3 ))))
temp$pi1 = 1/(1+(exp(-( temp$int_eff))))
temp$pi2 = 1/(1+(exp(-( temp$int_eff2))))
temp$pi3 = 1/(1+(exp(-( temp$int_eff3))))

temp$p_i = 1/(1+(exp(-(temp$image_eff))))

ggplot(temp, aes(image_eff)) +
  geom_line(aes(y=p_i), colour="black") + # accuracy 100% 
  geom_line(aes(y=p1), colour="red", size=0.5) +  # 0.6667
  geom_line(aes(y=p1.2), colour="red", size = 1) + # 0.7
  geom_line(aes(y=p1.3), colour="red",size=2) + # 0.566
  geom_line(aes(y=p2), colour="blue", size=0.5) + # 0.633
  geom_line(aes(y=p2.2), colour="blue", size=1) + # 0.6667
  geom_line(aes(y=p2.3), colour="blue", size=2) + 
  geom_line(aes(y=p3), colour="green", size=0.5) + # 0.633
  geom_line(aes(y=p3.2), colour="green", size=1) + # 0.6667
  geom_line(aes(y=p3.3), colour="green", size=2)
  #geom_vline(xintercept = -0.9413365, linetype="dashed") + 
  #geom_vline(xintercept = -0.2652010, linetype="dashed") + 
  #geom_vline(xintercept = 0.4848510, linetype="dashed")


0.1 
ggplot(temp, aes(image_eff)) +
  geom_line(aes(y=p_i), colour="black", size=1.5) + # original 
  geom_line(aes(y=p1), colour="red", size=1) +  # 0.1, 1, -1
  geom_line(aes(y=p1.2), colour="purple", size = 1) + # 0.1, 0.1, -0.1
  geom_line(aes(y=p1.3), colour="green",size=1)# + # 0.1, 0.5, -0.5
  #geom_line(aes(y=pw1), colour="blue", size=1) + # 0.1
  #geom_line(aes(y=pi1), colour="red", size=1) + # 0.
  #geom_line(aes(y=pi2), colour="purple", size=1) + 
  #geom_line(aes(y=pi3), colour="green", size=1)

print(sum(temp$acc1)/30) # 0.7333333
print(sum(temp$acc1.2)/30) # 0.6666667
print(sum(temp$acc1.3)/30) # 0.7666667

table(temp$task, temp$acc1)/15
table(temp$task, temp$acc1.2)/15 
table(temp$task, temp$acc1.3)/15


AUC(ddd[ddd$task == 'hard',]$image_eff, ddd[ddd$task == 'hard',]$p1) - AUC(ddd[ddd$task == 'hard',]$image_eff, ddd[ddd$task == 'hard',]$p_i)
AUC(ddd[ddd$task == 'easy',]$image_eff, ddd[ddd$task == 'easy',]$p_i)

AUC(temp$image_eff, temp$pi) 
abs(AUC(temp$image_eff, temp$p2) - AUC(temp$image_eff, temp$pi))
abs(AUC(temp$image_eff, temp$p2.2)- AUC(temp$image_eff, temp$pi))
abs(AUC(temp$image_eff, temp$p2.3)- AUC(temp$image_eff, temp$pi))


range(temp[(temp$task=='easy')&(temp$true==1),]$image_eff)
range(temp[(temp$task=='easy')&(temp$true==0),]$image_eff)
range(temp[(temp$task=='hard')&(temp$true==1),]$image_eff)
range(temp[(temp$task=='hard')&(temp$true==0),]$image_eff)

e1 = AUC(temp$image_eff, temp$p1, from = min(-5.752208), to = max(-1.979306)) - AUC(temp$image_eff, temp$p_i, from = min(-5.752208), to = max(-1.979306))
h1 = AUC(temp$image_eff, temp$p1, from = min(-1.428297), to = max(1.026011)) - AUC(temp$image_eff, temp$p_i, from = min(-1.428297), to = max(1.026011))
h2 = AUC(temp$image_eff, temp$p1, from = min(1.031257), to = max(1.912443)) - AUC(temp$image_eff, temp$p_i, from = min(1.031257), to = max(1.912443))
e2 = AUC(temp$image_eff, temp$p1, from = min(2.126327), to = max(7.965620)) - AUC(temp$image_eff, temp$p_i, from = min(2.126327), to = max(7.965620))

e1+ e2
h1+ h2


prop=c(0.3, 0.7, 0.3, 0.1, 0.9)
rbinom(n = 5, size = 1, prob = prop)

temp$d1 = NA
temp$d1.2 = NA
temp$d1.3 = NA
temp$d2 = NA
temp$d2.2 = NA
temp$d2.3 = NA
temp$d3 = NA
temp$d3.2 = NA
temp$d3.3 = NA

for(i in 1:30){
  temp$d1[i] = rbinom(n = 1, size = 1, prob = temp$p1[i])
  temp$d1.2[i] = rbinom(n = 1, size = 1, prob = temp$p1.2[i])
  temp$d1.3[i] = rbinom(n = 1, size = 1, prob = temp$p1.3[i])
  temp$d2[i] = rbinom(n = 1, size = 1, prob = temp$p2[i])
  temp$d2.2[i] = rbinom(n = 1, size = 1, prob = temp$p2.2[i])
  temp$d2.3[i] = rbinom(n = 1, size = 1, prob = temp$p2.3[i])
  temp$d3[i] = rbinom(n = 1, size = 1, prob = temp$p3[i])
  temp$d3.2[i] = rbinom(n = 1, size = 1, prob = temp$p3.2[i])
  temp$d3.3[i] = rbinom(n = 1, size = 1, prob = temp$p3.3[i])
}

temp$acc1 <- ifelse(temp$true == temp$d1, 1, 0)
temp$acc1.2 <- ifelse(temp$true == temp$d1.2, 1, 0)
temp$acc1.3 <- ifelse(temp$true == temp$d1.3, 1, 0)
temp$acc2 <- ifelse(temp$true == temp$d2, 1, 0)
temp$acc2.2 <- ifelse(temp$true == temp$d2.2, 1, 0)
temp$acc2.3 <- ifelse(temp$true == temp$d2.3, 1, 0)
temp$acc3 <- ifelse(temp$true == temp$d3, 1, 0)
temp$acc3.2 <- ifelse(temp$true == temp$d3.2, 1, 0)
temp$acc3.3 <- ifelse(temp$true == temp$d3.3, 1, 0)

#2 
ggplot(temp, aes(image_eff)) +
  geom_line(aes(y=p_i), colour="black", size=4) + # original 
  geom_line(aes(y=p2), colour="red", size=1) +  # 0.1, 1, -1
  geom_line(aes(y=p2.2), colour="purple", size = 1) + # 0.1, 0.1, -0.1
  geom_line(aes(y=p2.3), colour="green",size=1) + # 0.1, 0.5, -0.5
  geom_line(aes(y=pw2), colour="blue", size=1) + # 0.1
  geom_line(aes(y=pi1), colour="red", size=0.5) + # 0.
  geom_line(aes(y=pi2), colour="purple", size=0.5) + 
  geom_line(aes(y=pi3), colour="green", size=0.5)

print(sum(temp$acc2)/30) # 0.7
print(sum(temp$acc2.2)/30) # 0.833
print(sum(temp$acc2.3)/30) # 0.667

table(temp$task, temp$acc2)/15 # 0.666, 0.7333
table(temp$task, temp$acc2.2)/15 # 1, 0.667
table(temp$task, temp$acc2.3)/15 # 0.8, 0.533


#3
ggplot(temp, aes(image_eff)) +
  geom_line(aes(y=p_i), colour="black", size=3) + # original 
  geom_line(aes(y=p3), colour="red", size=1) +  # 0.1, 1, -1
  geom_line(aes(y=p3.2), colour="purple", size = 1) + # 0.1, 0.1, -0.1
  geom_line(aes(y=p3.3), colour="green",size=1) + # 0.1, 0.5, -0.5
  geom_line(aes(y=pw3), colour="blue", size=1) + # 0.1
  geom_line(aes(y=pi1), colour="red", size=0.5) + # 0.
  geom_line(aes(y=pi2), colour="purple", size=0.5) + 
  geom_line(aes(y=pi3), colour="green", size=0.5)

print(sum(temp$acc3)/30) # 0.7
print(sum(temp$acc3.2)/30) # 0.833
print(sum(temp$acc3.3)/30) # 0.667

table(temp$task, temp$acc3)/15 # 0.666, 0.7333
table(temp$task, temp$acc3.2)/15 # 1, 0.667
table(temp$task, temp$acc3.3)/15 # 0.8, 0.533


#4
temp$worker_eff4 = rep(1, 30)
#temp$int_eff2 = ifelse(temp$task=='easy', 0.1, -0.1)
#temp$int_eff3 = ifelse(temp$task=='easy', 0.5, -0.5)

temp$p4 = 1/(1+(exp(-(temp$image_eff + temp$worker_eff4 + temp$int_eff))))
temp$p4.2 = 1/(1+(exp(-(temp$image_eff + temp$worker_eff4 + temp$int_eff2))))
temp$p4.3 = 1/(1+(exp(-(temp$image_eff + temp$worker_eff4 + temp$int_eff3))))

temp$pw4 = 1/(1+(exp(-(temp$worker_eff4 ))))


temp$d4 = NA
temp$d4.2 = NA
temp$d4.3 = NA

for(i in 1:30){
  temp$d4[i] = rbinom(n = 1, size = 1, prob = temp$p4[i])
  temp$d4.2[i] = rbinom(n = 1, size = 1, prob = temp$p4.2[i])
  temp$d4.3[i] = rbinom(n = 1, size = 1, prob = temp$p4.3[i])
}

temp$acc4 <- ifelse(temp$true == temp$d4, 1, 0)
temp$acc4.2 <- ifelse(temp$true == temp$d4.2, 1, 0)
temp$acc4.3 <- ifelse(temp$true == temp$d4.3, 1, 0)

ggplot(temp, aes(image_eff)) +
  geom_line(aes(y=p_i), colour="black", size=3) + # original 
  geom_line(aes(y=p4), colour="red", size=1) +  # 0.1, 1, -1
  geom_line(aes(y=p4.2), colour="purple", size = 1) + # 0.1, 0.1, -0.1
  geom_line(aes(y=p4.3), colour="green",size=1) + # 0.1, 0.5, -0.5
  geom_line(aes(y=pw4), colour="blue", size=1) + # 0.1
  geom_line(aes(y=pi1), colour="red", size=0.5) + # 0.
  geom_line(aes(y=pi2), colour="purple", size=0.5) + 
  geom_line(aes(y=pi3), colour="green", size=0.5)

print(sum(temp$acc4)/30) # 0.7
print(sum(temp$acc4.2)/30) # 0.833
print(sum(temp$acc4.3)/30) # 0.667

table(temp$task, temp$acc4)/15 # 0.666, 0.7333
table(temp$task, temp$acc4.2)/15 # 1, 0.667
table(temp$task, temp$acc4.3)/15 # 0.8, 0.533




#4
temp$worker_eff5 = rep(0.5, 30)
#temp$int_eff2 = ifelse(temp$task=='easy', 0.1, -0.1)
#temp$int_eff3 = ifelse(temp$task=='easy', 0.5, -0.5)

temp$p5 = 1/(1+(exp(-(0))))


temp$d5 = NA

for(i in 1:30){
  temp$d5[i] = rbinom(n = 1, size = 1, prob = temp$p5[i])
}

temp$acc5 <- ifelse(temp$true == temp$d5, 1, 0)


ggplot(temp, aes(image_eff)) +
  geom_line(aes(y=p_i), colour="black", size=3) + # original 
  geom_line(aes(y=p5), colour="red", size=1)

print(sum(temp$acc5)/30) # 0.7

table(temp$task, temp$acc5)/15 # 0.666, 0.7333


summary(lm(p_i~image_eff , data = temp))
summary(lm(p4~image_eff , data = temp))
summary(lm(p4.2~image_eff , data = temp))
summary(lm(p4.3~image_eff , data = temp))

##############
t = rep(NA, 1000)
t0 = 0
for (i in seq_along(t)){
  temp$d0 = NA
  temp[temp$task=='easy',]$d0 <- 
    sample(c(0,1), dim(temp[temp$task=='easy',])[1], prob=c(0.5, 1-0.5), rep=TRUE)
  temp[temp$task=='hard',]$d0 <- 
    sample(c(0,1), dim(temp[temp$task=='hard',])[1], prob=c(0.5, 1-0.5), rep=TRUE)
  
  temp$acc0 <- ifelse(temp$true == temp$d0, 1, 0)
  #print(sum(temp$acc0)/30)
  
  # H0: acc_random - acc_shuffled = 0
  # H1: | acc_random - acc_shuffled | > 0
  
  a = rep(NA, 10000)
  for (i in seq_along(a)){
    temp$random = sample(temp$d0)
    temp$ran_acc <- ifelse(temp$true == temp$random, 1, 0)
    
    a[i] = sum(temp$acc0)/30 - sum(temp$ran_acc)/30 
    #table(temp$task, temp$ran_acc)/15 
  }
  #summary(a)
  #hist(a)
  if (mean(abs(a)>=0.18)<=0.05){
    t0 = t0+1
  }
}
t0 # 13/1000 = 0.013 

#mean(abs(a)>=0.18)

t_ = rep(NA, 1000)
t1 = 0
for (i in seq_along(t_)){
  temp$d00 = NA
  temp[temp$true==0,]$d00 <- 
    sample(c(0,1), dim(temp[temp$true==0,])[1], prob=c(0.5, 1-0.5), rep=TRUE)
  temp[temp$true==1,]$d00 <- 
    sample(c(0,1), dim(temp[temp$true==1,])[1], prob=c(0.5, 1-0.5), rep=TRUE)
  
  temp$acc00 <- ifelse(temp$true == temp$d00, 1, 0)
  #print(sum(temp$acc0)/30)
  
  # H0: acc_random - acc_shuffled = 0
  # H1: | acc_random - acc_shuffled | > 0
  
  aa = rep(NA, 10000)
  for (i in seq_along(aa)){
    temp$random1 = sample(temp$d00)
    temp$ran_acc1 <- ifelse(temp$true == temp$random1, 1, 0)
    
    aa[i] = sum(temp$acc00)/30 - sum(temp$ran_acc1)/30
    #table(temp$true, temp$ran_acc1)/15 
  }
  #summary(a)
  #hist(a)
  if (mean(abs(aa)>=0.18)<=0.05){
    t1 = t1+1
  }
}
t1 # 15/1000 = 0.015

#summary(aa)
#hist(aa)
#mean(abs(aa)>=0.3)
