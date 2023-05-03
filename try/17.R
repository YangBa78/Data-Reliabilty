n_image <- 80
n_worker <- 160 
n_measurements <- 5 

# assign names to each part, operator, trial
image <- str_glue("part_{1:image}") %>% as_factor()
worker<- str_glue("oper_{1:n_worker}") %>% as_factor()
measurement <- str_glue("measurment_{1:n_measurements}") %>% as_factor()

n_matrix <- n_image * n_worker * n_measurements # number of observations in the study

n_matrix
## [1] 60

grr_dummy_tbl <- crossing(image, worker, measurement)


# variance
var_image <- 6
var_worker <- 0.5
var_time <- 2
# aii <- 0.5/(0.5+6)
# aii

n_worker <- 50
n_image <-80
n_time <- 5

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
#dtTime <- dtTime[, c('imageid', 'image_eff', 'trueLable', 'task', 'workerid')]
dtTime$time = 5


dtTime1 <- addPeriods(dtTime,
                     nPeriods = n_time, idvars = "timeID",
                     timevarName = "t")

# generate worker data
def.worker <- defData(varname = "worker_eff", dist = "normal", formula = 0, variance = var_worker, 
                      id = "workerid")
dtWorker <- genData(n_worker, def.worker)



def.time <- defData(varname = "time_eff", dist = "normal", formula = 0, variance = var_time, 
                      id = "period")
dtT <- genData(n_time, def.time)

dtTime1 <- dtTime1[, c('imageid', 'image_eff', 'trueLable', 'task', 'workerid', 'period')]
# combine task and worker data
dd <- dtTime1%>% left_join(dtWorker, by = "workerid")

dtT$period = as.integer(dtT$period)

#dd$period = rep(c(0, 1, 2, 3, 4), 12800)
dtT$period = c(0, 1, 2, 3, 4)
dd <- dd%>% left_join(dtT, by = "period")
dd
#dtTime1 <- dtTime1[, c('imageid', 'image_eff', 'trueLable', 'task', 'workerid', 'period')]


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
dd



#interactions
var_w_t = 2
var_w_i = 2
var_t_i = 2

wt_eff = rnorm(n_worker*n_time,0, var_w_t)
wi_eff = rnorm(n_worker*n_image,0, var_w_i)
ti_eff = rnorm(n_image*n_time,0, var_t_i)


a = c()
for(i in seq(1, length(ti_eff), 5)){
  a = append(a, rep(ti_eff[i:(i+4)], 50))
}

a

dd$wt_eff = rep(wt_eff, 80)
dd$wi_eff = rep(wi_eff, each=5)
dd$ti_eff = a

dd





# response variable
def.d <- defDataAdd(varname = "decision", formula = "image_eff + worker_eff + time_eff + wt_eff + wi_eff + ti_eff", 
                    dist="binary", link = "logit")
dt <- addColumns(def.d, dd)



p <- glmer(as.factor(decision) ~  (1 | workerid) + (1 | imageid) + (1 | period) + (1 | workerid:imageid) + 
             (1 | workerid:period) + (1 | imageid:period), data = dt, family = binomial)
summary(p)


View(ranef(p)$`workerid:period`)
table(dt$period, dt$decision)
table(dt[dt$workerid==3,]$period, dt[dt$workerid==3,]$decision)


dtImage$decision = NA
dtImage$decision = as.integer(dtImage$decision)
###########
for (i in 1:80){
  dtImage[i]$decision = rbinom(1, 1, 0.5)
}
dtImage
table(dtImage$decision, dtImage$trueLable)

(18+21)/(19+22)



################################
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
p <- glmer(as.factor(decision) ~  (1 | workerid) + (1 | imageid) + (1 | workerid: task) + (1|period),   
           data = dt, family = binomial)
summary(p)


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

p.var.image = (data.frame(VarCorr(p))$vcov[1]  + data.frame(VarCorr(p))$vcov[2])/ p.t


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
  chi2 = chisq.test(table(dt[dt$workerid==x,]$task, dt[dt$workerid==x,]$decision), correct=F)
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


tail(dfl[order(dfl$loglik),])
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

data.frame()








