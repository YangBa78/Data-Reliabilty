library(simstudy)
library(lme4)
library(gmodels)
library(tidyverse)
#library(dplyr)
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


# length
max(rle(c(1,1,1, 1, 1, 1, 1, 0, 1,1, 1, 1, 1))[1]$lengths)

# variance
var_image <- 30
var_worker <- 1
aii <- 1/(0.5+6)
aii


# good: 
var_image <- 6
var_worker <- 0.5
aii <- 0.5/(0.5+6)
aii


#set.seed(123)
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
# different: 1, same: 0
dtImage$trueLable <- ifelse(dtImage$image_eff > median(dtImage$image_eff), 'different', 'same')


#dtImage$task<- sample_task


# bad data max acc: 0.66
# dtImage$trueLable[dtImage$task=='easy'] <- sample_same
# dtImage$trueLable[dtImage$task=='hard'] <- sample_diff



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


#int_sd = 2
#int_eff <- rnorm(n = 2, mean = 0, sd = 0.5)
#int_eff2 <- rnorm(n = 2, mean = 0, sd = 1)

#a = rnorm(n = 2, mean = 0, sd = 0.5)
#b = rnorm(n = 2, mean = 0, sd = 1)
#c = rnorm(n = 2, mean = 0, sd = 2)
#sd(a)
#sd(b)
#sd(c)
#sd(c(a, b, c))

int_sd = sample(runif(160, 0.5, 2))
int_eff = c()

for(x in int_sd){
  int_eff <- append(int_eff, rnorm(n = 2, mean = 0, sd = x)) 
}

intaction = data.frame( 'workerid' = rep(1:n_worker, each = 2) ,
                          'task' = c('easy', 'hard'),
                        'int_eff' = int_eff  
)
#sd(int_eff)
#mean(int_eff)
dd <- dd %>% left_join(intaction, by = c("workerid", "task"))


# response variable
def.d <- defDataAdd(varname = "decision", formula = "image_eff + worker_eff + int_eff", 
                    dist="binary", link = "logit")
#def.d <- defDataAdd(varname = "decision", formula = "-0.5 + image_eff + worker_eff", 
#                    dist="binary", link = "logit")

dt <- addColumns(def.d, dd)
dt
sum(dt$decision)
dim(dt)

#dt$logodds = dt$image_eff + dt$worker_eff
#dt$probability = logistic(dt$logodds)
#dt$decision = ifelse(dt$probability> 0.5, 1, 0)

# lazy spammer no.1, 3, 20, 40/0
dt[dt$workerid==20, ]
dt[dt$workerid==1, ]
dt[dt$workerid==3, ]

# 2, 30, 113 
# 43, 101, 113


dt$logodds <- dt$image_eff + dt$worker_eff + dt$int_eff


err_sd = 3
dt <- dt %>%
  mutate(time = 10 + log_odds + rnorm(
    n = nrow(dt),
    mean = 0,
    sd = err_sd
  ))


int_sd
norm_time_sd = 2
sp_time_sd = 2
for (x in unique(dt$workerid)){
  if (int_sd[x] > mean(int_sd) + 0.05) {
    dt$time[dt$imageid==x] <- 8 + rnorm(
      n = n_image,
      mean = 0,
      sd = sp_time_sd)} 
  else {
    dt$time[dt$imageid==x] <- 10 + rnorm(
      n = n_image,
      mean = 0,
      sd = norm_time_sd)}
}



a = lmer(time ~ (1 | workerid) + (1 | imageid) + (1 | workerid: task), data = dt)
summary(a)

fit <- glmer(as.factor(decision) ~  (1 | workerid) + (1 | task/imageid) + (1 | workerid: task),   
              data = dt, family = binomial)
summary(fit)
#fit1 <- glmer(as.factor(acc) ~ (1 | workerid) + (1 | imageid),   
#             data = dt, family = binomial)


summary(fit1)

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


group[group$accuary==min(group$accuary),]


group1 = dt %>% group_by(workerid, task) %>% summarise(accuary = sum(acc)/40, 
                                             .groups = 'drop') %>%
  as.data.frame()

group1

for (i in unique(group1$workerid)){
  if (((group1[group1$workerid==i,][3][1,] - group1[group1$workerid==i,][3][2,]) < 0.1) & ((group1[group1$workerid==i,][3][1,]<0.65) | (group1[group1$workerid==i,][3][2,]<0.65))){
    print(i)
  }
}

s = c(3, 20, 42, 64, 98, 137, 156)
s = c(87, 98, 127)
s = c(23, 30, 35, 37, 41, 133, 140)#, 122, 141)
group1[group1$workerid %in% s,]
group[group$accuary<0.65,]

mean(dt[dt$workerid==3,]$time)

mean(dt$time)

# other standards 

# [1] 23
# [1] 30
# [1] 35
# [1] 37
# [1] 41
# [1] 133
# [1] 140

group1[group1$workerid == 41,]

group1[group1$workerid==1,][3][1,]

#dt[dt$trueLable=='same' & dt$decision ==0, ]
dt[dt$workerid==128, ]


summary(fit1)


min(acc)
acc = c()
for( i in unique(dt$workerid)){
  acc <- append(acc, sum(dt[dt$workerid==i,]$acc))
}

hist(acc)


# 84, 90, 108, 146, 147, 
# analysis 
table(dt$imageid, dt$decision)
table(dt$workerid, dt$decision)
table(dt$worker_eff, dt$decision)
dtWorker[order(dtWorker$worker_eff, decreasing = TRUE), ]

#17,33, 63

# 15, 
# normal 27

dt
############### introduce spammers ##########################



# random guess
newT <- dtImage
p.0 <- 0.3
newT$decision <- NA
#newT$correct <- NA
newT$decision[newT$trueLable == 0] <- 
  sample(c(0,1), sum(newT$trueLable == 0), prob=c(0.3, 1-0.3), rep=TRUE)
newT$decision[newT$trueLable == 1] <- 
  sample(c(0,1), sum(newT$trueLable == 1), prob=c(0.3, 1-0.3), rep=TRUE)

newT
xtabs(~decision + trueLable, data = newT) %>%
  prop.table() %>%
  round(4) %>%
  addmargins()



#test if P(decision = 1, different) = P(decision=1)*P(different)
0.225*0.5 == 0.125
newT$workerid <- 161
newT$worker_eff <- NA
newT <- newT[, c("imageid", "image_eff", "nWorker", "trueLable", "task", "workerid",  "worker_eff",
                 "decision" )]

newdt <- rbind(dt, newT)

table(newdt[newdt$workerid==63,]$trueLable, newdt[newdt$workerid==63,]$decision)
newdt[newdt$workerid==161,]

#  dt[dt$workerid==160,]$decision = newT$decision


# random guess2
newT1 <- dtImage
p.0 <- 0.3
newT1$decision <- NA
#newT$correct <- NA
newT1$decision[newT1$task == "easy"] <- 
  sample(c(0,1), sum(newT1$task == "easy"), prob=c(0.5, 1-0.5), rep=TRUE)
newT1$decision[newT1$task == "hard"] <- 
  sample(c(0,1), sum(newT1$task == "hard"), prob=c(0.5, 1-0.5), rep=TRUE)

newT1
xtabs(~decision + trueLable, data = newT1) %>%
  prop.table() %>%
  round(4) %>%
  addmargins()

#test if P(decision = 1, different) = P(decision=1)*P(different)
0.225*0.5 == 0.125
newT1$workerid <- 163
newT1$worker_eff <- NA
newT1 <- newT1[, c("imageid", "image_eff", "nWorker", "trueLable", "task", "workerid",  "worker_eff",
                 "decision" )]

newdt1 <- rbind(dt, newT1)


fit1<- glmer(as.factor(decision) ~ (1 | workerid) + (1 | imageid),   
             data = newdt1, family = binomial)
summary(fit1)

newT1$decision

# repeat pattern
newT.1 <- dtImage
newT.1$workerid <- 162
newT.1$worker_eff <- NA
newT.1$decision <- rep(c(0,1), times = n_image/2)  
  
newT.1

newT.1 <- newT.1[, c("imageid", "image_eff", "nWorker", "trueLable", "task", "workerid",  "worker_eff",
                 "decision" )]

newdt.2 <- rbind(dt, newT.1)


fit2<- glmer(as.factor(decision) ~ (1 | workerid) + (1 | imageid),   
             data = newdt.2, family = binomial)
summary(fit2)

#dt[dt$workerid==158,]$decision = newT.1$decision


# repeat pattern
newT.2 <- dtImage
newT.2$workerid <- 164
newT.2$worker_eff <- NA
newT.2$decision <- rep(c(1,0), times = n_image/2)  

newT.2

newT.2 <- newT.2[, c("imageid", "image_eff", "nWorker", "trueLable", "task", "workerid",  "worker_eff",
                     "decision" )]

newdt.3 <- rbind(dt, newT.2)


fit2<- glmer(as.factor(decision) ~ (1 | workerid) + (1 | imageid),   
             data = newdt.2, family = binomial)
summary(fit2)

dt[dt$workerid==157,]$decision = newT.2$decision

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
newdtC1 <-rbind(newdtC, newT1)
newdtC2 <-rbind(newdtC1, newT.2)



fit0<- glmer(as.factor(decision) ~ (1 | workerid) + (1 | imageid),   
             data = dt2, family = binomial)
summary(fit0)


table(newdtC1[newdtC1$workerid==53,]$trueLable, newdtC1[newdtC1$workerid==53,]$decision)
table(newdtC[newdtC$workerid==52,]$trueLable, newdtC[newdtC$workerid==52,]$decision)
table(newdtC[newdtC$workerid==147,]$trueLable, newdtC[newdtC$workerid==147,]$decision)
table(newdtC[newdtC$workerid==84,]$trueLable, newdtC[newdtC$workerid==84,]$decision)
table(newdtC[newdtC$workerid==112,]$trueLable, newdtC[newdtC$workerid==112,]$decision)
table(newdtC[newdtC$workerid==165,]$trueLable, newdtC[newdtC$workerid==165,]$decision)
table(newdtC[newdtC$workerid==143,]$trueLable, newdtC[newdtC$workerid==143,]$decision)
table(newdtC[newdtC$workerid==108,]$trueLable, newdtC[newdtC$workerid==108,]$decision)
table(newdtC[newdtC$workerid==90,]$trueLable, newdtC[newdtC$workerid==90,]$decision)
#spammer: 1, 3, 20, 31, 32
# 13



# generate errors
newdtC1$difficulty <- ifelse(newdtC1$task=='easy', 0, 1)
newdtC1$converted_true <- ifelse(newdtC1$trueLable=='same', 0, 1)
newdtC1$error <- ifelse(newdtC1$converted_true == newdtC1$decision, 0, 1)
sum(newdtC1[newdtC1$workerid==51,][newdtC1[newdtC1$workerid==51,]$difficulty==0,]$error)
sum(newdtC1[newdtC1$workerid==51,][newdtC1[newdtC1$workerid==51,]$difficulty==1,]$error)

# plot
plot(c(0, 1), c(sum(newdtC1[newdtC1$workerid==53,][newdtC1[newdtC1$workerid==53,]$difficulty==0,]$error), 
                sum(newdtC1[newdtC1$workerid==53,][newdtC1[newdtC1$workerid==53,]$difficulty==1,]$error)), 
     xlim=c(0, 1),
     ylim = c(0,30),
     ylab="# error", xlab="task difficulty(easy - hard)",
     type = 'l')


plot(c(0, 1), c(sum(newdtC1[newdtC1$workerid==51,][newdtC1[newdtC1$workerid==51,]$difficulty==0,]$error), 
                sum(newdtC1[newdtC1$workerid==51,][newdtC1[newdtC1$workerid==51,]$difficulty==1,]$error)), 
     xlim=c(0, 1),
     ylim = c(0,30),
     ylab="# error", xlab="task difficulty(easy - hard)",
     type = 'l')



table(newdtC1$workerid, newdtC1$error)

post<- cbind( as.data.frame.matrix(table(newdtC1$workerid, newdtC1$decision)),
      as.data.frame.matrix(table(newdtC1$workerid, newdtC1$error)))[, c(1,2, 4)]

colnames(post)<- c(0, 1, 'error')
post
post[order(error),]

sp = c(21, 16, 18, 17, 17, 19, 20, 19, 19, 21, 18)
idx = c(1, 11, 13, 16, 18, 23, 28, 30, 31, 33, 45)
dim(post[-idx, ])

plot(c(mean(sp), mean(post$error), mean(post[-idx, ]$error)), type = 'l')



#############Jan 31############ 
# spammer dist, hypothesis testing 
# append(disagreerate, min(table(simulated_df[simulated_df$workerid==x, ]$decision))/sum(table(simulated_df[simulated_df$workerid==x, ]$decision)))

#dt$converted_decision <- ifelse(dt$decision==1, 'different', 'same')
newdtC$converted_decision <- ifelse(newdtC$decision==1, 'different', 'same')


table(dt[dt$workerid == 1, ]$decision)/sum(table(dt[dt$workerid == 1, ]$decision))



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
newdtC[newdtC$decision==1,] #package document 


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
plot(1/(1 + exp(-dt[dt$workerid == 30, ]$image_eff)), dt[dt$workerid == 30, ]$error3)
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


