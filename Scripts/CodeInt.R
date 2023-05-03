# load packages
library(lme4)
library(tidyverse)
library(simstudy)
library(influence.ME)
library(ggplot2)
library(Matrix)


# variance
var_image <- 20

#n_worker <- 160
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
dtImage$trueLable <- ifelse(dtImage$image_eff > 0, 1, 0)
dtImage

# var(worker): 6
# var(worker_eff): 5.123436
# spammers
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
    for (x in 1:200){
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
spam1 = sample_random(20)
spam1$workerid = rep(1:20, each=n_image)

spam2 = sample_primary1(20)
spam2$workerid = rep(21:40, each=n_image)

spam3 = sample_primary2(20)
spam3$workerid = rep(41:60, each=n_image)

spam4 = sample_pattern1(20)
spam4$workerid = rep(61:80, each=n_image)

spam5 = sample_pattern2(20)
spam5$workerid = rep(81:100, each=n_image)

norm = sample_normal(100) # 75 normal
norm$workerid = rep(11:85, each=n_image)

norm = sample_normal(100) # 75 normal
norm$workerid = rep(1:100, each=n_image)

data = rbind(spam1, spam2, spam3, spam4, spam5, norm)
data = rbind(spam1, spam2, spam3, spam4, spam5)

data = norm


view(dfl)
################### model
p <- glmer(as.factor(decision) ~  (1 | workerid) + (1 | imageid),   
           data = data, family = binomial)
summary(p)


p.t = sum(data.frame(VarCorr(p))$vcov)
aii = data.frame(VarCorr(p))$vcov[1] / p.t
aii

group_acc = data %>% group_by(workerid) %>% summarise(accuracy = sum(correct)/n_image, 
                                                      .groups = 'drop') %>%as.data.frame()

group_task = data %>% group_by(workerid, task) %>% summarise(accuracy = sum(correct)/(n_image/2),
                                                             .groups = 'drop') %>%as.data.frame()


################# deletion analysis 
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


dfl = df.p[, c('workerid', 'loglik', 'vartrace', 'total.var.change')]
hist(dfl$loglik)

s1 = dfl$loglik
s1.1 = dfl$loglik[21:60]

hist(dfl$loglik[61:100])
hist(dfl$loglik[1:20])
s2 = dfl$loglik
head(dfl, 20)

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

markovchainFit(data[data$workerid==31,]$decision)

########### HMM 
library(markovchain)
dtImage$task
task<-markovchainFit(dtImage$task)$estimate
task

true<-markovchainFit(dtImage$trueLable)$estimate
true

m <- 2
n_dep <- 1
q_emiss <- 2

start_TM = matrix(c(0.5, 0.5,
                    0.5, 0.5), byrow = TRUE, 
                  nrow = m, ncol = q_emiss)


start_TM = matrix(c(0.5681818, 0.4318182,
                    0.5428571, 0.4571429), byrow = TRUE, 
                  nrow = m, ncol = q_emiss)

prop.table(table(data$trueLable, data$decision),margin = 1)

start_EM = matrix(c(0.7926797, 0.2073203, 0.2033613, 0.7966387),
                  byrow = TRUE, 
                  nrow = m, ncol = q_emiss)


start_EM = matrix(c(0.5, 0.5, 0.5, 0.5),
                  byrow = TRUE, 
                  nrow = m, ncol = q_emiss)


hmd = data[,c('workerid', 'decision')]

colnames(hmd)[1] = 'id'
hmd$id = as.numeric(hmd$id)
hmd$decision = as.numeric(hmd$decision)
hmd$decision = ifelse(hmd$decision==1, 2, 1)


a <- mHMM(s_data = hmd, 
          gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss), 
          start_val = c(list(start_TM), list(start_EM)),
          mcmc = list(J = 1000, burn_in = 200))

a
summary(a)

gamma_pop <- obtain_gamma(a)
gamma_pop

gamma_subj <- obtain_gamma(a, level = "subject")
gamma_subj

emmiss_pop = obtain_emiss(a)
emmiss_pop
emmiss_subj = obtain_emiss(a, level = "subject")
emmiss_subj

length(emmiss_subj[[1]])
emmiss_subj[[1]][2]

a = c()
b = c()
c = c()
d = c()
for(i in 1:85){
  a[i]=sum(diag(matrix(unlist(emmiss_subj[[1]][i]), nrow = 2, ncol = 2)))/2
  b[i]=sum(1-diag(matrix(unlist(emmiss_subj[[1]][i]), nrow = 2, ncol = 2)))/2
  c[i]=sum(diag(matrix(unlist(emmiss_subj[[1]][i]), nrow = 2, ncol = 2) - matrix(unlist(emmiss_pop), nrow = 2, ncol = 2)))
  d[i]=sum(diag(-1*(matrix(unlist(emmiss_subj[[1]][i]), nrow = 2, ncol = 2) - matrix(unlist(emmiss_pop), nrow = 2, ncol = 2))))
}
group_acc$a = a
group_acc$b = b
group_acc$c = c
group_acc$d = d
cor(group_acc$accuracy, group_acc$a)
cor(group_acc$accuracy, group_acc$b)
#cor(group_acc$b, group_acc$b)
cor(group_acc$accuracy, group_acc$c)
cor(group_acc$accuracy, group_acc$d)


group_acc
diag(matrix(unlist(emmiss_subj[[1]][1]), nrow = 2, ncol = 2) - matrix(unlist(emmiss_pop), nrow = 2, ncol = 2))
-1*(matrix(unlist(emmiss_subj[[1]][1]), nrow = 2, ncol = 2) - matrix(unlist(emmiss_pop), nrow = 2, ncol = 2))

chisq.test(matrix(unlist(emmiss_subj[[1]][1]), nrow = 2, ncol = 2)) 
emmiss_subj[[1]][1]
asdf


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
