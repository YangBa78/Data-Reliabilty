library(lme4)
library(tidyverse)
library(simstudy)
library(influence.ME)
library(ggplot2)
library(Matrix)
library(HMM)
library(philentropy)

################ data simulation ###########
# define workers 
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
    for (x in 1:500){
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
    for (x in 1:500){
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


############# simulation 
# task 

# variance
var_image <- 20

#n_worker <- 160
n_image <-500

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


############# 
# worker

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

norm = sample_normal(50) 
norm$workerid = rep(11:60, each=n_image)

data = rbind(spam1, spam2, spam3, spam4, spam5, norm)

################ HMM
hmm = initHMM(c("A","B"),c('different','same'),
              transProbs=matrix(c(0.5,0.5,0.5,0.5),2),
              emissionProbs=matrix(c(0.01,0.99,0.99,0.01),2))
print(hmm)

# process data

s1 = spam1[spam1$workerid==2,]$decision
s1 = ifelse(s1==1, 'different', 'same')

# Baum-Welch
bw = baumWelch(hmm,s1,10)
print(bw$hmm)
bw$difference

####################. 
set.seed(1)
random_guess <- rep(NA, 10^4)
normal <- rep(NA, 10^4)

hmm = initHMM(c("A","B"),c('different','same'),
              transProbs=matrix(c(0.5,0.5,0.5,0.5),2),
              emissionProbs=matrix(c(0.999,0.001,0.001,0.999),2))

for(i in seq_along(normal){
  # variance
  var_image <- 10
  n_worker <- 160
  n_image <-2000
  # define image task
  def.image <- defData(varname = "image_eff", dist = "normal", formula = 0, variance = var_image,
                       id = "imageid")
  def.image <- defData(def.image, varname = "nWorker", dist = "nonrandom", formula = n_worker)
  
  # generate task data
  dtImage <- genData(n_image, def.image)
  
  dtImage$task <- ifelse(abs(dtImage$image_eff)< median(abs(dtImage$image_eff)), 'hard', 'easy')
  # different: 1, same: 0
  dtImage$trueLable <- ifelse(dtImage$image_eff > median(dtImage$image_eff), 1, 0)
  dtImage$trueLable <- ifelse(dtImage$image_eff > 0, 1, 0)
  
  # data
  spam = sample_random(1000)
  spam$workerid = rep(1:1000, each=n_image)
  
  norm = sample_normal(1000) 
  norm$workerid = rep(1001:2000, each=n_image)
  
  #data = rbind(spam1, norm)
  
  kl.d = c()
  kl.p1 = c()
  kl.p2 = c()
  kl.s = c()
  for(j in unique(spam$workerid)){
    d = spam[spam$workerid==j,]$decision
    d = ifelse(d==1, 'different', 'same') # all data
    
    #breaks <- quantile(d, probs = seq(0, 1, 1/2))
    #my_parts <- cut(d, breaks = breaks, include.lowest = TRUE, labels = FALSE)
    p1 = d[1:1000] # 1st partition
    p2 = d[1001:2000] # 2nd partition
    s = sample(d) # shuffled
    
    bw = baumWelch(hmm,d,10)
    bw1 = baumWelch(hmm,p1,10)
    bw2 = baumWelch(hmm,p2,10)
    #bw3 = baumWelch(hmm,seq3,10)
    bw3 = baumWelch(hmm,s,10)
    
    kl1.d = KL(rbind(bw$hmm$transProbs[1,], c(0.5, 0.5)), unit = "log")
    kl2.d = KL(rbind(bw$hmm$transProbs[2,], c(0.5, 0.5)), unit = "log")
    kl.d = append(kl.d, kl1.d + kl2.d)
    #kl.d[j] = kl1 + kl2
    
    kl1.p1 = KL(rbind(bw1$hmm$transProbs[1,], c(0.5, 0.5)), unit = "log")
    kl2.p1 = KL(rbind(bw1$hmm$transProbs[2,], c(0.5, 0.5)), unit = "log")
    kl.p1 = append(kl.p1, kl1.p1 + kl2.p1)
    #kl.p1[j] = kl1.p1 + kl2.p1
    
    kl1.p2 = KL(rbind(bw2$hmm$transProbs[1,], c(0.5, 0.5)), unit = "log")
    kl2.p2 = KL(rbind(bw2$hmm$transProbs[2,], c(0.5, 0.5)), unit = "log")
    kl.p2 = append(kl.p2, kl1.p2 + kl2.p2)
    #kl.p2[j] = kl1.p2 + kl2.p2
    
    kl1.s = KL(rbind(bw3$hmm$transProbs[1,], c(0.5, 0.5)), unit = "log")
    kl2.s = KL(rbind(bw3$hmm$transProbs[2,], c(0.5, 0.5)), unit = "log")
    kl.s = append(kl.s, kl1.s + kl2.s)
    #kl.s[j] = kl1.s + kl2.s
  }
}

# two sample t test: d - 1st, d - 2nd, 1st - 2nd, d - shuffled, spam - norm

kl.d1 = c()
kl.p11 = c()
kl.p21 = c()
kl.s1 = c()
for(j in unique(norm$workerid)){
  d = norm[norm$workerid==j,]$decision
  d = ifelse(d==1, 'different', 'same') # all data
  
  #breaks <- quantile(d, probs = seq(0, 1, 1/2))
  #my_parts <- cut(d, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  p1 = d[1:1000] # 1st partition
  p2 = d[1001:2000] # 2nd partition
  s = sample(d) # shuffled
  
  bw = baumWelch(hmm,d,10)
  bw1 = baumWelch(hmm,p1,10)
  bw2 = baumWelch(hmm,p2,10)
  #bw3 = baumWelch(hmm,seq3,10)
  bw3 = baumWelch(hmm,s,10)
  
  kl1.d = KL(rbind(bw$hmm$transProbs[1,], c(0.5, 0.5)), unit = "log")
  kl2.d = KL(rbind(bw$hmm$transProbs[2,], c(0.5, 0.5)), unit = "log")
  kl.d1 = append(kl.d1, kl1.d + kl2.d)
  #kl.d[j] = kl1 + kl2
  
  kl1.p1 = KL(rbind(bw1$hmm$transProbs[1,], c(0.5, 0.5)), unit = "log")
  kl2.p1 = KL(rbind(bw1$hmm$transProbs[2,], c(0.5, 0.5)), unit = "log")
  kl.p11 = append(kl.p11, kl1.p1 + kl2.p1)
  #kl.p1[j] = kl1.p1 + kl2.p1
  
  kl1.p2 = KL(rbind(bw2$hmm$transProbs[1,], c(0.5, 0.5)), unit = "log")
  kl2.p2 = KL(rbind(bw2$hmm$transProbs[2,], c(0.5, 0.5)), unit = "log")
  kl.p21 = append(kl.p21, kl1.p2 + kl2.p2)
  #kl.p2[j] = kl1.p2 + kl2.p2
  
  kl1.s = KL(rbind(bw3$hmm$transProbs[1,], c(0.5, 0.5)), unit = "log")
  kl2.s = KL(rbind(bw3$hmm$transProbs[2,], c(0.5, 0.5)), unit = "log")
  kl.s1 = append(kl.s1, kl1.s + kl2.s)
  #kl.s[j] = kl1.s + kl2.s
}

library(ggplot2)
length(kl.d)
length(kl.p1)
length(kl.p2)
length(kl.s)
length(kl.d1)
length(kl.p11)
length(kl.p21)
length(kl.s1)

x = as.numeric(rbind(kl.d, kl.p1, kl.p2, kl.s, kl.d1, kl.p11, kl.p21, kl.s1))
group = c(rep('KL Spammer', 1000),
          rep('KL Spammer P1', 1000),
          rep('KL Spammer P2', 1000),
          rep('KL Spammer Sfl', 1000),
          rep('KL Normal', 1000),
          rep('KL Normal P1', 1000),
          rep('KL Normal P2', 1000),
          rep('KL Normal Sfl', 1000))
df = data.frame(x, group)
ggplot(df, aes(x = x, colour = group)) +
  geom_density()


x = as.numeric(rbind(kl.d,  kl.d1))
group = c(rep('KL Spammer', 1000),
          rep('KL Normal', 1000))
df = data.frame(x, group)
ggplot(df, aes(x = x, colour = group)) +
  geom_density()


hist(kl.d1)
t.test(kl.d, kl.d1)

par(mfrow=c(2, 4))
hist(kl.d, prob=TRUE,xlab=" KL Spammer")
hist(kl.p1, prob=TRUE,xlab=" KL Spammer P1")
hist(kl.p2, prob=TRUE,xlab=" KL Spammer P2")
hist(kl.s, prob=TRUE,xlab=" KL Spammer Sfl")
hist(kl.d1, prob=TRUE,xlab=" KL Normal")
hist(kl.p11, prob=TRUE,xlab=" KL Normal P1")
hist(kl.p21, prob=TRUE,xlab=" KL Normal P2")
hist(kl.s1, prob=TRUE,xlab=" KL Normal Sfl")


plot(density(kl.s), col=5)
lines(density(kl.d), col=1)
lines(density(kl.p1), col=3)
lines(density(kl.p2), col=4)
legend("topright",legend=c("KL Spammer","KL Spammer P1", "Kl Spammer P2","Kl Spammer Sfl"),col=c(1,3,4, 5),lwd=3,bty="n")


plot(density(kl.d1), col=2)
lines(density(kl.s1), col=6)
lines(density(kl.p11), col=7)
lines(density(kl.p21), col=8)
legend("topright",legend=c("KL Normal","KL Normal P1", "Kl Normal P2","Kl Normal Sfl"),col=c(2,7,8, 6),lwd=3,bty="n")

plot(density(kl.s), col=5)
lines(density(kl.d), col=1)
lines(density(kl.p1), col=3)
lines(density(kl.p2), col=4)
lines(density(kl.d1), lty=2, lwd = 2, col=2)
lines(density(kl.s1), lty=4, lwd = 2, col=6)
lines(density(kl.p11), lty=4, lwd = 2, col=7)
lines(density(kl.p21), lty=4, lwd = 2, col=8)
legend("topright",legend=c("KL Spammer","KL Spammer P1", "Kl Spammer P2","Kl Spammer Sfl", "KL Normal","KL Normal P1", "Kl Normal P2","Kl Normal Sfl"),
       col=c(1, 3, 4, 5, 2, 7, 8, 6),lwd=3,bty="n")

