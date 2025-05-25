library(openxlsx)
library(markovchain)
library(philentropy)

# variance
var_image <- 6

#n_image <- 2000


n <- seq(100, 2000, by=100)
t05 = c()
t95 = c()

for (n_image in n){
  def.image <- defData(varname = "image_eff", dist = "normal", formula = 0, variance = var_image,
                       id = "imageid")
  #def.image <- defData(def.image, varname = "nWorker", dist = "nonrandom")
  
  # generate task data
  dtImage <- genData(n_image, def.image)
  
  # assign task difficulty and true label
  dtImage$task <- ifelse(abs(dtImage$image_eff)< median(abs(dtImage$image_eff)), 'hard', 'easy')
  # different: 1, same: 0
  dtImage$trueLable <- ifelse(dtImage$image_eff > median(dtImage$image_eff), 1, 0)
  
  n_sample = 10000
  random = sample_random(n_sample)
  random$workerid = rep(1:n_sample, each=n_image)
  
  normal = sample_normal(n_sample) 
  normal$workerid = rep(1:n_sample, each=n_image)
  
  kl.random = c()
  for (i in 1:n_sample){
    fit<-markovchainFit(random[random$workerid==i,]$decision)
    kl1.s = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0.5, 0.5)), unit = "log"))
    kl2.s = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0.5, 0.5)), unit = "log"))
    kl.random = append(kl.random, as.numeric((kl1.s + kl2.s)/2))
  }
  
  
  kl.normalr = c()
  for (i in 1:n_sample){
    # random
    fit<-markovchainFit(normal[normal$workerid==i,]$decision)
    kl1.n = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0.5, 0.5)), unit = "log"))
    kl2.n = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0.5, 0.5)), unit = "log"))
    kl.normalr = append(kl.normalr, as.numeric((kl1.n + kl2.n)/2))
  }
  
  cutoff.random <- quantile(sqrt(kl.normalr), 0.05)
  t05 = append(t05, length(sqrt(kl.random)[sqrt(kl.random)>cutoff.random]) /n_sample)  # type2 error
  
  
  cutoff.random1 <- quantile(sqrt(kl.random), 0.95)
  t95 = append(t95, length(sqrt(kl.normalr)[sqrt(kl.normalr)<cutoff.random1]) /n_sample)  # type2 error
  
}



t = data.frame(n, t05, t95)
colnames(t)= c('No. of Tasks', 'Type II Error', 'Type II Error.')


ggplot(t, aes(x = `No. of Tasks`, y = `Type II Error`)) 


ggplot(t, aes(x = `No. of Tasks`, y = `Type II Error`)) +
  geom_point() + 
  stat_smooth(method=lm)



write.xlsx(t05, file = "Simulated data/type205.xlsx", append = FALSE)
write.xlsx(t95, file = "Simulated data/type295.xlsx", append = FALSE)





kl.random = c()
for (i in 1:n_sample){
  fit<-markovchainFit(random[random$workerid==i,]$decision)
  kl1.s = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0.5, 0.5)), unit = "log"))
  kl2.s = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0.5, 0.5)), unit = "log"))
  kl.random = append(kl.random, as.numeric((kl1.s + kl2.s)/2))
}



kl.normalr = c()
for (i in 1:n_sample){
  # random
  fit<-markovchainFit(normal[normal$workerid==i,]$decision)
  kl1.n = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0.5, 0.5)), unit = "log"))
  kl2.n = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0.5, 0.5)), unit = "log"))
  kl.normalr = append(kl.normalr, as.numeric((kl1.n + kl2.n)/2))
  
}


# cutoff7
cutoff.random <- quantile(sqrt(kl.normalr), 0.05)
length(sqrt(kl.random)[sqrt(kl.random)>cutoff.random]) /n_sample  # type2 error
cutoff.random


cutoff.random1 <- quantile(sqrt(kl.random), 0.95)
length(sqrt(kl.normalr)[sqrt(kl.normalr)<cutoff.random1]) /n_sample  # type2 error
cutoff.random1


n = data.frame('Square Root of AKLD' = sqrt(kl.normalr))
r = data.frame('Square Root of AKLD' = sqrt(kl.random))
n$worker = 'normal'
r$worker = 'random guess'
b = rbind(n, r)
ggplot(b, aes(Square.Root.of.AKLD , fill = worker)) + geom_density(alpha = 0.4) 
