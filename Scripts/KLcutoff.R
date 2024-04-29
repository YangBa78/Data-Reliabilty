library(markovchain)
library(philentropy)
library(ggplot2)


set.seed(42)
# data
n_sample = 30000
random = sample_random(n_sample)
random$workerid = rep(1:n_sample, each=n_image)

primary1.1 = sample_primary1(n_sample)
primary2.1 = sample_primary1(n_sample/2)
primary1 = rbind(primary1.1, primary2.1)[1:(n_sample*n_image),]
dim(primary1)
primary1$workerid = rep(1:n_sample, each=n_image) 


primary1.2 = sample_primary2(n_sample)
primary2.2 = sample_primary2(n_sample/2)
primary2 = rbind(primary1.2, primary2.2)[1:(n_sample*n_image),]
dim(primary2)
primary2$workerid = rep(1:n_sample, each=n_image) 


strong1 = sample_strong_pattern1(n_sample)
strong1$workerid = rep(1:n_sample, each=n_image)

strong2 = sample_strong_pattern2(n_sample)
strong2$workerid = rep(1:n_sample, each=n_image)

weak1 = sample_weak_pattern1(n_sample)
weak1$workerid = rep(1:n_sample, each=n_image)

weak2 = sample_weak_pattern2(n_sample)
weak2$workerid = rep(1:n_sample, each=n_image)

normal = sample_normal(n_sample) 
normal$workerid = rep(1:n_sample, each=n_image)




# MC 
kl.random = c()
kl.sfl = c()
kl.sflpls = c()
for (i in 1:n_sample){
  fit<-markovchainFit(random[random$workerid==i,]$decision)
  kl1.s = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0.5, 0.5)), unit = "log"))
  kl2.s = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0.5, 0.5)), unit = "log"))
  kl.random = append(kl.random, as.numeric((kl1.s + kl2.s)/2))
  
  sfl = sample(random[random$workerid==i,]$decision)
  fit1<-markovchainFit(sfl)
  kl1.sfl = as.numeric(KL(rbind(fit1$estimate@transitionMatrix[1,], fit$estimate@transitionMatrix[1,]), unit = "log"))
  kl2.sfl = as.numeric(KL(rbind(fit1$estimate@transitionMatrix[2,], fit$estimate@transitionMatrix[2,]), unit = "log"))
  kl.sfl = append(kl.sfl, as.numeric((kl1.sfl + kl2.sfl)/2))
  
  sflpls = append(random[random$workerid==i,]$decision, sfl)
  fit2<-markovchainFit(sflpls)
  kl1.sflpls = as.numeric(KL(rbind(fit2$estimate@transitionMatrix[1,], fit$estimate@transitionMatrix[1,]), unit = "log"))
  kl2.sflpls = as.numeric(KL(rbind(fit2$estimate@transitionMatrix[2,], fit$estimate@transitionMatrix[2,]), unit = "log"))
  kl.sflpls = append(kl.sflpls, as.numeric((kl1.sflpls + kl2.sflpls)/2))
}


kl.normalr = c()
kl.normalsfl = c()
kl.normalsflpls = c()
kl.normalpr1 = c()
kl.normalpr2 = c()
kl.normalptn1 = c()
kl.normalptn2 = c()
for (i in 1:n_sample){
  # random
  fit<-markovchainFit(normal[normal$workerid==i,]$decision)
  kl1.n = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0.5, 0.5)), unit = "log"))
  kl2.n = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0.5, 0.5)), unit = "log"))
  kl.normalr = append(kl.normalr, as.numeric((kl1.n + kl2.n)/2))
  
  # primary
  kl1.n1 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(1, 0)), unit = "log"))
  kl2.n1 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(1, 0)), unit = "log"))
  kl.normalpr1 = append(kl.normalpr1, as.numeric((kl1.n1 + kl2.n1)/2))
  
  kl1.n2 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0, 1)), unit = "log"))
  kl2.n2 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0, 1)), unit = "log"))
  kl.normalpr2 = append(kl.normalpr2, as.numeric((kl1.n2 + kl2.n2)/2))
  
  # pattern
  kl1.n3 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(1, 0)), unit = "log"))
  kl2.n3 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0, 1)), unit = "log"))
  kl.normalptn1 = append(kl.normalptn1, as.numeric((kl1.n3 + kl2.n3)/2))
  
  kl1.n4 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0, 1)), unit = "log"))
  kl2.n4 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(1, 0)), unit = "log"))
  kl.normalptn2 = append(kl.normalptn2, as.numeric((kl1.n4 + kl2.n4)/2))
  
  # shuffled 
  sfl = sample(normal[normal$workerid==i,]$decision)
  fit1<-markovchainFit(sfl)
  kl1.sfl = as.numeric(KL(rbind(fit1$estimate@transitionMatrix[1,], fit$estimate@transitionMatrix[1,]), unit = "log"))
  kl2.sfl = as.numeric(KL(rbind(fit1$estimate@transitionMatrix[2,], fit$estimate@transitionMatrix[2,]), unit = "log"))
  kl.normalsfl = append(kl.normalsfl, as.numeric((kl1.sfl + kl2.sfl)/2))
  
  sflpls = append(normal[normal$workerid==i,]$decision, sfl)
  fit2<-markovchainFit(sflpls)
  kl1.sflpls = as.numeric(KL(rbind(fit2$estimate@transitionMatrix[1,], fit$estimate@transitionMatrix[1,]), unit = "log"))
  kl2.sflpls = as.numeric(KL(rbind(fit2$estimate@transitionMatrix[2,], fit$estimate@transitionMatrix[2,]), unit = "log"))
  kl.normalsflpls = append(kl.normalsflpls, as.numeric((kl1.sflpls + kl2.sflpls)/2))
}



kl.p1.1 = c()
kl.p1.2 = c()
for (i in 1:n_sample){
  fit<-markovchainFit(primary1[primary1$workerid==i,]$decision)
  kl1.p1 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(1, 0)), unit = "log"))
  kl2.p1 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(1, 0)), unit = "log"))
  kl.p1.1 = append(kl.p1.1, as.numeric((kl1.p1 + kl2.p1)/2))
  
  kl1.p2 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0, 1)), unit = "log"))
  kl2.p2 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0, 1)), unit = "log"))
  kl.p1.2 = append(kl.p1.2, as.numeric((kl1.p2 + kl2.p2)/2))
}


kl.p2.1 = c()
kl.p2.2 = c()
for (i in 1:n_sample){
  fit<-markovchainFit(primary2[primary2$workerid==i,]$decision)
  kl1.p1 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(1, 0)), unit = "log"))
  kl2.p1 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(1, 0)), unit = "log"))
  kl.p2.1 = append(kl.p2.1, as.numeric((kl1.p1 + kl2.p1)/2))
  
  kl1.p2 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0, 1)), unit = "log"))
  kl2.p2 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0, 1)), unit = "log"))
  kl.p2.2 = append(kl.p2.2, as.numeric((kl1.p2 + kl2.p2)/2))
}



kl.strong1.1 = c()
kl.strong1.2 = c()
for (i in 1:n_sample){
  fit<-markovchainFit(strong1[strong1$workerid==i,]$decision)
  kl1.s = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(1, 0)), unit = "log"))
  kl2.s = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0, 1)), unit = "log"))
  kl.strong1.1 = append(kl.strong1.1, as.numeric((kl1.s + kl2.s)/2))

  kl1.s1 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0, 1)), unit = "log"))
  kl2.s2 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(1, 0)), unit = "log"))
  kl.strong1.2 = append(kl.strong1.2, as.numeric((kl1.s1 + kl2.s2)/2))
}


kl.strong2.1 = c()
kl.strong2.2 = c()
for (i in 1:n_sample){
  fit<-markovchainFit(strong2[strong2$workerid==i,]$decision)
  kl1.s = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(1, 0)), unit = "log"))
  kl2.s = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0, 1)), unit = "log"))
  kl.strong2.1 = append(kl.strong2.1, as.numeric((kl1.s + kl2.s)/2))

  kl1.s1 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0, 1)), unit = "log"))
  kl2.s2 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(1, 0)), unit = "log"))
  kl.strong2.2 = append(kl.strong2.2, as.numeric((kl1.s1 + kl2.s2)/2))
}



kl.weak1.1 = c()
kl.weak1.2 = c()
for (i in 1:n_sample){
  fit<-markovchainFit(weak1[weak1$workerid==i,]$decision)
  kl1.w = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(1, 0)), unit = "log"))
  kl2.w = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0, 1)), unit = "log"))
  kl.weak1.1 = append(kl.weak1.1, as.numeric((kl1.w + kl2.w)/2))
  
  kl1.w1 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0, 1)), unit = "log"))
  kl2.w1 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(1, 0)), unit = "log"))
  kl.weak1.2 = append(kl.weak1.2, as.numeric((kl1.w1 + kl2.w1)/2))
}


kl.weak2.1 = c()
kl.weak2.2 = c()
for (i in 1:n_sample){
  fit<-markovchainFit(weak2[weak2$workerid==i,]$decision)
  kl1.w = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(1, 0)), unit = "log"))
  kl2.w = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0, 1)), unit = "log"))
  kl.weak2.1 = append(kl.weak2.1, as.numeric((kl1.w + kl2.w)/2))
  
  kl1.w1 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0, 1)), unit = "log"))
  kl2.w1 = as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(1, 0)), unit = "log"))
  kl.weak2.2 = append(kl.weak2.2, as.numeric((kl1.w1 + kl2.w1)/2))
}




######### cutoff & plot 
# primary1
p1 <- hist(kl.normalpr1)             
p2 <- hist(kl.p1.2)    
plot( p1, col=rgb(0,0,1,1/4), xlim = c(0, 7), main = 'AKLD Histograms in Simulation', xlab = 'AKLD')  
plot( p2, col=rgb(1,0,0,1/4), add=T)  
abline(v=cutoff.p1, col='red', lwd=3, lty='dashed')
#legend(2, 3000, legend = c('normal', 'primary'), 
#       col = c('purple', 'orange'))
legend("topright", legend = c("Normal Workers", "Primary Choice Spammers"), fill = c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)))

# cutoff.p1 <- quantile(kl.p1.2, 0.95)
# length(kl.normalpr1[kl.normalpr1<cutoff.p1]) /n_sample  # type2 error
# cutoff.p1


n = data.frame('AKLD' = kl.normalpr1)    
r = data.frame('AKLD' = kl.p1.2)    
n$worker = 'normal'
r$worker = 'primary choice'
b = rbind(n, r)
ggplot(b, aes(AKLD, fill = worker)) + geom_density(alpha = 0.4)


# cutoff1
cutoff.p1 <- quantile(kl.normalpr1, 0.05)
length(kl.p1.2[kl.p1.2>cutoff.p1]) /n_sample  # type2 error
cutoff.p1

# primary2
p1 <- hist(kl.normalpr2)               
p2 <- hist(kl.p2.1)  
plot( p1, col=rgb(0,0,1,1/4), xlim = c(0, 9), main = 'primary type2')  
plot( p2, col=rgb(1,0,0,1/4), add=T)  

# cutoff.p2 <- quantile(kl.p2.1, 0.95)
# length(kl.normalpr2[kl.normalpr2<cutoff.p2]) /n_sample  # type2 error
# cutoff.p2

# cutoff2
cutoff.p2 <- quantile(kl.normalpr2, 0.05)
length(kl.p2.1[kl.p2.1>cutoff.p2]) /n_sample  # type2 error
cutoff.p2

# strong 1
p1 <- hist(kl.normalptn1)
p2 <- hist(kl.strong1.2)
plot( p1, col=rgb(0,0,1,1/4), xlim = c(0, 9), main = 'strong pattern type1')
plot( p2, col=rgb(1,0,0,1/4), add=T)

cutoff.s1 <- quantile(kl.strong1.2, 0.95)
length(kl.normalptn1[kl.normalptn1<cutoff.s1]) /n_sample  # type2 error
cutoff.s1

# cutoff3
cutoff.s1 <- quantile(kl.normalptn1, 0.05)
length(kl.strong1.2[kl.strong1.2>cutoff.s1]) /n_sample  # type2 error

cutoff.s1


# strong 2
p1 <- hist(kl.normalptn2)
p2 <- hist(kl.strong2.2)
plot( p1, col=rgb(0,0,1,1/4), xlim = c(0, 9), main = 'strong pattern type1')
plot( p2, col=rgb(1,0,0,1/4), add=T)

cutoff.s2 <- quantile(kl.strong2.2, 0.95)
length(kl.normalptn2[kl.normalptn2<cutoff.s2]) /n_sample  # type2 error
cutoff.s2

# cutoff4
cutoff.s2 <- quantile(kl.normalptn2, 0.05)
length(kl.strong2.2[kl.strong2.2>cutoff.s2]) /n_sample  # type2 error
cutoff.s2

# weak 1
p1 <- hist(kl.normalptn1)               
p2 <- hist(kl.weak1.2)  
plot( p1, col=rgb(0,0,1,1/4), xlim = c(0, 9), main = 'AKLD Histograms in Simulation', xlab = 'AKLD')
plot( p2, col=rgb(1,0,0,1/4), add=T)
abline(v=cutoff.w1, col='red', lwd=3, lty='dashed')
legend("topright", legend = c("Normal Workers", "Repeated Pattern Spammers"), fill = c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)))

# cutoff.w1 <- quantile(kl.weak1.2, 0.95)
# length(kl.normalptn1[kl.normalptn1<cutoff.w1]) /n_sample  # type2 error
# cutoff.w1

# cutoff5
cutoff.w1 <- quantile(kl.normalptn1, 0.05)
length(kl.weak1.2[kl.weak1.2>cutoff.w1]) /n_sample  # type2 error
cutoff.w1


# weak 2
p1 <- hist(kl.normalptn2)               
p2 <- hist(kl.weak2.2)  
plot( p2, col=rgb(1,0,0,1/4), xlim = c(0, 7), main = 'AKLD Histograms in Simulation', xlab = 'AKLD')
plot( p1, col=rgb(0,0,1,1/4), add=T)
abline(v=cutoff.w2, col='red', lwd=3, lty='dashed')
legend("topright", legend = c("Normal Workers", "Repeated Pattern Spammers"), fill = c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)))

# cutoff.w2 <- quantile(kl.weak2.2, 0.95)
# length(kl.normalptn2[kl.normalptn2<cutoff.w2]) /n_sample  # type2 error
# cutoff.w2

# cutoff6
cutoff.w2 <- quantile(kl.normalptn2, 0.05)
length(kl.weak2.2[kl.weak2.2>cutoff.w2]) /n_sample  # type2 error
cutoff.w2



n = data.frame('AKLD' = kl.normalptn2)  
r = data.frame('AKLD' = kl.weak2.2)  
n$worker = 'normal'
r$worker = 'repeated pattern'
b = rbind(n, r)
ggplot(b, aes(AKLD, fill = worker)) + geom_density(alpha = 0.4)



# random
p1 <- hist(sqrt(kl.normalr))              
p2 <- hist(sqrt(kl.random))  
plot( p1, col=rgb(0,0,1,1/4), xlim = c(0, 0.4), main = 'random')  
plot( p2, col=rgb(1,0,0,1/4), add=T)  



cutoff.random <- quantile(sqrt(kl.random), 0.95)
length(sqrt(kl.normalr)[sqrt(kl.normalr)<cutoff.random]) /n_sample  # type2 error
cutoff.random

# cutoff7
cutoff.random <- quantile(kl.normalr, 0.05)

cutoff.random <- quantile(sqrt(kl.normalr), 0.05)
length(sqrt(kl.random)[sqrt(kl.random)>cutoff.random]) /n_sample  # type2 error
cutoff.random

cutoff.random <- quantile(sqrt(kl.normalr), 0.05)
length(kl.random[kl.random^2>cutoff.random]) /n_sample  # type2 error
cutoff.random

n = data.frame('Square Root of AKLD' = sqrt(kl.normalr))
r = data.frame('Square Root of AKLD' = sqrt(kl.random))
n$worker = 'normal'
r$worker = 'random guess'
b = rbind(n, r)
ggplot(b, aes(Square.Root.of.AKLD , fill = worker)) + geom_density(alpha = 0.4) 

ks.test(kl.normalr, kl.random, exact = TRUE)


abline(v=cutoff.random, col='red', lwd=3, lty='dashed')
head(b)



p1 <- hist(sqrt(kl.sfl))               
p2 <- hist(sqrt(kl.sflpls))  
plot( p1, col=rgb(0,0,1,1/4), xlim = c(0, 0.4), main = 'random')  
plot( p2, col=rgb(1,0,0,1/4), add=T)  


p1 <- hist(sqrt(kl.sfl))               
p2 <- hist(sqrt(kl.normalsfl))  
plot( p2, col=rgb(0,0,1,1/4), xlim = c(0, 0.4), main = 'random')  
plot( p1, col=rgb(1,0,0,1/4), add=T)  

mean(kl.sfl)
mean(kl.sflpls)
mean(kl.random)
mean(kl.normalr)


# Create example data
set.seed(123)
data1 <- rnorm(1000, mean = 0, sd = 1)
data2 <- rnorm(1000, mean = 2, sd = 1)

# Plot overlapping histograms
hist(data1, col = "blue", xlim = c(-5, 7), ylim = c(0, 0.25), breaks = 30, main = "Overlapping Histograms")
hist(data2, col = "red", add = TRUE, breaks = 30)

# Add legend
legend("topright", legend = c("Data 1", "Data 2"), fill = c("blue", "red"))

