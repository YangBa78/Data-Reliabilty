y = c(rep(TRUE, 10), rep(FALSE, 24), rep(TRUE, 4), rep(FALSE, 12))

t = y[1:34]
c = y[35:50]
mean(t)
mean(c)
ob_df = mean(t)-mean(c)
ob_df

y
random = sample(y)
a = random[1:34]
b = random[35:50]
mean(a)
mean(b)
ran_df = mean(a)-mean(b)
ran_df
dif = rep(NA, 10000)
for(i in seq_along(dif)){
  random <- sample(y)
  a = random[1:34]
  b = random[35:50]
  dif[i] = mean(a) - mean(b)
}
summary(dif)


mean(dif>=ob_df)


auc



t = rep(NA, 10000)
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
t0

library(DescTools)
x <- seq(0, pi, length.out=200)
AUC(x=x, y=sin(x)) 
