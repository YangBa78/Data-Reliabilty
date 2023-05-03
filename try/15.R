differences <- rep(NA, 10^4)
spam <- rep(NA, 10^4)
norm <- rep(NA, 10^4)

for(i in seq_along(differences)){
  # difference to the rest low accuacy vs norm
  # just count observation 
  #samples[i] <- sort(rle(dt[dt$workerid==i, ]$decision)$lengths, decreasing = TRUE)[1]
  spam1 <- sample(low_acc, length(low_acc), replace = TRUE)
  norm1 <- sample(setdiff(unique(dt$workerid), low_acc), length(low_acc), replace = TRUE)
  for (a in spam1){
   spam[i]  <- sort(rle(dt[dt$workerid==a, ]$decision)$lengths, decreasing = TRUE)[1]
  }
  for (b in norm1){
   norm[i]  <- sort(rle(dt[dt$workerid==b, ]$decision)$lengths, decreasing = TRUE)[1]
  }
  differences[i] <- mean(spam[i]) - mean(norm[i])
}


for (i in low_acc){
  dt
}



#obs_diff = 
mean(abs(differences))


  
# 670 
# extreme value distribution: sampling distribution 
x_bar - t dist


# time - 
# time - response 
# acc no related task difficulty 
# unitlize time -  fuzhu response 
# simulation based - emperical sampling distribution , different hypothesis, construst different hoptehsis 
# Paper
# real data for testing 
# different data, different model
# statistic: metric
# 

# scale. AI
# syntehtic data 
# privacy - good enough tio real data, 
# train model via sytentic data
# training data 
# transformer 
# GAN 
# sythetnic data 





# differences
# length(setdiff(unique(dt$workerid), low_acc))



as.numeric(table(rle(dt[dt$workerid==2,]$decision)$lengths))[1]
table(rle(dt[dt$workerid==2,]$decision)$lengths)


sort(rle(c(1,1,1, 1, 1, 1, 1, 0, 1,1, 1, 1, 1, 1, 1))[1]$lengths, decreasing =TRUE)[1]


h = c()
for (i in high_acc){
     h[i] <- sort(rle(dt[dt$workerid==i, ]$decision)$lengths, decreasing = TRUE)[1]
     }
hist(h,freq = FALSE)
x <- seq(0, 40, by = 0.01)
d = dchisq(x, 10)
lines(x, d, col = 'red')






