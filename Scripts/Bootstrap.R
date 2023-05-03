# parametric bootstrap
set.seed(1)
sample_variances <- rep(NA, 10^4)
for(i in seq_along(sample_variances)){
  samp <- rnorm(15, mean=500, sd=100)
  samp <- round(samp, -1)
  samp <- ifelse(samp > 800, 800, samp)
  samp <- ifelse(samp < 200, 200, samp)
  sample_variances[i] <- var(samp)
  highest_score[i] <- max(samp)
}

summary(sample_variances)
hist(sample_variances)
# var(samp)


x <- seq(0, 40, by = 0.01)
d <- dchisq(x, 14)
scaled <- (15-1) /(100^2)*sample_variances
hist(scaled, freq = FALSE, main = "Histogram of scaled sample variances")
lines(x, d, col = 'red')


# inference 
mean(sample_variances>=20000)
#mean(highest_score >=750)



# nonparametric bootstrap
set.seed(1)
differences <- rep(NQ, 10000)
for (i in seq_along(differences)){
  groupA <- sample(A, 116, replace = TRUE)
  groupB <- sample(B, 34, replace = TRUE)
  differences[i] <- mean(groupA) - mean(groupB)
}

 mean(abs(differences) >= abs(observed_diff))


library(boot)

 
  
 
 
 
 