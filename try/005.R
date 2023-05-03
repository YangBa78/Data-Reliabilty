# 
sort(rle(c(1,1,1, 1, 1, 1, 1, 0, 1,1, 1, 1, 1, 1, 1))[1]$lengths, decreasing =TRUE)

tes = rep(c(0,1), times = 40) 
rle(c(1,1,1, 1, 1, 1, 1, 0, 1,1, 1, 1, 1))[1]$lengths
rle(tes)[1]$lengths
hist(rle(dt[dt$workerid==140,]$decision)$lengths)
table(rle(dt[dt$workerid==14,]$decision)$lengths)
rle(dt[dt$workerid==2,]$decision)$lengths
dt[dt$workerid==2,]$decision
as.numeric(table(rle(dt[dt$workerid==2,]$decision)$lengths))[1]
# maximum number: how long to choose one
# 
sum(dt[dt$workerid==32,]$accuracy)/80
sum(dt[dt$workerid==140,]$accuracy)/80


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

dchisq(14, 10)

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





newT1$CT = ifelse(newT1$trueLable=='different', 1, 0)
newT1$C = ifelse(newT1$decision == newT1$CT, 1, 0)
a = newT1 %>% group_by(trueLable) %>% summarise(acc = sum(C), n = n())



# if I simulate very low accuracy to see loglik range !!!
# all possible situations


tbl = matrix(data=c(55, 45, 20, 30), nrow=2, ncol=2, byrow=T)
dimnames(tbl) = list(City=c('B', 'T'), Gender=c('M', 'F'))

chi2 = chisq.test(tbl, correct=F)
#table(newT1$task, newT1$decision)
c(chi2$statistic, chi2$p.value)
sqrt(chi2$statistic / sum(tbl))


tbl = matrix(data=c(51, 49, 24, 26), nrow=2, ncol=2, byrow=T)
dimnames(tbl) = list(City=c('B', 'T'), Gender=c('M', 'F'))

chi2 = chisq.test(tbl, correct=F)
c(chi2$statistic, chi2$p.value)

sqrt(chi2$statistic / sum(tbl))

chi2 = chisq.test(table(newT$task, newT$decision), correct=F) 
chi2 = chisq.test(table(newT1$task, newT1$decision), correct=F) 
chi2 = chisq.test(table(dt[dt$workerid==1,]$task, dt[dt$workerid==1,]$decision), correct=F) 


for (i in unique(dt$workerid)){
  chi2 = chisq.test(table(dt[dt$workerid==i,]$task, dt[dt$workerid==i,]$decision), correct=F)
  x[i] <- sqrt(chi2$statistic / sum(table(dt[dt$workerid==i,]$task, dt[dt$workerid==i,]$decision)))
}

x[1]
x[2]


library(psych)
x = table(dt[dt$workerid==1,]$task, dt[dt$workerid==1,]$decision)
x = table(newT1$task, newT1$decision)
tetrachoric(x,y=NULL,correct=.5,smooth=TRUE,global=TRUE,weight=NULL,na.rm=TRUE,
            delete=TRUE)




# Generate a random sample of 100 observations from a normal distribution with mean 0 and standard deviation 1
set.seed(123)
sample_data <- rnorm(100, mean = 0, sd = 1)

# Calculate the sample mean
sample_mean <- mean(sample_data)

# Generate 1000 random samples of size 100 from the same distribution
n_samples <- 1000
sample_means <- replicate(n_samples, mean(rnorm(100, mean = 0, sd = 1)))

# Plot the empirical sampling distribution of the sample mean
hist(sample_means, breaks = 30, main = "Empirical Sampling Distribution of the Sample Mean", xlab = "Sample Mean")
abline(v = sample_mean, col = "red", , lwd = 2)



library(tidyverse)
library(timetk)
library(tibbletime)
library(lubridate)
library(forecast)


set.seed(123)

# Create a data frame with the categorical variable and the target variable
df <- tibble(
  task_difficulty = sample(c("easy", "medium", "hard"), 100, replace = TRUE),
  same_diff = sample(c("same", "different"), 100, replace = TRUE)
)

# Create a time series object
ts <- df %>%
  mutate(date = seq.Date(from = ymd("20220101"), by = "day", length.out = n())) %>%
  select(date, same_diff) %>%
  tk_ts(start = "2022-01-01", freq = "day")

# Create a random walk model
rw_model <- rwf(ts)

# Plot the random walk model
autoplot(rw_model) +
  labs(title = "Random Walk Model for Same vs Different", y = "Same vs Different")





int_sd = sample(runif(160, 0.5, 2))
int_eff = c()

# for each worker i int_eff: (worker_i: easy, worker_i: hard) should be mean: 0, sd: random(range(int_sd))
for(x in int_sd){
  int_eff <- append(int_eff, rnorm(n = 2, mean = 0, sd = x)) 
}

interact = data.frame( 'workerid' = rep(1:160, each = 2) ,
                       'task' = c('easy', 'hard'),
                       'int_eff' = int_eff)


rbinom(1, 1, prob = 0.5)
00
