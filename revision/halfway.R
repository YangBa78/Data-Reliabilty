norm1 = sample_normal(12)
norm1$workerid = rep(121:132, each=n_image)

head(norm1)

# 
# for (i in 1:12) {
#   accuracy <- sum(data[data$workerid == i, ]$correct) / 80
#   print(paste("Worker", i, "Accuracy:", accuracy))
# }
# 
# 
# for (i in 121:132) {
#   accuracy <- sum(norm1[norm1$workerid == i, ]$correct) / 80
#   print(paste("Worker", i, "Accuracy:", accuracy))
# }


for (i in 1:12) {
  # Map workerid 1-12 to 121-132
  norm_workerid <- i + 120  
  
  data[data$workerid==i]$decision[1:40] = norm1[norm1$workerid==norm_workerid]$decision[1:40]
  data[data$workerid==i]$correct[1:40] = norm1[norm1$workerid==norm_workerid]$correct[1:40]
}


p = glmer(as.factor(decision) ~ (1 | workerid ) + (1 | imageid),      
          data = data, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)


workerid <- c()
loglik <- c()
p_value<- c()


for (x in unique(data$workerid)) {
  workerid <- append(workerid, x)
  p_temp <- exclude.influence(p, "workerid", x)
  loglik <- append(loglik, -2*(logLik(p)[1]- logLik(p_temp)[1]))
  
  p_value <- append(p_value, 1-pchisq(-2*(logLik(p)[1]- logLik(p_temp)[1]), df = 80))
}


df = data.frame(workerid,loglik, p_value)
df[df$p_value<=0.05,]



ggplot(df) +
  geom_point(aes(x = 1:nrow(df), y = loglik)) +
  geom_segment(aes(x = 1:nrow(df), xend = 1:nrow(df), y = 0, yend = loglik),
               color = 'cornflowerblue') +
  geom_hline(yintercept = qchisq(p=.95, df=80), color = 'salmon') +
  labs(x = 'Index of Worker ID', y = 'Deviance Distance',
       title = 'Deviance Distance of Worker i Removed',
       subtitle = 'Cutoff (p-value) based on a Likelihood Ratio Test (Chi-Sqaured) with a significance level of 0.05 and df = 80') +
  geom_text(aes(x = 1:nrow(df)), y = df$loglik,
            label = ifelse(df$p_value <=0.05,
                           df$workerid, ''), vjust = -1, size = 3)


ggplot(df) +
  geom_point(aes(x = 1:nrow(df), y = loglik)) +
  geom_segment(aes(x = 1:nrow(df), xend = 1:nrow(df), y = 0, yend = loglik),
               color = 'cornflowerblue') +
  geom_hline(yintercept = qchisq(p = 0.95, df = 80), color = 'salmon') +  # p = 0.05 threshold
  geom_hline(yintercept = qchisq(p = 0.9, df = 80), color = 'orange') +  # p = 0.1 threshold
  labs(x = 'Index of Worker ID', y = 'Deviance Distance',
       title = 'Deviance Distance of Worker i Removed',
       subtitle = 'Cutoff (p-value) based on a Likelihood Ratio Test (Chi-Squared) with a significance level of 0.05 and 0.1 and df = 80') +
  # Adding text for the p = 0.05 threshold line
  geom_text(aes(x = nrow(df) * 0.9, y = qchisq(p = 0.95, df = 80)), 
            label = "p = 0.05", color = 'salmon', size = 4, vjust = -1) +
  # Adding text for the p = 0.1 threshold line
  geom_text(aes(x = nrow(df) * 0.9, y = qchisq(p = 0.9, df = 80)), 
            label = "p = 0.1", color = 'orange', size = 4, vjust = 1.5) +
  # Adding text for workerid if p_value <= 0.05
  geom_text(aes(x = 1:nrow(df), y = loglik,
                label = ifelse(p_value <= 0.1, as.character(workerid), '')),
            vjust = -1, size = 3)

##################################### subset

data_half <- data[data$imageid %in% 41:80, ]
dim(data_half)
head(data_half, 81)



p = glmer(as.factor(decision) ~ (1 | workerid ) + (1 | imageid),      
          data = data_half, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)


workerid <- c()
loglik <- c()
p_value<- c()


for (x in unique(data_half$workerid)) {
  workerid <- append(workerid, x)
  p_temp <- exclude.influence(p, "workerid", x)
  loglik <- append(loglik, -2*(logLik(p)[1]- logLik(p_temp)[1]))
  
  p_value <- append(p_value, 1-pchisq(-2*(logLik(p)[1]- logLik(p_temp)[1]), df = 40))
}


df = data.frame(workerid,loglik, p_value)
df[df$p_value<=0.05,]
df[df$p_value<=0.1,]


# 
# ggplot(df) +
#   geom_point(aes(x = 1:nrow(df), y = loglik)) +
#   geom_segment(aes(x = 1:nrow(df), xend = 1:nrow(df), y = 0, yend = loglik),
#                color = 'cornflowerblue') +
#   geom_hline(yintercept = qchisq(p=.95, df=40), color = 'salmon') +
#   labs(x = 'Index of Worker ID', y = 'Deviance Distance',
#        title = 'Deviance Distance of Worker i Removed',
#        subtitle = 'Cutoff (p-value) based on a Likelihood Ratio Test (Chi-Sqaured) with a significance level of 0.05 and df = 40') +
#   geom_text(aes(x = 1:nrow(df)), y = df$loglik,
#             label = ifelse(df$p_value <=0.05,
#                            df$workerid, ''), vjust = -1, size = 3)


ggplot(df) +
  geom_point(aes(x = 1:nrow(df), y = loglik)) +
  geom_segment(aes(x = 1:nrow(df), xend = 1:nrow(df), y = 0, yend = loglik),
               color = 'cornflowerblue') +
  geom_hline(yintercept = qchisq(p = 0.95, df = 40), color = 'salmon') +  # p = 0.05 threshold
  geom_hline(yintercept = qchisq(p = 0.9, df = 40), color = 'orange') +  # p = 0.1 threshold
  labs(x = 'Index of Worker ID', y = 'Deviance Distance',
       title = 'Deviance Distance of Worker i Removed',
       subtitle = 'Cutoff (p-value) based on a Likelihood Ratio Test (Chi-Squared) with a significance level of 0.05 and 0.1 and df = 40') +
  # Adding text for the p = 0.05 threshold line
  geom_text(aes(x = nrow(df) * 0.9, y = qchisq(p = 0.95, df = 40)), 
            label = "p = 0.05", color = 'salmon', size = 4, vjust = -1) +
  # Adding text for the p = 0.1 threshold line
  geom_text(aes(x = nrow(df) * 0.9, y = qchisq(p = 0.9, df = 40)), 
            label = "p = 0.1", color = 'orange', size = 4, vjust = 1.5) +
  # Adding text for workerid if p_value <= 0.05
  geom_text(aes(x = 1:nrow(df), y = loglik,
                label = ifelse(p_value <= 0.1, as.character(workerid), '')),
            vjust = -1, size = 3)






####################### mc
library(markovchain)
library(philentropy)

klr.1 = c()
klr.2 = c()
klr = c()

klp1.1 = c()
klp1.2 = c()
klp1 = c()

klp2.1 = c()
klp2.2 = c()
klp2 = c()

kls1.1 = c()
kls1.2 = c()
kls1 = c()

kls2.1 = c()
kls2.2 = c()
kls2 = c()
#ll = c()

for (i in unique(data_half$workerid)){
  print(i)
  fit<-markovchainFit(data_half[data_half$workerid==i,]$decision)
  
  klr.1 = append(klr.1, as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0.5, 0.5)), unit = "log")))
  klr.2 = append(klr.2, as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0.5, 0.5)), unit = "log")))
  klr = append(klr, as.numeric((klr.1 + klr.2)/2))

  kls1.1 = append(kls1.1, as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(1, 0)), unit = "log")))
  kls1.2 = append(kls1.2, as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0, 1)), unit = "log")))
  kls1 = append(kls1, as.numeric((kls1.1 + kls1.2)/2))
  
  kls2.1 = append(kls2.1, as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0, 1)), unit = "log")))
  kls2.2 = append(kls2.2, as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(1, 0)), unit = "log")))
  kls2 = append(kls2, as.numeric((kls2.1 + kls2.2)/2))
  
  klp1.1 = append(klp1.1, as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(1, 0)), unit = "log")))
  klp1.2 = append(klp1.2, as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(1, 0)), unit = "log")))
  klp1 = append(klp1, as.numeric((klp1.1 + klp1.2)/2))
  
  klp2.1 = append(klp2.1, as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0, 1)), unit = "log")))
  klp2.2 = append(klp2.2, as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0, 1)), unit = "log")))
  klp2 = append(klp2, as.numeric((klp2.1 + klp2.2)/2))
}


length(klr)


# Initialize empty numeric vectors of correct length
num_workers <- length(unique(data_half$workerid))
klr <- numeric(num_workers)
klr.1 <- numeric(num_workers)
klr.2 <- numeric(num_workers)

kls1 <- numeric(num_workers)
kls1.1 <- numeric(num_workers)
kls1.2 <- numeric(num_workers)

kls2 <- numeric(num_workers)
kls2.1 <- numeric(num_workers)
kls2.2 <- numeric(num_workers)

klp1 <- numeric(num_workers)
klp1.1 <- numeric(num_workers)
klp1.2 <- numeric(num_workers)

klp2 <- numeric(num_workers)
klp2.1 <- numeric(num_workers)
klp2.2 <- numeric(num_workers)

# Loop over unique worker IDs
for (idx in seq_along(unique(data_half$workerid))) {
  i <- unique(data_half$workerid)[idx]
  print(i)
  
  # Fit Markov Chain model for the current worker
  fit <- markovchainFit(data_half[data_half$workerid == i,]$decision)
  
  # Compute KL divergences for different reference distributions
  klr.1[idx] <- as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0.5, 0.5)), unit = "log"))
  klr.2[idx] <- as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0.5, 0.5)), unit = "log"))
  klr[idx] <- (klr.1[idx] + klr.2[idx]) / 2
  
  kls1.1[idx] <- as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(1, 0)), unit = "log"))
  kls1.2[idx] <- as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0, 1)), unit = "log"))
  kls1[idx] <- (kls1.1[idx] + kls1.2[idx]) / 2
  
  kls2.1[idx] <- as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0, 1)), unit = "log"))
  kls2.2[idx] <- as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(1, 0)), unit = "log"))
  kls2[idx] <- (kls2.1[idx] + kls2.2[idx]) / 2
  
  klp1.1[idx] <- as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(1, 0)), unit = "log"))
  klp1.2[idx] <- as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(1, 0)), unit = "log"))
  klp1[idx] <- (klp1.1[idx] + klp1.2[idx]) / 2
  
  klp2.1[idx] <- as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0, 1)), unit = "log"))
  klp2.2[idx] <- as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0, 1)), unit = "log"))
  klp2[idx] <- (klp2.1[idx] + klp2.2[idx]) / 2
}

kl = data.frame(unique(data_half$workerid), klr, kls1, kls2, klp1, klp2)
kl


head(kl[order(kl$klr), ])
head(kl[order(kl$kls1), ])
head(kl[order(kl$kls2), ])
head(kl[order(kl$klp1), ])
head(kl[order(kl$klp2), ])

mean(kl$klr) # 0.07640975
mean(kl$kls1) #  5.61902
mean(kl$kls2) # 4.66043
mean(kl$klp1) # 4.941094
mean(kl$klp2) # 5.338356





