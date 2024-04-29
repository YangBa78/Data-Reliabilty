library(lme4)
library(influence.ME)
require(Matrix)
library(dplyr)
library(ggplot2)

# data
###### combine together #######
set.seed(12)
spam1 = sample_random(4)
spam1$workerid = rep(1:4, each=n_image)

spam2 = sample_primary1(2)
spam2$workerid = rep(5:6, each=n_image)

spam3 = sample_primary2(2)
spam3$workerid = rep(7:8, each=n_image)

spam3 = sample_primary2(3)
spam3$workerid = rep(7:8, each=n_image)

spam4 = sample_strong_pattern1(2)
spam4$workerid = rep(9:10, each=n_image)
# 
spam5 = sample_strong_pattern2(2)
spam5$workerid = rep(11:12, each=n_image)

#spam6 = sample_weak_pattern1(2)
#spam6$workerid = rep(9:10, each=n_image)

#spam7 = sample_weak_pattern2(2)
#spam7$workerid = rep(11:12, each=n_image)

norm = sample_normal(108)
norm$workerid = rep(13:120, each=n_image)

data = rbind(spam1, spam2, spam3, spam4, spam5, norm)
dim(data)

p = glmer(as.factor(decision) ~ (1 | workerid ) + (1 | imageid),      
          data = data, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)


p = glmer(as.factor(decision) ~ (1 | workerid ) + (1 | imageid),      
          data = norm, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)

# Spammer Index
p.t = sum(data.frame(VarCorr(p))$vcov)
si = data.frame(VarCorr(p))$vcov[1] / p.t
si




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
var.wk <- c()
total.var <- c()
total.var.ratio <- c()
aii.change <- c()
p_value<- c()


for (x in unique(data$workerid)) {
  workerid <- append(workerid, x)
  p_temp <- exclude.influence(p, "workerid", x)
  loglik <- append(loglik, -2*(logLik(p)[1]- logLik(p_temp)[1]))
  
  var.wk  <- append(var.wk, data.frame(VarCorr(p_temp))$vcov[1])
  
  total.var <- append(total.var, sum(data.frame(VarCorr(p_temp))$vcov))
  
  aii.change <- append(aii.change, data.frame(VarCorr(p_temp))$vcov[1]/ sum(data.frame(VarCorr(p_temp))$vcov) - p.var.image )
  
  total.var.ratio <- append(total.var.ratio, sum(data.frame(VarCorr(p_temp))$vcov) / p.t)
  
  p_value <- append(p_value, 1-pchisq(-2*(logLik(p)[1]- logLik(p_temp)[1]), df = 80))
  
}


# length(workerid)
# length(loglik)
# length(total.var)
# length(var.wk)
# length(total.var.ratio)
# length(aii.change)


df = data.frame(workerid,loglik, p_value, total.var, var.wk, 
                total.var.ratio, aii.change)
df[df$p_value<=0.05,]




hist(df$loglik, prob = TRUE, main = 'Histogram of Deviance Distance', xlab='Deviance Distance')
#lines(x, dchisq(x, 80), col='red')
x = seq(min(df$loglik), max(df$loglik), by=1)
d = dchisq(x, 80)
lines(x, d, col = 'red')
legend("topright", legend = 'Chi-Squre Dist with DF = 80', fill = 'red')




ggplot(df) +
  geom_point(aes(x = 1:nrow(df), y = loglik)) +
  geom_segment(aes(x = 1:nrow(df), xend = 1:nrow(df), y = 0, yend = loglik),
               color = 'cornflowerblue') +
  geom_hline(yintercept = qchisq(p=.95, df=80), color = 'salmon') +
  labs(x = 'Index of Worker ID', y = 'Deviance Distance',
       title = 'Deviance Distance of Worker i Removed',
       subtitle = 'Cutoff (p-value) based on a Chi-Sqaure test with a significance level of 0.05 and df = 80') +
  geom_text(aes(x = 1:nrow(df)), y = df$loglik,
            label = ifelse(df$p_value <=0.05,
                           df$workerid, ''), vjust = -1, size = 3)
                           



