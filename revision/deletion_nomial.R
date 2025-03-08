library(dplyr)
library(ggplot2)
library(mclogit)


spam1 = sample_random(4)
spam1$workerid = rep(1:4, each=n_image)

spam2 = sample_primary1(1)
spam2$workerid = rep(5, each=n_image)

spam3 = sample_primary2(1)
spam3$workerid = rep(6, each=n_image)

spam4 = sample_primary3(1)
spam4$workerid = rep(7, each=n_image)

spam5 = sample_primary4(1)
spam5$workerid = rep(8, each=n_image)


spam6 = sample_primary5(1)
spam6$workerid = rep(9, each=n_image)


spam7 = sample_strong_pattern(4)
spam7$workerid = rep(10:13, each=n_image)#

norm = sample_normal(108)
norm$workerid = rep(14:121, each=n_image)

data = rbind(spam1, spam2, spam3, spam4, spam5, spam6, spam7, norm)
dim(data)


head(data)



######################### deletion analysis
model_freq <- mblogit(
  decision ~ 1,
  random = list(~1|imageid, ~1|workerid),  
  data = data,
  estimator = "REML"  
)


var_image = sum(diag(model_freq$VarCov$imageid))

var_worker = sum(diag(model_freq$VarCov$workerid))

si = var_worker / (var_worker + var_image)


ddd = model_freq$deviance

workerid <- c()
dev <- c()
p_value <- c()

for (x in 16:121) {
  print(x)
  workerid <- append(workerid, x)
  data_temp = data[workerid!=x]
  
  model_temp<- mblogit(
    decision ~ 1,
    random = list(~1|imageid, ~1|workerid),  # Crossed random effects
    data = data_temp,
    estimator = "REML"  # Restricted Maximum Likelihood
  )
  
  d_temp = model_temp$deviance
  dev <- append(dev, ddd-d_temp)
  
  p_value <- append(p_value, 1-pchisq(ddd-d_temp, df = 80))
  
}

df = data.frame(workerid, dev, p_value)
df[df$p_value<=0.05,]

colnames(df) = c("workerid", "loglik",  "p_value" )


########################### visuals 
ggplot(df) +
  geom_point(aes(x = 1:nrow(df), y = loglik)) +
  geom_segment(aes(x = 1:nrow(df), xend = 1:nrow(df), y = 0, yend = loglik),
               color = 'cornflowerblue') +
  geom_hline(yintercept = qchisq(p=.95, df=80), color = 'salmon') +
  labs(x = 'Worker ID', y = 'Deviance Distance',
       title = 'Deviance Distance with Worker i Removed (Nominal Categories: A, B, C, D, E)',
       subtitle = 'Cutoff (p-value) based on a Likelihood Ratio test (Chi-squared) with a significance level of 0.05 and df = 80') +
  geom_text(aes(x = 1:nrow(df)), y = df$loglik,
            label = ifelse(df$p_value <=0.05,
                           df$workerid, ''), vjust = -1, size = 3)




###############################  mc



