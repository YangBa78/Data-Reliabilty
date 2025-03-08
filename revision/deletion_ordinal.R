library(dplyr)
library(ggplot2)
library(ordinal)

# data
###### combine together #######
set.seed(27)
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


#################deletion analysis 



final_ctrl <- clmm.control(
  method = "ucminf",
  maxIter = 1000,
  gradTol = 1e-6,
  # trace = 1,
  innerCtrl = "warnOnly"
)

final_model <- clmm(decision ~ scale(image_eff) + (1 | imageid) + (1 | workerid),
                    data = data,
                    control = final_ctrl)
ll = logLik(final_model)[1]


var_image = VarCorr(final_model)$imageid[1]
var_worker = VarCorr(final_model)$workerid[1]

si =  var_worker / (var_image + var_worker)
si


model_norm <- clmm(decision ~ scale(image_eff) + (1 | imageid) + (1 | workerid),
                    data = norm,
                    control = final_ctrl)
ll = logLik(final_model)[1]


var_image.1 = VarCorr(model_norm)$imageid[1]
var_worker.1 = VarCorr(model_norm)$workerid[1]

si.1 =  var_worker.1 / (var_image.1 + var_worker.1)
si.1



workerid <- c()
loglik <- c()
p_value <- c()


for (x in unique(data$workerid)){
  print(x)
  workerid <- append(workerid, x)
  data_temp = data[workerid!=x]
  
  model_temp<-clmm(decision ~ scale(image_eff) + (1 | imageid) + (1 | workerid),
                   data = data_temp,
                   control = final_ctrl)
  
  loglik <- append(loglik, -2*(ll- logLik(model_temp)[1]))
  
  p_value <- append(p_value, 1-pchisq(-2*(ll- logLik(model_temp)[1]), df = 80))
}


df = data.frame(workerid, loglik, p_value)
df[df$p_value<=0.05,]


###########################################Visuals ############################

ggplot(df) +
  geom_point(aes(x = 1:nrow(df), y = loglik)) +
  geom_segment(aes(x = 1:nrow(df), xend = 1:nrow(df), y = 0, yend = loglik),
               color = 'cornflowerblue') +
  geom_hline(yintercept = qchisq(p=.95, df=80), color = 'salmon') +
  labs(x = 'Worker ID', y = 'Deviance Distance',
       title = 'Deviance Distance with Worker i Removed (Ordinal Likert Scale 1-5)',
       subtitle = 'Cutoff (p-value) based on a Likelihood Ratio test (Chi-squared) with a significance level of 0.05 and df = 80') +
  geom_text(aes(x = 1:nrow(df)), y = df$loglik,
            label = ifelse(df$p_value <=0.05,
                           df$workerid, ''), vjust = -1, size = 3)


###################################### MC#####################################
library(markovchain)

fit<-markovchainFit(data[data$workerid==2,]$decision)
round(fit$estimate@transitionMatrix, 2)

fit<-markovchainFit(data[data$workerid==7,]$decision)
round(fit$estimate@transitionMatrix, 2)

fit<-markovchainFit(data[data$workerid==13,]$decision)
round(fit$estimate@transitionMatrix, 2)


fit<-markovchainFit(data[data$workerid==100,]$decision)
round(fit$estimate@transitionMatrix, 2)














