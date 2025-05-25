library(readxl)
library(lme4)
library(influence.ME)
require(Matrix)
library(dplyr)
library(ggplot2)
library(stringr)


mt <- read.csv("Data/MTurkPerformanceData.csv")

colnames(mt)

mt = mt[,c('Assignment.ID','Completion.Time','Final.Decision', 'Coded.Prediction', 'Task.difficulty', 'TID')]
colnames(mt)[1] = 'workerid'
colnames(mt)[2] = 'time'
colnames(mt)[3] = 'decision'
colnames(mt)[4] = 'accuracy'
colnames(mt)[5] = 'task'

colnames(mt)[6] = 'image'
#mt


p = glmer(as.factor(decision) ~ (1 | workerid ) + (1 | image) + (1 | workerid : image),      
          data = mt, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)


# Spammer Index
p.t = sum(data.frame(VarCorr(p))$vcov)
si = data.frame(VarCorr(p))$vcov[2] / p.t
si # 0.1663119



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

p.var.image = data.frame(VarCorr(p))$vcov[2] / p.t


workerid <- c()
loglik <- c()
var.wk <- c()
total.var <- c()
total.var.ratio <- c()
aii.change <- c()
pvalue <- c()


for (x in unique(mt$workerid)) {
  workerid <- append(workerid, x)
  p_temp <- exclude.influence(p, "workerid", x)
  loglik <- append(loglik, -2*(logLik(p)[1]- logLik(p_temp)[1]))
  
  var.wk  <- append(var.wk, data.frame(VarCorr(p_temp))$vcov[2])
  
  total.var <- append(total.var, sum(data.frame(VarCorr(p_temp))$vcov))
  
  aii.change <- append(aii.change, data.frame(VarCorr(p_temp))$vcov[2]/ sum(data.frame(VarCorr(p_temp))$vcov) - p.var.image )
  
  total.var.ratio <- append(total.var.ratio, sum(data.frame(VarCorr(p_temp))$vcov) / p.t)
  
  pvalue <- append(pvalue, 1-pchisq(-2*(logLik(p)[1]- logLik(p_temp)[1]), df = 77))
}



df = data.frame(workerid,loglik, pvalue, total.var, var.wk, 
                total.var.ratio, aii.change)

View(df)

df[df$pvalue<=0.05,]$workerid




# ggplot(df) +
#   geom_point(aes(x = 1:nrow(df), y = loglik)) +
#   geom_segment(aes(x = 1:nrow(df), xend = 1:nrow(df), y = 0, yend = loglik),
#                color = 'cornflowerblue') +
#   geom_hline(yintercept = qchisq(p=.95, df=77), color = 'salmon') +
#   labs(x = 'Index of Worker ID', y = 'Deviance Distance',
#        title = 'Deviance Distance of Worker i Deleted',
#        subtitle = 'Thresholds are Chi-Sqaure Distribution DF = 77 at 0.05 significance level') +
#   geom_text(aes(x = 1:nrow(df)), y = df$loglik,
#             label = ifelse(df$pvalue <=0.05,
#                            paste0(round(df$loglik, digits = 0), ' (',  str_extract(df$workerid, "^.{5}"), ')'), ''))




ggplot(df) +
  geom_point(aes(x = 1:nrow(df), y = loglik)) +
  geom_segment(aes(x = 1:nrow(df), xend = 1:nrow(df), y = 0, yend = loglik),
               color = 'cornflowerblue') +
  geom_hline(yintercept = qchisq(p=.95, df=77), color = 'salmon') +
  labs(x = 'Index of Worker ID', y = 'Deviance Distance',
       title = 'Deviance Distance of Worker i Deleted',
       subtitle = 'Thresholds are Chi-Sqaure Distribution DF = 77 at 0.05 significance level') +
  geom_text(aes(x = 1:nrow(df)), y = df$loglik,
            label = ifelse(df$pvalue <=0.05,
                           paste0(round(df$loglik, digits = 0)), ''))



