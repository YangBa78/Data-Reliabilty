library(readxl)
library(lme4)
library(influence.ME)
require(Matrix)
library(dplyr)
library(ggplot2)
library(writexl)


pl <- read_excel("Data/Prolific.xlsx")
pl = pl[pl['WorkerId']!='2f33f23f23',]

colnames(pl)

pl = pl[,c('WorkerId','Time Spend','Final Decision', 'error', 'task', 'Image 1')]
colnames(pl)[1] = 'workerid'
colnames(pl)[2] = 'time'
colnames(pl)[3] = 'decision'
colnames(pl)[6] = 'image'



# Spammer Index
p = glmer(as.factor(decision) ~ (1 | workerid ) + (1 | image),      
                 data = pl, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)

p.t = sum(data.frame(VarCorr(p))$vcov)
si = data.frame(VarCorr(p))$vcov[1] / p.t
si  #Spammer Index 



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
pvalue <- c()


for (x in unique(pl$workerid)) {
  workerid <- append(workerid, x)
  p_temp <- exclude.influence(p, "workerid", x)
  loglik <- append(loglik, -2*(logLik(p)[1]- logLik(p_temp)[1]))
  
  var.wk  <- append(var.wk, data.frame(VarCorr(p_temp))$vcov[1])

  total.var <- append(total.var, sum(data.frame(VarCorr(p_temp))$vcov))
  
  aii.change <- append(aii.change, data.frame(VarCorr(p_temp))$vcov[1]/ sum(data.frame(VarCorr(p_temp))$vcov) - p.var.image )
  
  total.var.ratio <- append(total.var.ratio, sum(data.frame(VarCorr(p_temp))$vcov) / p.t)
  pvalue <- append(pvalue, 1-pchisq(-2*(logLik(p)[1]- logLik(p_temp)[1]), df = 72))
}


# length(workerid)
# length(loglik)
# length(total.var)
# length(var.wk)
# length(total.var.ratio)
# length(aii.change)
# length(pvalue)


df = data.frame(workerid,loglik, pvalue, total.var, var.wk, 
                  total.var.ratio, aii.change)


df[df$pvalue<=0.05,]


# df$rankll = rank(df$loglik)
# df$rank.total.var = rank(df$total.var)
# df$rank.var.w = rank(df$var.wk)
# df$rank.var.ratio = rank(df$total.var.ratio)
# df$rank.aii = rank(df$aii.change)
# 
# 
# 
# colnames(df)
# 
# df$zs.ll = (df$loglik - mean(df$loglik)) / sd(df$loglik)
# df$zs.v = (df$total.var - mean(df$total.var)) / sd(df$total.var)
# df$zs.vw = (df$var.wk - mean(df$var.wk)) / sd(df$var.wk)
# df$zs.vr = (df$total.var.ratio - mean(df$total.var.ratio)) / sd(df$total.var.ratio)
# df$zs.a = (df$aii.change - mean(df$aii.change)) / sd(df$aii.change)


# ggplot(df) +
#   geom_point(aes(x = 1:nrow(df), y = loglik)) +
#   geom_segment(aes(x = 1:nrow(df), xend = 1:nrow(df), y = 0, yend = loglik),
#                color = 'cornflowerblue') +
#   geom_hline(yintercept = qchisq(p=.95, df=72), color = 'salmon') +
#   labs(x = 'Index of Worker ID', y = 'Deviance Distance',
#        title = 'Deviance Distance of Worker i Deleted',
#        subtitle = 'Thresholds are Chi-Sqaure Distribution DF = 72 at 0.05 significance level') +
#   geom_text(aes(x = 1:nrow(df)), y = df$loglik,
#             label = ifelse(df$pvalue <=0.05,
#                            paste0(round(df$loglik, digits = 0), ' (',  str_extract(df$workerid, "^.{5}"), ')'), ''))



ggplot(df) +
  geom_point(aes(x = 1:nrow(df), y = loglik)) +
  geom_segment(aes(x = 1:nrow(df), xend = 1:nrow(df), y = 0, yend = loglik),
               color = 'cornflowerblue') +
  geom_hline(yintercept = qchisq(p=.95, df=72), color = 'salmon') +
  labs(x = 'Index of Worker ID', y = 'Deviance Distance',
       title = 'Deviance Distance of Worker i Deleted', 
       subtitle = 'Thresholds are Chi-Sqaure Distribution DF = 72 at 0.05 significance level') +
  geom_text(aes(x = 1:nrow(df)), y = df$loglik,
            label = ifelse(df$pvalue <=0.05,
                           df$workerid, ''),  vjust = -1, size = 3)


