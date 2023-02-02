require(lme4)
#library(arm)
#library(nlme)
#library(numDeriv)
library(readxl)
library(influence.ME)
library(corrplot)
library(ggplot2)
library(ggpmisc)
require(Matrix)
library(dplyr)
#install.packages('dplyr')


# REM
p = glmer(as.factor(decision) ~ (1 | workerid) + (1 | imageid),   
          data = newdtC, family = binomial)

p = glmer(as.factor(mod_decision) ~ (1 | workerid) + (1 | imageid),   
          data = dt, family = binomial)


summary(p)

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


# trace
#p.t = trace(v.matrix)



# variance -covariance matrix 
p.vcov = VarCorr(p)
v.matrix = as.matrix(Matrix::bdiag(p.vcov))
rankMatrix(v.matrix )[1]  # rank of matrix


# another method for trace
p.t = sum(data.frame(VarCorr(p))$vcov)


#logLik(p)[1]
#loglik
#-2*(logLik(p)[1]- logLik(p_1)[1])
#-2*(logLik(p)[1]- logLik(p_)[1])
#min(table(Prolific[Prolific$imageid=='2395_1.jpg', ]$decision))/sum(table(Prolific[Prolific$imageid=='2395_1.jpg', ]$decision))
#min(table(Prolific[Prolific$imageid=='192_02_01_051_16.png', ]$decision))/sum(table(Prolific[Prolific$imageid=='192_02_01_051_16.png', ]$decision))

p.var.image = data.frame(VarCorr(p))$vcov[1] / p.t
#p.var.imageint = (data.frame(VarCorr(p))$vcov[1] + data.frame(VarCorr(p))$vcov[4]) /p.t


#p_ = exclude.influence(p, "imageid", '2395_1.jpg')
#summary(p)


#p.vcov = VarCorr(p)
#trace(as.matrix(Matrix::bdiag(VarCorr(p_))) - v.matrix) - rankMatrix(v.matrix )[1]


# 1, 4
# 4
# list 
workerid <- c()
loglik <- c()
total.var <- c()
var.image <- c()
varint.image <- c()
vartrace <- c()
vartrace2 <- c()
vartrace3 <- c()
total.var.change <- c()
var.image.change <- c()
varint.image.change <- c()
accuary <- c()
p_value <- c()



#p_t <- exclude.influence(p, "imageid", 1)
#n004201_0001_02.png

#-2*(logLik(p)[1]- logLik(p_t)[1])

for (x in unique(newdtC$workerid)) {
  workerid <- append(workerid, x)
  p_temp <- exclude.influence(p, "workerid", x)
  loglik <- append(loglik, -2*(logLik(p)[1]- logLik(p_temp)[1]))
  
  var.image  <- append(var.image, data.frame(VarCorr(p_temp))$vcov[1])
  #varint.image <- append(varint.image , data.frame(VarCorr(p_temp))$vcov[1] + data.frame(VarCorr(p_temp))$vcov[5] + data.frame(VarCorr(p_temp))$vcov[4])
  total.var <- append(total.var, sum(data.frame(VarCorr(p_temp))$vcov))
  
  var.image.change <- append(var.image.change, data.frame(VarCorr(p_temp))$vcov[1]/ sum(data.frame(VarCorr(p_temp))$vcov) - p.var.image )
  #varint.image.change <- append(varint.image.change, (data.frame(VarCorr(p_temp))$vcov[1] + data.frame(VarCorr(p_temp))$vcov[3] + data.frame(VarCorr(p_temp))$vcov[4]) /sum(data.frame(VarCorr(p_temp))$vcov) - p.var.imageint)
  
  total.var.change <- append(total.var.change, sum(data.frame(VarCorr(p_temp))$vcov) / p.t)
  
  vartrace <- append(vartrace,  trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp)))))
  vartrace2 <- append(vartrace2, trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp))))- rankMatrix(v.matrix)[1])
  vartrace3 <- append(vartrace3, abs(trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp))))- rankMatrix(v.matrix)[1]))
  #disagreerate <- append(disagreerate, min(table(Prolific[Prolific$imageid==x, ]$decision))/sum(table(Prolific[Prolific$imageid==x, ]$decision)))
  p_value <- append(p_value, pchisq(-2*(logLik(p)[1]- logLik(p_temp)[1]), df=66, lower.tail=F))
}


length(workerid)
length(loglik)
length(total.var)
length(var.image)
length(varint.image)
length(vartrace)
length(vartrace2)
length(vartrace3)
length(total.var.change)
length(var.image.change)
length(varint.image.change)
length(disagreerate) 
length(p_value) 


#df.p$trace = with(df.p, total.var - p.t)
#df.p$varimageratio = with(df.p, var.image/total.var - p.var.image )
#df.p$varimageintratio = with(df.p, varint.image/total.var - p.var.imageint)

# check length of vector
#col =  c(imageid, loglik, var.image, varint.image, total.var,var.image.change, varint.image.change, total.var.change, vartrace, vartrace2, vartrace3, disagreerate)



# df
#Data.p <- list(unlist(imageid),
#              unlist(loglik),
#             unlist(var.image),
#            unlist(varint.image),
#           unlist(total.var),
#          unlist(var.image.change),
#         unlist(varint.image.change),
#        unlist(total.var.change),
#       unlist(vartrace),
#      unlist(vartrace2),
#     unlist(vartrace3),
#unlist(change), 
#unlist(disagreerate)
#)

df.p = data.frame(workerid,loglik, total.var, var.image, vartrace, vartrace2, vartrace3,
                  total.var.change, var.image.change, p_value)

#names(Data.p) <- c('imageid', 'loglik', 'var.image', 'varint.image', 'total.var','var.image.change', 'varint.image.change', 'total.var.change', 'vartrace', 'vartrace2', 'vartrace3', 'disagreerate')


# concatenate
dfl = df.p[, c('workerid', 'loglik', 'vartrace', 'total.var.change')]%>% left_join(dtWorker, by = "workerid")
dfl


dfl[order(loglik),]
dfl[order(vartrace),]
dfl[order(total.var.change),]


cor(dfl$loglik, abs(dfl$worker_eff)) # -0.6561019.      # -0.2752111
cor(dfl$vartrace, abs(dfl$worker_eff))  # 0.6623779.    # 0.3852399
cor(dfl$total.var.change, abs(dfl$worker_eff))  #-0.6623779.   # -0.3852399




# image 
imageid <- c()
loglik <- c()
total.var <- c()
var.image <- c()
varint.image <- c()
vartrace <- c()
vartrace2 <- c()
vartrace3 <- c()
total.var.change <- c()
var.image.change <- c()
varint.image.change <- c()
disagreerate <- c()


for (x in unique(newdtC$imageid)) {
  imageid <- append(imageid, x)
  p_temp <- exclude.influence(p, "imageid", x)
  loglik <- append(loglik, -2*(logLik(p)[1]- logLik(p_temp)[1]))
  
  var.image  <- append(var.image, data.frame(VarCorr(p_temp))$vcov[3])
  #varint.image <- append(varint.image , data.frame(VarCorr(p_temp))$vcov[1] + data.frame(VarCorr(p_temp))$vcov[5] + data.frame(VarCorr(p_temp))$vcov[4])
  total.var <- append(total.var, sum(data.frame(VarCorr(p_temp))$vcov))
  
  var.image.change <- append(var.image.change, data.frame(VarCorr(p_temp))$vcov[3]/ sum(data.frame(VarCorr(p_temp))$vcov) - p.var.image )
  #varint.image.change <- append(varint.image.change, (data.frame(VarCorr(p_temp))$vcov[1] + data.frame(VarCorr(p_temp))$vcov[3] + data.frame(VarCorr(p_temp))$vcov[4]) /sum(data.frame(VarCorr(p_temp))$vcov) - p.var.imageint)
  
  total.var.change <- append(total.var.change, sum(data.frame(VarCorr(p_temp))$vcov) / p.t)
  
  vartrace <- append(vartrace,  trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp)))))
  vartrace2 <- append(vartrace2, trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp))))- rankMatrix(v.matrix)[1])
  vartrace3 <- append(vartrace3, abs(trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp))))- rankMatrix(v.matrix)[1]))
  disagreerate <- append(disagreerate, min(table(dt[dt$imageid==x, ]$decision))/sum(table(dt[dt$imageid==x, ]$decision)))
}


length(imageid)
length(loglik)
length(total.var)
length(var.image)
length(varint.image)
length(vartrace)
length(vartrace2)
length(vartrace3)
length(total.var.change)
length(var.image.change)
length(varint.image.change)
length(disagreerate) 



df.i = data.frame(imageid,loglik, total.var, var.image, vartrace, vartrace2, vartrace3,
                  total.var.change, var.image.change, disagreerate)

df.i$disagreerate <- ifelse(df.i$disagreerate ==1, 0, df.i$disagreerate)


cor(df.i$loglik, df.i$disagreerate)  # 0.4203562.      # 0.8705909
cor(df.i$vartrace, df.i$disagreerate) # -0.4889774.   # -0.7145904
cor(df.i$total.var.change, df.i$disagreerate) # 0.4889774.  # 0.7145904

dfi = df.i[, c('imageid', 'loglik', 'vartrace', 'total.var.change')]%>% left_join(dtImage, by = "imageid")
dfi

cor(dfi$loglik, abs(dfi$image_eff)) # -0.3665448.   # -0.8620711
cor(dfi$vartrace, abs(dfi$image_eff))  # 0.4603978.  # 0.7760047
cor(dfi$total.var.change, abs(dfi$image_eff))  #-0.4603978. # -0.7760047