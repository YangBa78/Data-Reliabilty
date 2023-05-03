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
          data = newdtC2, family = binomial)
p
p = glmer(as.factor(decision) ~ (1 | workerid) + (1 | imageid) + (1 | workerid : task),   
          data = dt, family = binomial)

p = glmer(as.factor(decision) ~ (1 | workerid) + (1 | imageid) + (1|workerid:imageid),   
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

for (x in unique(dt$workerid)) {
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
  p_value <- append(p_value, pchisq(-2*(logLik(p)[1]- logLik(p_temp)[1]), df=80, lower.tail=F))
  #accuary <- append(accuary, 1- sum(simul[simul$workerid==x,]$correct)/30)
  accuary  <- append(accuary , sum(dt[dt$workerid==x,]$acc)/80)
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
length(accuary) 
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
                  total.var.change, var.image.change, p_value, accuary)

#names(Data.p) <- c('imageid', 'loglik', 'var.image', 'varint.image', 'total.var','var.image.change', 'varint.image.change', 'total.var.change', 'vartrace', 'vartrace2', 'vartrace3', 'disagreerate')



# concatenate
dfl = df.p[, c('workerid', 'loglik', 'vartrace', 'total.var.change', 'p_value', 'accuary')]%>% left_join(dtWorker, by = "workerid")
tail(dfl)
head(dfl)
range(dfl$vartrace)
range(dfl$total.var.change)
# 84, 90, 108, 146, 147, 
#17,33, 63 

dfl$zscore = (dfl$loglik - mean(dfl$loglik)) / sd(dfl$loglik)


newdtC2$convTruth = ifelse(newdtC2$trueLable=='same', 0, 1)
newdtC2$correct = ifelse(newdtC2$decision ==newdtC2$convTruth, 0, 1)

ggplot(dfl) +
  geom_point(aes(x = 1:nrow(dfl), y = loglik)) +
  geom_segment(aes(x = 1:nrow(dfl), xend = 1:nrow(dfl), y = 0, yend = loglik),
               color = 'cornflowerblue') +
  geom_hline(yintercept = qchisq(p = .05, df = 80, lower.tail = FALSE), color = 'salmon') +
  labs(x = 'Index of Worker ID', y = 'Deviance Difference Worker i Deleted',
       title = 'Deviance Difference ',
       subtitle = 'Thresholds are Chi Square Test with DF = No. Task at 0.05 significance level') +
  geom_text(aes(x = 1:nrow(dfl)), y = loglik,
            label = ifelse(loglik > qchisq(p = .05, df = 80, lower.tail = FALSE),
                           paste0(round(loglik, digits = 1), ' (', dfl$workerid, ')'), ''))

# z score
ggplot(dfl) +
  geom_point(aes(x = 1:nrow(dfl), y = zscore)) +
  geom_segment(aes(x = 1:nrow(dfl), xend = 1:nrow(dfl), y = 0, yend = zscore),
               color = 'cornflowerblue') +
  geom_hline(yintercept = c(-2, 2), color = 'salmon') +
  labs(x = 'Index of Worker ID', y = 'Deviance Difference Worker i Deleted',
       title = 'Deviance Difference ',
       subtitle = 'Thresholds are Chi Square Test with DF = No. Task at 0.05 significance level') +
  geom_text(aes(x = 1:nrow(dfl)), y = dfl$zscore,
            label = ifelse(abs(dfl$zscore) > 1.96,
                           paste0(round(dfl$zscore, digits = 2), ' (', dfl$workerid, ')'), ''))

dfl$zs = (dfl$vartrace - mean(dfl$vartrace)) / sd(dfl$vartrace)
 


ggplot(dfl) +
  geom_point(aes(x = 1:nrow(dfl), y = zs)) +
  geom_segment(aes(x = 1:nrow(dfl), xend = 1:nrow(dfl), y = 0, yend = zs),
               color = 'cornflowerblue') +
  geom_hline(yintercept = c(-2, 2), color = 'salmon') +
  labs(x = 'Index of Worker ID', y = 'Difference in Trace for Variance Matrix(Total Variance Changes) Worker i Deleted',
       title = 'Variance Change') +
  geom_text(aes(x = 1:nrow(dfl)), y = dfl$zs,
            label = ifelse(abs(dfl$zs) > 1.96,
                           paste0(round(dfl$zs, digits = 2), ' (', dfl$workerid, ')'), ''))


dfl$accuary = dfl$accuary/80

dfl$rankl = rank(dfl$loglik)
dfl$rankabtr = rank(dfl$vartrace)
dfl$ranktr = rank(abs(dfl$vartrace))
dfl$ranktrratio = rank(dfl$total.var.change)

s = c(87, 98, 127, 3, 20, 42, 64, 137, 156)
dfl[dfl$workerid%in% s,]


tail(dfl[order(dfl$loglik),])
head(dfl[order(dfl$loglik),])
tail(dfl[order(dfl$rankabtr),])

# only ID 1-160

df2 = dfl[dfl$workerid<=160, ]
df2$zs = (df2$loglik - mean(df2$loglik)) / sd(df2$loglik)

df2[abs(df2$zs)>2, ]

# 
ggplot(df2) +
  geom_point(aes(x = 1:nrow(df2), y = zs)) +
  geom_segment(aes(x = 1:nrow(df2), xend = 1:nrow(df2), y = 0, yend = zs),
               color = 'cornflowerblue') +
  geom_hline(yintercept = c(-2, 2), color = 'salmon') +
  labs(x = 'Index of Worker ID', y = 'Deviance Difference Worker i Deleted',
       title = 'Deviance Difference ',
       subtitle = 'Thresholds are Chi Square Test with DF = No. Task at 0.05 significance level') +
  geom_text(aes(x = 1:nrow(df2)), y = df2$zs,
            label = ifelse(abs(df2$zs) > 2,
                           paste0(round(df2$zs, digits = 2), ' (', df2$workerid, ')'), ''))

dfl[dfl$workerid==c(30, 31), ]


for (x in c(17, 30, 33, 43, 101, 113)){
  print(x)
  print(df2[df2$workerid==x, ])
}

# why 30, 113

table(newdtC2[newdtC2$workerid==33,]$trueLable, newdtC2[newdtC2$workerid==113,]$decision)
table(newdtC2[newdtC2$workerid==113,]$trueLable, newdtC2[newdtC2$workerid==113,]$decision)
table(newdtC2[newdtC2$workerid==113,]$trueLable, newdtC2[newdtC2$workerid==113,]$decision)
table(newdtC2[newdtC2$workerid==17,]$trueLable, newdtC2[newdtC2$workerid==17,]$decision)
table(newdtC2[newdtC2$workerid==17,]$trueLable, newdtC2[newdtC2$workerid==17,]$decision)

min(df2$accuary)



df2[df2$accuary == min(df2$accuary), ]

df2$ranktr = rank(df2$vartrace)
df2$ranktrratio = rank(df2$total.var.change)

df2$zs2 = (df2$vartrace - mean(df2$vartrace)) / sd(df2$vartrace)


df2[df2$zs2>2, ]
df2[df2$workerid==113, ]


# trace
ggplot(df2) +
  geom_point(aes(x = 1:nrow(df2), y = zs2)) +
  geom_segment(aes(x = 1:nrow(df2), xend = 1:nrow(df2), y = 0, yend = zs2),
               color = 'cornflowerblue') +
  geom_hline(yintercept = c(-2, 2), color = 'salmon') +
  labs(x = 'Index of Worker ID', y = 'Difference in Trace for Variance Matrix(Total Variance Changes) Worker i Deleted',
       title = 'Variance Change') +
  geom_text(aes(x = 1:nrow(df2)), y = df2$zs2,
            label = ifelse(abs(df2$zs2) > 2,
                           paste0(round(df2$zs2, digits = 2), ' (', df2$workerid, ')'), ''))


tail(df2[order(vartrace),], 5)

for (i in unique(dt$workerid)){
  #1- sum(dt[dt$workerid==i,]$correct)
  df.1[df.1$workerid==i,]$acc = 1- sum(dt[dt$workerid==i,]$correct)/80
}

df2[df2$workerid==63, ]

df.1$acc = NA
head(df.1[order(df.1$acc),])
tail(df.1[order(df.1$acc),])dfl

dfl[dfl$workerid==c(161, 162, 163, 164), ]


head(dfl[order(loglik),])
tail(dfl[order(loglik),])
tail(dfl[order(abs(vartrace)),], 20)
tail(dfl[order(total.var.change),])


#spamer1 <- c(53, 52, 51, 23, 28, 1, 30, 13, 16, 31, 18, 33, 11, 45)
spamer1 <- c(53, 52, 51, 12, 36, 22, 41)
aii <- c()
vartrace <- c()
workerid <-c()
aii
vartrace
p <-c(p)

for (x in spamer1){
  workerid <- append(workerid, x)
  p_temp <- exclude.influence(p, "workerid", x)
  #loglik <- append(loglik, -2*(logLik(p)[1]- logLik(p_temp)[1]))
  vartrace <- append(vartrace,  trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp)))))
  aii <- append(aii, data.frame(VarCorr(p_temp))$vcov[2] / sum(data.frame(VarCorr(p_temp))$vcov))
  data <- newdtC1[!newdtC1$workerid==x,]
  p <- glmer(as.factor(decision) ~ (1 | workerid) + (1 | imageid),   
            data = data, family = binomial)
}



plt <- data.frame(workerid, aii)
plt
#plot(as.factor(plt$workerid), plt$aii, type = 'l')

#plot(as.factor(workerid), aii, type = 'l')
#workerid

library(ggplot2)
ggplot(plt) + 
  geom_bar(stat="identity", aes(x=as.factor(workerid), y=aii)) +
  scale_x_discrete(limits = as.factor(workerid))

workerid <- append(workerid, spamer1[x])
p_temp <- exclude.influence(p, "workerid", spamer1[x])
aii <- append(aii, data.frame(VarCorr(p_temp))$vcov[2] / sum(data.frame(VarCorr(p_temp))$vcov))


newdtC1[!newdtC1$workerid==1,]

spamer1[1]
for(x in 1:length(spamer1)){
  print(x)
}




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


for (x in unique(dt$imageid)) {
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
