library(lme4)
library(readxl)
library(influence.ME)
library(corrplot)
library(ggplot2)
library(ggpmisc)
require(Matrix)
library(dplyr)
library(brms)


df = read.csv("~/Dropbox (ASU)/Facewise/DF_Facewise_Airports1.csv")
View(df)

p = glmer(as.factor(Final.Decision) ~ (1 | ID ) + (1 | Pair) + (1 | ID:Pair),      
          data = df, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)


p3 = glmer(as.factor(Final.Decision) ~ Time.Spend + (1 + Time.Spend | ID ) + (1 + Time.Spend | Pair),      
          data = df, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p3)

p = glmer(as.factor(Final.Decision) ~ (1 | ID ) + (1 | Pair),      
          data = df, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)

p2 = glmer(as.factor(Final.Decision) ~ (1 | Pair),      
           data = df, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p2)


p4 = lmer(Time.Spend ~  (1 | ID ) + (1 | Pair),      
           data = df)
summary(p4)



df
#####################
#acc = df %>% group_by(ID) %>% summarise(accuracy = sum(correct)/(nimage), 
#                                              .groups = 'drop') %>%as.data.frame()


wid = data.frame(ranef(p)$ID)
wid <- cbind(newColName = rownames(wid), wid)
rownames(wid) <- 1:nrow(wid)
colnames(wid) = c('ID', 'worker_eff')

wid$ID = as.integer(wid$ID)
df = df%>% left_join(wid, by='ID')


iid = data.frame(ranef(p)$Pair)
iid <- cbind(newColName = rownames(iid), iid)
rownames(iid) <- 1:nrow(iid)
colnames(iid) = c('Pair', 'image_eff')

iid$Pair = as.integer(iid$Pair)
df = df%>% left_join(iid, by='Pair')


df$re = df$image_eff + df$worker_eff
df$p = 1/(1+(exp(-df$re)))

df$ratio = df$image_eff / df$re
df$p_1 = 1/(1+(exp(-df$image_eff)))



plot(df$image_eff, df$p)

cor(abs(df$ratio), df$p)

plot(df[abs(df$ratio)<20,]$ratio, df[abs(df$ratio)<20,]$p)




ggplot(df[df$ID==119,], aes(image_eff)) +
  geom_line(aes(y=p), colour="black") 
#+ geom_line(aes(y=correct), colour="red")


df[df$ID==234,]$p




sum(ifelse(abs(df[df$ID==119,]$re)<0.3, 1, 0))/dim(df[df$ID==119,])[1]

sum(ifelse(abs(df[df$ID==239,]$re)<0.3, 1, 0))/dim(df[df$ID==239,])[1]
sum(ifelse(abs(df[df$ID==234,]$re)<0.3, 1, 0))/dim(df[df$ID==234,])[1]
sum(ifelse(abs(df[df$ID==243,]$re)<0.3, 1, 0))/dim(df[df$ID==243,])[1]


plot(df[df$ID==119,]$correct/dim(df[df$ID==119,])[1])
plot(df[df$ID==243,]$correct/dim(df[df$ID==243,])[1])



brmm1 <- brm( as.factor(Final.Decision) ~ 1 + (1 | ID ) + (1 | Pair), 
                              data   = df, 
                              warmup = 1000, 
                              iter   = 2000, 
                              family = bernoulli,
                              chains = 4, 
                              cores  = 2)  #the 


brmm2 <- brm( as.factor(Final.Decision)~ 1 +  (1 | Pair), 
              data   = df, 
              warmup = 1000, 
              iter   = 2000, 
              family = bernoulli,
              chains = 4, 
              cores  = 2)  #the 

summary(brmm1)
summary(p1)

logLik(brmm1)

ranef(brmm1)
View(ranef(p1)$Pair)


true = data.frame(ranef(p1)$Pair)
true <- cbind(newColName = rownames(true), true)
rownames(true) <- 1:nrow(true)
colnames(true) = c('Pair', 'RT')
true$Pair = as.integer(true$Pair)


true = true %>% left_join(df[,c('Pair', 'True.Label')][!duplicated(df[,c('Pair', 'True.Label')]$Pair), ], by='Pair')

df[,c('Pair', 'True.Label')][!duplicated(df[,c('Pair', 'True.Label')]$Pair), ]


true$conT = ifelse(true$True.Label=='Same', 1, 0)
true$t1 = ifelse(true$RT>0, 1, 0) # 1: same
true$t2 = ifelse(true$RT>0, 0, 1)

true$com1 = ifelse(true$t1==true$conT, 1, 0)
true$com2 = ifelse(true$t2==true$conT, 1, 0)

sum(true$com1)
sum(true$com2)


df$con = ifelse(df$Final.Decision=='Same', 1, 0)
#mv = 


View(df %>% group_by(Pair) %>% summarise(mv = sum(con)/80))

a = c()
for (i in 1:80){
  a[i] = sum(df[df$Pair==i,]$con) / dim(df[df$Pair==i,])[1]
}

true$mv = a

#119 -0.392932756 -9.426633e-04
#234 -1.156392419  1.613820e-02
#248 -0.637909550  1.944775e-02
#355  2.474444064 -3.776910e-02

# 0.0003718
# 0.0161382
# 0.01944775
# -0.0377691
# -0.0009426633






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

for (x in unique(df$ID)) {
  workerid <- append(workerid, x)
  p_temp <- exclude.influence(p, "ID", x)
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


df.p = data.frame(workerid,loglik, total.var, var.image, vartrace, vartrace2, vartrace3,
                  total.var.change, var.image.change, p_value)

#names(Data.p) <- c('imageid', 'loglik', 'var.image', 'varint.image', 'total.var','var.image.change', 'varint.image.change', 'total.var.change', 'vartrace', 'vartrace2', 'vartrace3', 'disagreerate')

df.p

tail(df.p[order(df.p$z),])

# concatenate
dfl = df.p[, c('workerid', 'loglik', 'vartrace', 'total.var.change', 'p_value')]%>% left_join(dtWorker, by = "workerid")
dfl


# 3.2
# Plot
rownames(df.1) <- df.1$workerid

# null: 
# Alternative: 
library(ggplot2)
ggplot(df.1) +
  geom_point(aes(x = 1:nrow(df.1), y = loglik)) +
  geom_segment(aes(x = 1:nrow(df.1), xend = 1:nrow(df.1), y = 0, yend = loglik),
               color = 'cornflowerblue') +
  geom_hline(yintercept = qchisq(p = .05, df = 80, lower.tail = FALSE), color = 'salmon') +
  labs(x = 'Index of Worker ID', y = 'Deviance Difference Worker i Deleted',
      title = 'Deviance Difference ',
      subtitle = 'Thresholds are Chi Square Test with DF = No. Task at 0.05 significance level') +
  geom_text(aes(x = 1:nrow(df.1)), y = loglik,
                label = ifelse(loglik > qchisq(p = .05, df = 80, lower.tail = FALSE),
                               paste0(round(loglik, digits = 1), ' (', df.1$workerid, ')'), ''))


df[df$ID==119, c('hu_Accuracy', 'Accu_difficult', 'Accu_easy')][80, ]
df[df$ID==234, c('hu_Accuracy', 'Accu_difficult', 'Accu_easy')][80, ]
df[df$ID==248, c('hu_Accuracy', 'Accu_difficult', 'Accu_easy')][80, ]
df[df$ID==251, c('hu_Accuracy', 'Accu_difficult', 'Accu_easy')][80, ]




# 2.23
df.p$rankL = rank(df.p$loglik)

df.1 = df.p[, c('workerid', 'loglik', 'vartrace', 'total.var.change', 'p_value')]
#df.1 = df.p[, c('workerid', 'loglik', 'vartrace', 'total.var.change', 'rankL', 'ranktr', 'rankabtr', 'ranktratio')]
df.1$zscore = (df.1$loglik - mean(df.1$loglik)) / sd(df.1$loglik) 

head(df.1[order(loglik),])


df.1  =df.1%>%left_join(dtWorker, by = "workerid")

dt$convTruth = ifelse(dt$trueLable=='same', 0, 1)
dt$correct = ifelse(dt$decision ==dt$convTruth, 0, 1)

for (i in unique(dt$workerid)){
  #1- sum(dt[dt$workerid==i,]$correct)
  df.1[df.1$workerid==i,]$acc = 1- sum(dt[dt$workerid==i,]$correct)/80
}


df.1$acc = NA
head(df.1[order(df.1$acc),])
tail(df.1[order(df.1$acc),])

head(df.1[order(loglik),])
tail(df.1[order(loglik),])
head(df.1[order(vartrace),], 20)
tail(df.1[order(vartrace),])
head(df.1[order(abs(vartrace)),])
tail(df.1[order(abs(vartrace)),])
tail(df.1[order(total.var.change),])
head(df.1[order(total.var.change),])

df.1[df.1$workerid==119,]
df.1[df.1$workerid==234,]
df.1[df.1$workerid==355,]
df.1[df.1$workerid==347,]
dim(df.p)


id = c(107,
  119,
  125,
  126,
  128,
  133,
  140,
  201,
  202,
  217,
  234,
  235,
  245,
  248,
  251,
  303,
  307,
  308,
  309,
  321,
  332,
  333,
  353,
  355,
  357,
  373,
  381)

for (i in id){
  print(i)
  print(df.1[df.1$workerid==i,])
}


id.a[order(id.a$ranefID),]
colnames(id.a)
id.a
df.1
id.a = data.frame(row.names(ranef(p)$ID), ranef(p)$ID)
colnames(id.a) <- c('workerid', 'ranefID')


id.a$workerid <- as.numeric(id.a$workerid)
df.id <- df.1 %>% left_join(id.a, by = "workerid")

df.id

head(df.id[order(loglik),])
tail(df.id[order(loglik),])
tail(df.id[order(vartrace),])
head(df.id[order(vartrace),])
head(df.id[order(abs(vartrace)),])
tail(df.id[order(abs(vartrace)),])
tail(df.id[order(total.var.change),])
head(df.id[order(total.var.change),])

df.id[df.1$workerid==119,]
df.id[df.1$workerid==234,]
df.id[df.1$workerid==355,]
df.id[df.1$workerid==349,]
df.id[df.1$workerid==321,]


dfl[order(loglik),]
dfl[order(abs(vartrace)),]
dfl[order(total.var.change),]







###########
id = data.frame(ranef(p4)$ID)
id = cbind(newColName = rownames(id), id)
rownames(id) <- 1:nrow(id)
colnames(id) = c('ID', 'worker_eff')
id
id$ID = as.integer(id$ID)


pair = data.frame(ranef(p4)$Pair)
pair = cbind(newColName = rownames(pair), pair)
rownames(pair) <- 1:nrow(pair)
colnames(pair) = c('Pair', 'image_eff')
pair$Pair = as.integer(pair$Pair)

df = df %>% left_join(id, by='ID')
df = df %>% left_join(pair, by='Pair')
df$re = df$worker_eff + df$image_eff
df$p = 1/(1+(exp(-df$re)))


t1 = df[df$ID==355, ][, c('ID', 'Pair', 'image_eff', 'p')]
t2 = df[df$ID==373, ][, c('ID', 'Pair', 'image_eff', 'p')]
t3 = df[df$ID==234, ][, c('ID', 'Pair', 'image_eff', 'p')]
t4 = df[df$ID==248, ][, c('ID', 'Pair', 'image_eff', 'p')]
t5 = df[df$ID==243, ][, c('ID', 'Pair', 'image_eff', 'p')]
t6 = df[df$ID==309, ][, c('ID', 'Pair', 'image_eff', 'p')]
colnames(t2)[4] = 'p2'
colnames(t3)[4] = 'p3'
colnames(t4)[4] = 'p4'
colnames(t5)[4] = 'p5'
colnames(t6)[4] = 'p6'

t = t1 %>% left_join(t2, by='Pair')
t = t %>% left_join(t3, by='Pair')
t = t %>% left_join(t4, by='Pair')
t = t %>% left_join(t5, by='Pair')
t = t %>% left_join(t6, by='Pair')
t$p_i = 1/(1+(exp(-t$image_eff.x)))



ggplot(t, aes(image_eff.x)) +
  geom_line(aes(y=p_i), colour="black") + # accuracy 100% 
  geom_line(aes(y=p), colour="red") +  # 0.6625
  geom_line(aes(y=p2), colour="green") + # 
  geom_line(aes(y=p3), colour="blue") + # 0.566
  geom_line(aes(y=p4), colour="yellow") + # 0.633
  geom_line(aes(y=p5), colour="brown") + # 0.6667
  geom_line(aes(y=p6), colour="orange") # 0.6667
  #geom_vline(xintercept = -0.9413365, linetype="dashed") + 
  #geom_vline(xintercept = -0.2652010, linetype="dashed") + 
  #geom_vline(xintercept = 0.4848510, linetype="dashed") 


pair
