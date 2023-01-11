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

options(scipen=999)

simulated_df = simul
colnames(simulated_df)

#read data -prolific
Prolific <- read_excel("Dropbox (ASU)/Code/8.3/Prolific.xlsx")
#colnames(simulated_df)[2] <- 'workerid'
colnames(simulated_df)[2] <- 'imageid'
#colnames(simulated_df)[1] <- 'workerid'
#colnames(simulated_df)[2] <- 'imageid'
colnames(simulated_df)[1] <- 'workerid'
colnames(Prolific)[2] <- 'workerid'
colnames(Prolific)[8] <- 'imageid'
colnames(Prolific)[18] <- 'decision'
#simulated_df$decision <- ifelse(simulated_df$decision=='different', 1, 0)

colnames(norm_df)[1] <- 'imageid'
norm_df$decision <- ifelse(norm_df$decision=='different', 1, 0)

colnames(spam_df)[1] <- 'imageid'
spam_df$decision <- ifelse(spam_df$decision=='different', 1, 0)

Prolific$decision <- ifelse(Prolific$decision=='different', 1, 0)


# REM
p = glmer(cbind(y, num_task - y) ~ (1 | worker),     
          data = simulated_df, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)

p1 = glmer(decision ~ (1 | workerid) + (1 | task/imageid) + (1 | imageid:workerid) + (1 | task:workerid),     
           data = norm_df, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p1)

p= glmer(decision ~ (1 | workerid) + (1 | task/imageid) + (1 | imageid:workerid) + (1 | task:workerid),     
         data = simulated_df, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)

p = glmer(decision ~ (1 | workerid) + (1 | task/imageid) + (1 | imageid:workerid) + (1 | task:workerid),    
          data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)

p = glmer(as.factor(decision) ~ (1 | workerid) + (1 | imageid) + (1|task:workerid),   
                 data = dt, family = binomial)
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

p.var.image = data.frame(VarCorr(p))$vcov[3] / p.t
#p.var.imageint = (data.frame(VarCorr(p))$vcov[1] + data.frame(VarCorr(p))$vcov[4]) /p.t



#p_ = exclude.influence(p, "imageid", '2395_1.jpg')
#summary(p)


#p.vcov = VarCorr(p)
#trace(as.matrix(Matrix::bdiag(VarCorr(p_))) - v.matrix) - rankMatrix(v.matrix )[1]


# 1, 4
# 4
# list 
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
p_value <- c()



p_t <- exclude.influence(p, "imageid", 'n004201_0001_02.png')
n004201_0001_02.png

-2*(logLik(p)[1]- logLik(p_t)[1])

for (x in unique(Prolific$imageid)) {
  imageid <- append(imageid, x)
  p_temp <- exclude.influence(p, "imageid", x)
  loglik <- append(loglik, -2*(logLik(p)[1]- logLik(p_temp)[1]))
  
  var.image  <- append(var.image, data.frame(VarCorr(p_temp))$vcov[4])
  varint.image <- append(varint.image , data.frame(VarCorr(p_temp))$vcov[1] + data.frame(VarCorr(p_temp))$vcov[5] + data.frame(VarCorr(p_temp))$vcov[4])
  total.var <- append(total.var, sum(data.frame(VarCorr(p_temp))$vcov))
  
  var.image.change <- append(var.image.change, data.frame(VarCorr(p_temp))$vcov[4]/ sum(data.frame(VarCorr(p_temp))$vcov) - p.var.image )
  varint.image.change <- append(varint.image.change, (data.frame(VarCorr(p_temp))$vcov[1] + data.frame(VarCorr(p_temp))$vcov[5] + data.frame(VarCorr(p_temp))$vcov[4]) /sum(data.frame(VarCorr(p_temp))$vcov) - p.var.imageint)
  
  total.var.change <- append(total.var.change, sum(data.frame(VarCorr(p_temp))$vcov) / p.t)
  
  vartrace <- append(vartrace,  trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp)))))
  vartrace2 <- append(vartrace2, trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp))))- rankMatrix(v.matrix)[1])
  vartrace3 <- append(vartrace3, abs(trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp))))- rankMatrix(v.matrix)[1]))
  disagreerate <- append(disagreerate, min(table(Prolific[Prolific$imageid==x, ]$decision))/sum(table(Prolific[Prolific$imageid==x, ]$decision)))
  p_value <- append(p_value, pchisq(-2*(logLik(p)[1]- logLik(p_temp)[1]), df=66, lower.tail=F))
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

df.p = data.frame(imageid,loglik, total.var, var.image, varint.image, vartrace, vartrace2, vartrace3,
                  total.var.change, var.image.change, varint.image.change, disagreerate, p_value)

#names(Data.p) <- c('imageid', 'loglik', 'var.image', 'varint.image', 'total.var','var.image.change', 'varint.image.change', 'total.var.change', 'vartrace', 'vartrace2', 'vartrace3', 'disagreerate')


df.p$inf = ifelse(df.p$p_value>0.05, 1, 0)

ggplot(data = df.p, aes(x = imageid, y = loglik)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point()


ggplot(data = plot_df, aes(x = accuracy, y = total.var.change)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) + geom_point()


ggplot(data = plot_df, aes(x = accuracy, y = vartrace)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) + geom_point()

cor(plot_df$loglik, plot_df$accuracy)
cor(plot_df$vartrace, plot_df$accuracy)
cor(plot_df$total.var.change, plot_df$accuracy)

cor(df.p$loglik, df.p$disagreerate)
cor(df.p$vartrace, df.p$disagreerate)
cor(df.p$total.var.change, df.p$disagreerate)


#================ accuary ===========
colnames(simulation2) =  c("worker",    "uidworker", "task",       "num_task",   "y" )
colnames(simulation1)

simu_df = rbind(simulation1, simulation2)

gv_df = simu_df %>% group_by(worker) %>%
  summarise(accuracy = sum(y)/100)
View(gv_df)

colnames(df.p)[1] = 'workerid'
colnames(gv_df)[1] = 'workerid'

plot_df = merge(df.p, gv_df, by='workerid')

View(plot_df)

#df.p <- as.data.frame(Data.p)
View(df.p)
which.max(df.p$disagreerate)
df.p$disagreerate <- ifelse(df.p$disagreerate ==1, 0, df.p$disagreerate)


df.p1 = subset (df.p, select = -workerid)
cor(df.p1)
corrplot(cor(df.p1), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)





#============ deletion anaysis for image===========

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


for (x in unique(simulated_df$imageid)) {
  imageid <- append(imageid, x)
  p_temp <- exclude.influence(p, "imageid", x)
  loglik <- append(loglik, -2*(logLik(p)[1]- logLik(p_temp)[1]))
  
  var.image  <- append(var.image, data.frame(VarCorr(p_temp))$vcov[3])
  varint.image <- append(varint.image , data.frame(VarCorr(p_temp))$vcov[1] + data.frame(VarCorr(p_temp))$vcov[3])
  total.var <- append(total.var, sum(data.frame(VarCorr(p_temp))$vcov))
  
  var.image.change <- append(var.image.change, data.frame(VarCorr(p_temp))$vcov[3]/ sum(data.frame(VarCorr(p_temp))$vcov) - p.var.image )
  varint.image.change <- append(varint.image.change, (data.frame(VarCorr(p_temp))$vcov[1] + data.frame(VarCorr(p_temp))$vcov[3]) /sum(data.frame(VarCorr(p_temp))$vcov) - p.var.imageint)
  
  total.var.change <- append(total.var.change, sum(data.frame(VarCorr(p_temp))$vcov) / p.t)
  
  vartrace <- append(vartrace,  trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp)))))
  vartrace2 <- append(vartrace2, trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp))))- rankMatrix(v.matrix)[1])
  vartrace3 <- append(vartrace3, abs(trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp))))- rankMatrix(v.matrix)[1]))
  
  disagreerate <- append(disagreerate, min(table(simulated_df[simulated_df$imageid==x, ]$decision))/sum(table(simulated_df[simulated_df$imageid==x, ]$decision)))
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

df.i = data.frame(imageid,loglik, total.var, var.image, varint.image, vartrace, vartrace2, vartrace3,
                  total.var.change, var.image.change, varint.image.change, disagreerate)

View(df.i)


cor(df.i$loglik, df.i$disagreerate)
cor(df.i$vartrace, df.i$disagreerate)
cor(df.i$total.var.change, df.i$disagreerate)

#cor(plot_df$trace, plot_df$accuracy)
#cor(df.p$varimageratio, plot_df$accuracy)
#cor(df.p$varimageintratio, plot_df$accuracy)


ggplot(data = df.i, aes(x = disagreerate, y = loglik)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point()


ggplot(data = df.i, aes(x = disagreerate, y = total.var.change)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) + geom_point()


ggplot(data = df.i, aes(x = disagreerate, y = vartrace)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) + geom_point()


# =================================

# correlation
cor(plot_df$loglik, plot_df$accuracy)
cor(plot_df$trace, plot_df$accuracy)
cor(df.p$varimageratio, plot_df$accuracy)
cor(df.p$varimageintratio, plot_df$accuracy)


#df.p$trace = with(df.p, total.var - p.t)
#df.p$varimageratio = with(df.p, var.image/total.var - p.var.image )
#df.p$varimageintratio = with(df.p, varint.image/total.var - p.var.imageint)



#col = c("loglik", "var.image", "varint.image", "total.var", "var.image.change", "varint.image.change", "total.var.change")
#for (x in col) {
# mod = lm(paste("disagreerate ~",  x[[1]]), data = df.p)
#print(summary(mod))
#print(cor(df.p$var.image.change, df.p$disagreerate))
#plot(mod)
#}


# plot 
ggplot(data = plot_df, aes(x = accuracy, y = loglik)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point()


ggplot(data = plot_df, aes(x = accuracy, y = total.var.change)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) + geom_point()


ggplot(data = plot_df, aes(x = accuracy, y = vartrace)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) + geom_point()



library("writexl")
write_xlsx(df.p,"E:/Dropbox (ASU)/Code/8.3\\df.p.xlsx")




# annotator influence index - Mturk: 0.1741032
(data.frame(VarCorr(m))$vcov[1] + data.frame(VarCorr(m))$vcov[2] + data.frame(VarCorr(m))$vcov[3]) / sum(data.frame(VarCorr(m))$vcov)

# Prolific: 0.07184537
(data.frame(VarCorr(p))$vcov[1] + data.frame(VarCorr(p))$vcov[2] + data.frame(VarCorr(p))$vcov[3]) / sum(data.frame(VarCorr(p))$vcov)



S
#p_ = exclude.influence(p, "imageid", "2395_1.jpg")
#summary(p_1)

#sum(data.frame(VarCorr(p_1))$vcov)


#p_1 = exclude.influence(p, "imageid", '192_02_01_051_16.png')

#Prolific$imageid



# influence
#estex.pi <- influence(p, "imageid")
#estex.pw <- influence(p, "workerid")
#estex.pw <- influence(p, "WorkerId")
#estex.pw <- influence(p, "WorkerId")
#estex.pw <- influence(p, "WorkerId")

#p3 <- exclude.influence(p1, "image1", "2395_1.jpg")







# Mturk 
Mturk<- read_excel("E:/Dropbox (ASU)/Code/8.3/MtutkP.xlsx")
MTurkPerformanceData<- read.csv("E:/Dropbox (ASU)/Code/8.3/MTurkPerformanceData.csv")
Mturk$decision <- MTurkPerformanceData$Final.Decision
View(Mturk)

colnames(Mturk)[3] <- 'workerid'
colnames(Mturk)[6] <- 'task'
colnames(Mturk)[8] <- 'imageid'

Mturk$decision <- ifelse(Mturk$decision=='different', 1, 0)


m = glmer(decision ~ (1 | workerid) + (1 | task/imageid) + (1 | imageid:workerid) + (1 | task:workerid),  
          data = Mturk, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(m)

#estex.pi <- influence(p, "imageid")
#estex.pw <- influence(p, "workerid")


#m1 <- exclude.influence(m, "imageid", "1024")
#summary(m1)

#data.frame(VarCorr(m))$vcov[5]/5.485464

#data.frame(VarCorr(m))$vcov[5]/sum(data.frame(VarCorr(m))$vcov)
#sum(data.frame(VarCorr(m))$vcov)


# list 
imageid.m <- c()
loglik.m <- c()
total.var.m <- c()
var.image.m <- c()
varint.image.m <- c()
vartrace.m <- c()
vartrace2.m <- c()
vartrace3.m <- c()
total.var.change.m <- c()
var.image.change.m <- c()
varint.image.change.m <- c()
disagreerate.m <- c()

#table(subset(Mturk, imageid='1024'))

#min(table(Mturk[Mturk$imageid=='793', ]$decision))/max(table(Mturk[Mturk$imageid=='793', ]$decision))


#age = append(age, kappam.fleiss(x[, -1])$value)


#table(Mturk$decision)[1]

#min(table(Mturk[Mturk$imageid==x, ]$decision))/sum(table(Mturk[Mturk$imageid==x, ]$decision))
#min(table(Mturk[Mturk$imageid==x, ]$decision))/sum(table(Mturk[Mturk$imageid==x, ]$decision))

#for (x in unique(Mturk$imageid)) {
# imageid <- append(imageid, x)
#  m_temp <- exclude.influence(m, "imageid", x)
#  varratio <- append(varratio, data.frame(VarCorr(m_temp))$vcov[5]/5.485464)
#  ratio <- append(ratio, data.frame(VarCorr(m_temp))$vcov[5]/sum(data.frame(VarCorr(m_temp))$vcov))
#  change <- append(change, (ratio - 0.8258973))  # issue with changes: wrong number: 3303 why is that? 
#  disagreerate <- append(disagreerate, min(table(Mturk[Mturk$imageid==x, ]$decision))/sum(table(Mturk[Mturk$imageid==x, ]$decision)))
#}

# variance -covariance matrix 
m.vcov = VarCorr(m)
v.matrix.m = as.matrix(Matrix::bdiag(m.vcov))
rankMatrix(v.matrix.m )[1]  # rank of matrix


# another method for trace
m.t = sum(data.frame(VarCorr(m))$vcov)


#logLik(p)[1]
#loglik
#-2*(logLik(p)[1]- logLik(p_1)[1])
#-2*(logLik(p)[1]- logLik(p_)[1])
#min(table(Prolific[Prolific$imageid=='2395_1.jpg', ]$decision))/sum(table(Prolific[Prolific$imageid=='2395_1.jpg', ]$decision))
#min(table(Prolific[Prolific$imageid=='192_02_01_051_16.png', ]$decision))/sum(table(Prolific[Prolific$imageid=='192_02_01_051_16.png', ]$decision))

m.var.image = data.frame(VarCorr(m))$vcov[4] / m.t
m.var.imageint = (data.frame(VarCorr(m))$vcov[1] + data.frame(VarCorr(m))$vcov[4]) /m.t




for (x in unique(Mturk$imageid)) {
  imageid.m <- append(imageid.m, x)
  m_temp <- exclude.influence(m, "imageid", x)
  loglik.m <- append(loglik.m, -2*(logLik(m)[1]- logLik(m_temp)[1]))
  
  var.image.m  <- append(var.image.m, data.frame(VarCorr(m_temp))$vcov[4])
  varint.image.m <- append(varint.image.m , data.frame(VarCorr(m_temp))$vcov[1] + data.frame(VarCorr(m_temp))$vcov[4])
  total.var.m <- append(total.var.m, sum(data.frame(VarCorr(m_temp))$vcov))
  
  var.image.change.m <- append(var.image.change.m, data.frame(VarCorr(m_temp))$vcov[4]/ sum(data.frame(VarCorr(m_temp))$vcov) - m.var.image )
  varint.image.change.m <- append(varint.image.change.m, (data.frame(VarCorr(m_temp))$vcov[1] + data.frame(VarCorr(m_temp))$vcov[4]) /sum(data.frame(VarCorr(m_temp))$vcov) - m.var.imageint)
  
  total.var.change.m <- append(total.var.change.m, sum(data.frame(VarCorr(m_temp))$vcov) / m.t)
  
  vartrace.m <- append(vartrace.m,  trace(v.matrix.m - as.matrix(Matrix::bdiag(VarCorr(m_temp)))))
  vartrace2.m <- append(vartrace2.m, trace(v.matrix.m - as.matrix(Matrix::bdiag(VarCorr(m_temp))))- rankMatrix(v.matrix.m)[1])
  vartrace3.m <- append(vartrace3.m, abs(trace(v.matrix.m - as.matrix(Matrix::bdiag(VarCorr(m_temp))))- rankMatrix(v.matrix.m)[1]))
  #varratio <- append(varratio, data.frame(VarCorr(m_temp))$vcov[4]/sum(data.frame(VarCorr(m_temp))$vcov))
  #ratio <- append(ratio, data.frame(VarCorr(m_temp))$vcov[5]/sum(data.frame(VarCorr(m_temp))$vcov))
  #change <- append(change, (ratio - 0.8258973))  # issue with changes: wrong number: 3303 why is that? 
  disagreerate.m <- append(disagreerate.m, min(table(Mturk[Mturk$imageid==x, ]$decision))/sum(table(Mturk[Mturk$imageid==x, ]$decision)))
}


#col.m =  c('imageid.m', 'loglik.m', 'var.image.m', 'varint.image.m', 'total.var.m','var.image.change.m', 'varint.image.change.m', 'total.var.change.m', 'vartrace.m', 'vartrace2.m', 'vartrace3.m', 'disagreerate.m')
#for (x in col.m) {
#print(length(x))
#}

length(total.var.m)



# df
Data.m <- list(unlist(imageid.m),
               unlist(loglik.m),
               unlist(var.image.m),
               unlist(varint.image.m),
               unlist(total.var.m),
               unlist(var.image.change.m),
               unlist(varint.image.change.m),
               unlist(total.var.change.m),
               unlist(vartrace.m),
               unlist(vartrace2.m),
               unlist(vartrace3.m),
               #unlist(change), 
               unlist(disagreerate.m)
)

names(Data.m) <- c('imageid', 'loglik', 'var.image', 'varint.image', 'total.var','var.image.change', 'varint.image.change', 'total.var.change', 'vartrace', 'vartrace2', 'vartrace3', 'disagreerate')


df.m <- as.data.frame(Data.m)
View(df.m)
which.max(df.m$disagreerate)
df.m$disagreerate <- ifelse(df.m$disagreerate ==1, 0, df.m$disagreerate)




df.m1 = subset (df.m, select = -imageid.m)
cor(df.m1)
corrplot(cor(df.m1), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)




# correlation
cor(df.m$loglik, df.m$disagreerate.m)
cor(df.m$trace, df.m$disagreerate.m)
cor(df.m$varimageratio, df.m$disagreerate.m)
cor(df.m$varimageintratio, df.m$disagreerate.m)


#df.p$trace = with(df.p, total.var - p.t)
#df.p$varimageratio = with(df.p, var.image/total.var - p.var.image )
#df.p$varimageintratio = with(df.p, varint.image/total.var - p.var.imageint)



#col = c("loglik", "var.image", "varint.image", "total.var", "var.image.change", "varint.image.change", "total.var.change")
#for (x in col) {
# mod = lm(paste("disagreerate ~",  x[[1]]), data = df.p)
#print(summary(mod))
#print(cor(df.p$var.image.change, df.p$disagreerate))
#plot(mod)
#}


# plot 
ggplot(data = df.m, aes(x = disagreerate, y = total.var.change)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point()



write_xlsx(df.m,"E:/Dropbox (ASU)/Code/8.3\\df.m.xlsx")
































# df
Data.m <- list(unlist(imageid),
               unlist(varratio),
               unlist(ratio),
               #unlist(change), 
               unlist(disagreerate)
)

names(Data.m) <- c('imageid',  'varchange', 'var.total', 'disagreerate')


df.m <- as.data.frame(Data.m)
View(df.m)
df.m$disagreerate <- ifelse(df.m$disagreerate ==1, 0, df.m$disagreerate)
which.max(df.m$disagreerate)
df.m$change <- df.m$var.total - 0.8258973


df.m$disagreerate
df.m
dim(df.m)
length(disagreerate)


#imageid.1 = c()
#disagg = c()


#for (x in unique(df.m$imageid)) {
#imageid.1 <- append(imageid.1, x)
# disagg <- append(disagg, min(table(Mturk[Mturk$imageid==x, ]$decision))/sum(table(Mturk[Mturk$imageid==x, ]$decision)))
#}
#
#Data <- list(unlist(imageid.1),
#            unlist(disagg)
#)

#names(Data) <- c('imageid', 'disagreerate')
#df.m1 <- as.data.frame(Data)


#df.m1$disagreerate <- ifelse(df.m1$disagreerate ==1, 0, df.m1$disagreerate)
#View(df.m1)
#df.m0 = merge(df, df.m1, by='imageid')
#View(df.m0)
#df = df.m[-c(4)]
#View(df)
#plot(abs(df.m0$change), df.m0$disagreerate)
#cor(abs(df.m0$change), df.m0$disagreerate)
#cor(abs(df.m0$change), df.m0$disagreerate)
#summary(lm(df.m0$disagreerate ~ df.m0$change, data=df.m0))


# plot, correlation 
#cor(df.m0)
#corrplot(cor(df.m0), type = "upper", order = "hclust", 
#        tl.col = "black", tl.srt = 45)






#df_ = Mturk[Mturk$imageid!='394', ]
#m_ = glmer(decision ~ (1 | workerid) + (1 | imageid) + (1 | imageid:workerid) + (1 | task:workerid)  + (1 | task/imageid),    
#          data = df_, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
#summary(m_)


#(data.frame(VarCorr(m_))$vcov[5] +data.frame(VarCorr(m_))$vcov[4])/sum(data.frame(VarCorr(m_))$vcov)
#0.8243667



library(rstan)
