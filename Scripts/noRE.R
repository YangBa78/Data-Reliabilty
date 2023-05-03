library(influence.ME)
library(dplyr)
library(ggplot2)
library(ggpmisc)

nworker= 160 # number of pupils per school
nimage = 80 # number of schools
#npupilstotal = npupils*nschools # to give each pupil a unique number

# 1.1 Design data frame:
simdf = data.frame("workerid" = rep(1:nworker,
                                 each = nimage),
                   "imageid" = rep(1:nimage,
                                  nworker))
# task
easy <- rep("easy", nimage/2)
hard <- rep("hard", nimage/2)
sample_task <- c(easy, hard)
simdf$task <- sample(sample_task)

simdf

# GT 
same <- rep("same", nimage/4)
diff <- rep("different", nimage/4)
sample_vec <- c(same, diff)
sample_same <- sample(sample_vec)
sample_diff <- sample(sample_vec)

simdf$trueLable[simdf$task=='easy'] <- sample_same
simdf$trueLable[simdf$task=='hard'] <- sample_diff


df = simdf 
# random guess

df$decision <- NA
#newT$correct <- NA
df[df$workerid ==1, ][df[df$workerid ==1, ]$trueLable=='same',]$decision <- 
  sample(c(0,1), dim(df[df$workerid ==1, ][df[df$workerid ==1, ]$trueLable=='same',])[1], prob=c(0.3, 1-0.3), rep=TRUE)
df[df$workerid ==1, ][df[df$workerid ==1, ]$trueLable=='different',]$decision <- 
  sample(c(0,1), dim(df[df$workerid ==1, ][df[df$workerid ==1, ]$trueLable=='different',])[1], prob=c(0.3, 1-0.3), rep=TRUE)


#newT$correct <- NA
df[df$workerid ==2, ][df[df$workerid ==2, ]$task=='easy',]$decision <- 
  sample(c(0,1), dim(df[df$workerid ==2, ][df[df$workerid ==2, ]$task=='easy',])[1], prob=c(0.5, 1-0.5), rep=TRUE)
df[df$workerid ==2, ][df[df$workerid ==2, ]$task=='hard',]$decision <- 
  sample(c(0,1), dim(df[df$workerid ==2, ][df[df$workerid ==2, ]$task=='hard',])[1], prob=c(0.5, 1-0.5), rep=TRUE)

# repeat pattern
df[df$workerid==3,]$decision <- rep(c(0,1), times = nimage/2)  

df[df$workerid==4,]$decision <- rep(c(1,0), times = nimage/2)  


df$correct = NA


for (x in 5: nworker){
  r = runif(1, 0.8, 0.9)
  r_minus = runif(1, 0.1, 0.2)
  #r = rnorm(1, 0.75, 0.1)
  df[df$workerid ==x, ][df[df$workerid ==x, ]$task=='easy',]$correct<- 
    sample(c(1,0), dim(df[df$workerid ==x, ][df[df$workerid ==x, ]$task=='easy',])[1], prob=c(r, 1-r), rep=TRUE)
  df[df$workerid ==x, ][df[df$workerid ==x, ]$task=='hard',]$correct<- 
    sample(c(1,0), dim(df[df$workerid ==x, ][df[df$workerid ==x, ]$task=='hard',])[1], prob=c(r-0.15, 1-r+0.15), rep=TRUE)
}

df

df$convert = ifelse(df$trueLable=='different', 1, 0)

for (x in 5: nworker){
  df[df$workerid==x,]$decision = ifelse(df[df$workerid==x,]$correct==1, df[df$workerid==x,]$convert, 1- df[df$workerid==x,]$convert)
}


for (x in 1:4){
  df[df$workerid==x,]$correct = ifelse(df[df$workerid==x,]$decision== df[df$workerid==x,]$convert, 1, 0)
}

p <- glmer(as.factor(decision) ~  (1 | workerid) + (1 | imageid) ,   
             data = df, family = binomial)
summary(p)


group = df %>% group_by(workerid, task) %>% summarise(accuary = sum(correct)/(nimage/2), 
                                                .groups = 'drop') %>%
  as.data.frame()
group

groupT = df %>% group_by(workerid) %>% summarise(accuary = sum(correct)/nimage, 
                                                      .groups = 'drop') %>%
  as.data.frame()
groupT



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

# 64, 93, 118 
# 64 easy   0.625.  64: 46 34
# 64 hard   0.475.  93: 33 47
# 93 easy   0.925.  118: 41 39
# 93 hard   0.650.  119: 38 42
# 118 easy   0.600
# 118 hard   0.475
# 119 easy   0.625
# 119 hard   0.475

# trace
#p.t = trace(v.matrix)


# variance -covariance matrix 
p.vcov = VarCorr(p)
v.matrix = as.matrix(Matrix::bdiag(p.vcov))
rankMatrix(v.matrix )[1]  # rank of matrix


# another method for trace
p.t = sum(data.frame(VarCorr(p))$vcov)

p.var.image = data.frame(VarCorr(p))$vcov[1] / p.t


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

for (x in unique(df$workerid)) {
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
  accuary  <- append(accuary , sum(df[df$workerid==x,]$correct)/80)
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
dfl = df.p[, c('workerid', 'loglik', 'vartrace', 'total.var.change', 'p_value', 'accuary')]
head(dfl)

# 84, 90, 108, 146, 147, 
#17,33, 63 

dfl$zscore = (dfl$loglik - mean(dfl$loglik)) / sd(dfl$loglik)
dfl$zs = (dfl$vartrace - mean(dfl$vartrace)) / sd(dfl$vartrace)
dfl[abs(dfl$zscore)>1.96,]
# 64, 93, 118 
# 64 easy   0.625.  64: 46 34, 0.5500
# 64 hard   0.475.  93: 33 47
# 93 easy   0.925.  118: 41 39
# 93 hard   0.650.  119: 38 42
# 118 easy   0.600
# 118 hard   0.475
# 119 easy   0.625
# 119 hard   0.475

dfl[abs(dfl$zs)>1.96,]
# 64, 93, 118 
# 64 easy   0.625.  64: 46 34, 0.5500
# 64 hard   0.475.  93: 33 47, 0.7875
# 93 easy   0.925.  118: 41 39, 0.5375
# 93 hard   0.650.  119: 38 42, 0.5500
# 118 easy   0.600  140: 38 42, 0.6000
# 118 hard   0.475
# 119 easy   0.625
# 119 hard   0.475
# 140 easy  0.625  
# 140 hard  0.575

range(groupT[c(5:160), ]$accuary) # 0.5375 0.8250

groupT[order(groupT$accuary),]

#2          2  0.4125
#1          1  0.4625
#3          3  0.4750
#4          4  0.5250
#118      118  0.5375
#126      126  0.5375, 39 41
#64        64  0.5500, 35 45
#119      119  0.5500, 
#29        29  0.5625
#52        52  0.5625
#101      101  0.5625
#96        96  0.5750
#13        13  0.5875
#21        21  0.5875
#57        57  0.5875
#117      117  0.5875

table(df$workerid, df$decision)

dfl[dfl$accuary<0.6, ]
hist(dfl$accuary)

dfl$rankl = rank(dfl$loglik)
dfl$rankabtr = rank(dfl$vartrace)
dfl$ranktr = rank(abs(dfl$vartrace))
dfl$ranktrratio = rank(dfl$total.var.change)

dfl

#newdtC2$convTruth = ifelse(newdtC2$trueLable=='same', 0, 1)
#newdtC2$correct = ifelse(newdtC2$decision ==newdtC2$convTruth, 0, 1)

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


#########################
df[df$workerid==5, ]$decision = df[df$workerid==1, ]$decision
df[df$workerid==6, ]$decision = df[df$workerid==1, ]$decision
df[df$workerid==7, ]$decision = df[df$workerid==2, ]$decision
df[df$workerid==8, ]$decision = df[df$workerid==3, ]$decision
df[df$workerid==9, ]$decision = df[df$workerid==4, ]$decision
df[df$workerid==10, ]$decision = df[df$workerid==2, ]$decision


rbinom(nimage,1,0.5)






