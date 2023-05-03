Annot <- read_excel("Data/df_times.xlsx")

Ann <- Annot[, c('Image.Name.1', 'Ground.Truth', 'Worker.Id', 'Task.ID', 'pred.Human', 'Age', 'Color', 
                 'Hair', 'Gender.Same', 'Race.Same', 'Exp.Same', 'Rotation.Same', 'Gender.Type', 
                 'Race.Type', 'Exp.Type', 'Rotation.Type', 'Occ', 'Makeup', 'Illumination')]
#Ann <- na.omit(Ann)

colnames(Annot)[2] = 'image1'

pa <- glmer(as.factor(Human_pred) ~ (1| Worker_id) + (1| image1) + (1| Worker_id: image1) + (1| Worker_id: Times) + (1| Times), 
            data = Annot , family = binomial)

summary(pa)


pa <- glmer(as.factor(Human_pred) ~ (1| Worker_id: Age) + (1| Color), 
            data = Annot , family = binomial)

Annot$human_code = ifelse(Annot$Human=='Correct', 1, 0)
#Annot$accuracy  = 


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
p.vcov = VarCorr(pa)
v.matrix = as.matrix(Matrix::bdiag(p.vcov))
rankMatrix(v.matrix )[1]  # rank of matrix


# another method for trace
p.t = sum(data.frame(VarCorr(pa))$vcov)

p.var.image = data.frame(VarCorr(pa))$vcov[4] / p.t


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
chiindep <- c()
c <- c()


table(dt$workerid,dt$decision)
min(table(dt$workerid,dt$decision)[, 1])

for (x in unique(Annot$Worker_id)) {
  workerid <- append(workerid, x)
  p_temp <- exclude.influence(pa, "Worker_id", x)
  loglik <- append(loglik, -2*(logLik(p)[1]- logLik(p_temp)[1]))
  
  #var.image  <- append(var.image, data.frame(VarCorr(p_temp))$vcov[4])
  #varint.image <- append(varint.image , data.frame(VarCorr(p_temp))$vcov[1] + data.frame(VarCorr(p_temp))$vcov[5] + data.frame(VarCorr(p_temp))$vcov[4])
  #total.var <- append(total.var, sum(data.frame(VarCorr(p_temp))$vcov))
  
  #var.image.change <- append(var.image.change, data.frame(VarCorr(p_temp))$vcov[4]/ sum(data.frame(VarCorr(p_temp))$vcov) - p.var.image )
  #varint.image.change <- append(varint.image.change, (data.frame(VarCorr(p_temp))$vcov[1] + data.frame(VarCorr(p_temp))$vcov[3] + data.frame(VarCorr(p_temp))$vcov[4]) /sum(data.frame(VarCorr(p_temp))$vcov) - p.var.imageint)
  
  total.var.change <- append(total.var.change, sum(data.frame(VarCorr(p_temp))$vcov) / p.t)
  
  vartrace <- append(vartrace,  trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp)))))
  #vartrace2 <- append(vartrace2, trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp))))- rankMatrix(v.matrix)[1])
  #vartrace3 <- append(vartrace3, abs(trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp))))- rankMatrix(v.matrix)[1]))
  #disagreerate <- append(disagreerate, min(table(Prolific[Prolific$imageid==x, ]$decision))/sum(table(Prolific[Prolific$imageid==x, ]$decision)))
  #p_value <- append(p_value, pchisq(-2*(logLik(p)[1]- logLik(p_temp)[1]), df=1362, lower.tail=F))
  #chi2 = chisq.test(table(dt[dt$workerid==x,]$task, dt[dt$workerid==x,]$decision), correct=F)
  #c <- append(c, as.numeric(sqrt(chi2$statistic / sum(table(dt[dt$workerid==x,]$task, dt[dt$workerid==x,]$decision)))))
  #chiindep <- append(chiindep, chi2$p.value)
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
length(c)
length(chiindep)


df.p = data.frame(workerid,loglik,  vartrace, 
                  total.var.change)

# concatenate
dfl = df.p[, c('workerid', 'loglik', 'vartrace', 'total.var.change')]
head(df.p)

for (i in head(df.p[order(df.p$vartrace), ]$workerid)){
  print(acc[i])
}

for (i in tail(df.p[order(df.p$vartrace), ]$workerid)){
  print(acc[i])
}

# low acc
for (i in tail(df.p[order(abs(df.p$vartrace)), ]$workerid)){
  print(acc[i])
}

# high acc
for (i in head(df.p[order(abs(df.p$vartrace)), ]$workerid)){
  print(acc[i])
}



for (i in head(df.p[order(df.p$loglik), ]$workerid)){
  print(acc[i])
}

for (i in tail(df.p[order(df.p$loglik), ]$workerid)){
  print(acc[i])
}


for (i in head(df.p[order(df.p$total.var.change), ]$workerid)){
  print(acc[i])
}

for (i in tail(df.p[order(df.p$total.var.change), ]$workerid)){
  print(acc[i])
}



acc['A2QRIJVRFJD4C5']
for (i in df.p$workerid){
  df.p[df.p$workerid==i,]$acc = as.numeric(acc[i])
}


acc = c()
id = c()
for (i in unique(Annot$Worker_id)){
  acc[i] = sum(Annot[Annot$Worker_id==i,]$human_code)/dim(Annot[Annot$Worker_id==i,])[1]
}



tail(df.p[order(df.p$rankl), ])
head(df.p[order(df.p$rankl), ])

tail(df.p[order(df.p$rankabtr), ])
head(df.p[order(df.p$rankabtr), ])


head(df.p[order(df.p$ranktr), ])
tail(df.p[order(df.p$ranktr), ])


df.p$workerid
acc


df.p$rankl = rank(df.p$loglik)
df.p$rankabtr = rank(df.p$vartrace)
df.p$ranktr = rank(abs(df.p$vartrace))
df.p$ranktrratio = rank(df.p$total.var.change)



as.numeric(acc['AXZEMSNUZON2T'])

hist(acc)
acc[acc<0.4]


library(tidyverse)
library(timetk)
library(tibbletime)
library(lubridate)
library(forecast)

set.seed(123)

# Create a data frame with the categorical variable and the target variable
df <- tibble(
  task_difficulty = sample(c("easy", "medium", "hard"), 100, replace = TRUE),
  same_diff = sample(c("same", "different"), 100, replace = TRUE)
)

# Create a time series object
ts <- df %>%
  mutate(date = seq.Date(from = ymd("20220101"), by = "day", length.out = n())) %>%
  select(date, same_diff) %>%
  tk_ts(start = "2022-01-01", freq = "day")

# Create a random walk model
rw_model <- rwf(ts)

# Plot the random walk model
autoplot(rw_model) +
  labs(title = "Random Walk Model for Same vs Different", y = "Same vs Different")

