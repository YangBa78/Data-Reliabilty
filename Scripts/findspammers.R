library(readxl)
library(dplyr)


########################## Airport data ##################################

fw = read.csv("~/Dropbox (ASU)/Facewise/DF_Facewise_Airports1.csv")

fw = fw[,c('ID','Time.Spend','Final.Decision', 'Easyness', 'Pair')]
colnames(fw)[1] = 'workerid'
colnames(fw)[2] = 'time'
colnames(fw)[3] = 'decision'
colnames(fw)[4] = 'task'
colnames(fw)[5] = 'image'


fw1 = c(119, 234, 248, 251, 314, 316, 227, 305, 352, 381, 217, 320, 349, 355, 234, 350, 374)


# primary
pri.spam = c()
for (i in unique(fw$workerid)){
  d = fw[fw$workerid==i,]$decision
  run = sort(rle(d)[1]$lengths, decreasing =TRUE)[1]
  if (run >=11){
    pri.spam = append(pri.spam, i)
    print(c(i, run))
  }
}

sort(pri.spam)

for (i in pri.spam){
  fit<-markovchainFit(fw[fw$workerid==i,]$decision)
  print(i) 
  print(fit$estimate@transitionMatrix)
}


for (i in sort(fw1)){
  fit<-markovchainFit(fw[fw$workerid==i,]$decision)
  print(i) 
  print(fit$estimate@transitionMatrix)
}


fit<-markovchainFit(fw[fw$workerid==374,]$decision)
fit$estimate@transitionMatrix

d = fw[fw$workerid==374,]$decision
run = rle(d)[1]$lengths
run
sum(run==1)

fw[fw$workerid==350,]$decision

# repeated
ptn.spam = c()
for (i in unique(fw1)){
  d = fw[fw$workerid==i,]$decision
  run = rle(d)[1]$lengths
  ccount = 0
  maxcount = 0
  for (j in 1:length(run)){
    if (run[j]==1){
      ccount <- ccount + 1
    }else{
      if (ccount > maxcount) {
        maxcount <- ccount
      }
      ccount <- 0
    }
  }
  
  if (ccount > maxcount) {
    maxcount <- ccount
  }
  
  if (maxcount/2 >=4.5){
    print(c(i, maxcount/2))
    ptn.spam  = append(ptn.spam, i)
  }
}

ptn.spam 


for (i in ptn.spam){
  fit<-markovchainFit(fw[fw$workerid==i,]$decision)
  print(fit$estimate@transitionMatrix)
}


fit<-markovchainFit(fw[fw$workerid==234,]$decision)
print(fit$estimate@transitionMatrix)


# random 
# ineligible



########################################################## Prolific data ###########################

pl <- read_excel("~/Dropbox (ASU)/Code/Data-Reliabilty/Data/Prolific.xlsx")
pl = pl[pl['WorkerId']!='2f33f23f23',]

pl = pl[,c('WorkerId','Time Spend','Final Decision', 'error', 'task', 'Image 1')]
colnames(pl)[1] = 'workerid'
colnames(pl)[2] = 'time'
colnames(pl)[3] = 'decision'
colnames(pl)[6] = 'image'


# primary
pri.spam.pl = c()
for (i in unique(pl$workerid)){
  d = pl[pl$workerid==i,]$decision
  run = sort(rle(d)[1]$lengths, decreasing =TRUE)[1]
  if (run >=10){
    pri.spam.pl = append(pri.spam.pl, i)
    print(c(i, run))
  }
}
pri.spam.pl

for (i in pri.spam.pl){
  fit<-markovchainFit(pl[pl$workerid==i,]$decision)
  print(fit$estimate@transitionMatrix)
}


# repeated
ptn.spam.pl = c()
for (i in unique(pl$workerid)){
  d = pl[pl$workerid==i,]$decision
  run = rle(d)[1]$lengths
  ccount = 0
  maxcount = 0
  for (j in 1:length(run)){
    if (run[j]==1){
      ccount <- ccount + 1
    }else{
      if (ccount > maxcount) {
        maxcount <- ccount
      }
      ccount <- 0
    }
  }
  
  if (ccount > maxcount) {
    maxcount <- ccount
  }
  
  if (maxcount/2 >=4.5){
    print(c(i, maxcount/2))
    ptn.spam.pl  = append(ptn.spam.pl, i)
  }
}

ptn.spam.pl


for (i in ptn.spam.pl){
  fit<-markovchainFit(pl[pl$workerid==i,]$decision)
  print(fit$estimate@transitionMatrix)
}


fit<-markovchainFit(pl[pl$workerid==126,]$decision)
print(fit$estimate@transitionMatrix)




#################################################### MTurk data ###################################

mt <- read.csv("~/Dropbox (ASU)/Code/Data-Reliabilty/Data/MTurkPerformanceData.csv")

mt = mt[,c('Assignment.ID','Completion.Time','Final.Decision', 'Coded.Prediction', 'Task.difficulty', 'TID')]
colnames(mt)[1] = 'workerid'
colnames(mt)[2] = 'time'
colnames(mt)[3] = 'decision'
colnames(mt)[4] = 'accuracy'
colnames(mt)[5] = 'task'
colnames(mt)[6] = 'image'
mt


mt1 = c(50,  30,  72, 100,   1, 121, 141, 137,  11,  73, 123,   2,  35,
        65, 108, 118,  10,   0,   3, 113,  56, 151,  62,  67, 109, 124,
        155,  53,  83,  44,  55, 106, 126,  59, 143)



# primary
pri.spam.mt = c()
for (i in unique(mt$workerid)){
  d = mt[mt$workerid==i,]$decision
  run = sort(rle(d)[1]$lengths, decreasing =TRUE)[1]
  if (run >11){
    pri.spam.mt = append(pri.spam.mt, i)
    print(c(i, run))
  }
}
sort(pri.spam.mt)

for (i in pri.spam.mt){
  fit<-markovchainFit(mt[mt$workerid==i,]$decision)
  print(i)
  print(fit$estimate@transitionMatrix)
}


fit<-markovchainFit(mt[mt$workerid==151,]$decision)
print(fit$estimate@transitionMatrix)


d = mt[mt$workerid==151,]$decision
run = rle(d)[1]$lengths
run
sum(run==1)



# repeated
ptn.spam.mt = c()
for (i in unique(mt1)){
  d = mt[mt$workerid==i,]$decision
  run = rle(d)[1]$lengths
  ccount = 0
  maxcount = 0
  for (j in 1:length(run)){
    if (run[j]==1){
      ccount <- ccount + 1
    }else{
      if (ccount > maxcount) {
        maxcount <- ccount
      }
      ccount <- 0
    }
  }
  
  if (ccount > maxcount) {
    maxcount <- ccount
  }
  
  if (maxcount/2 >=4.5){
    print(c(i, maxcount/2))
    ptn.spam.mt  = append(ptn.spam.mt, i)
  }
}

ptn.spam.mt




for (i in ptn.spam.mt){
  fit<-markovchainFit(mt[mt$workerid==i,]$decision)
  print(fit$estimate@transitionMatrix)
}


fit<-markovchainFit(mt[mt$workerid==109,]$decision)
print(fit$estimate@transitionMatrix)





