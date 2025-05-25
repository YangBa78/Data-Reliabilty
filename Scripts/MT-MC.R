library(readr)
library(markovchain)


mt <- read_csv("Data/MTurkPerformanceData.csv")
colnames(mt)

mt1 = mt[,c('Assignment ID','Completion Time','Final Decision', 'Coded Prediction')]
colnames(mt1)[1] = 'WorkerId'
mt1


#########################################################################################

klr.1 = c()
klr.2 = c()
klp1.1 = c()
klp1.2 = c()
klp2.1 = c()
klp2.2 = c()
kls1.1 = c()
kls1.2 = c()
kls2.1 = c()
kls2.2 = c()
#ll = c()

for (i in unique(mt1$WorkerId)){
  print(i)
  fit<-markovchainFit(mt1[mt1$WorkerId==i,]$`Final Decision`)
  
  klr.1 = append(klr.1, as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0.5, 0.5)), unit = "log")))
  klr.2 = append(klr.2, as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0.5, 0.5)), unit = "log")))
  #klr.1 = append(klr.1, klr.1)
  #klr = append(klr, as.numeric((klr.1 + klr.2)/2))
  #ll = append(ll, fit$logLikelihood)
  
  kls1.1 = append(kls1.1, as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(1, 0)), unit = "log")))
  kls1.2 = append(kls1.2, as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0, 1)), unit = "log")))
  #kls1 = append(kls1, as.numeric((kls1.1 + kls1.2)/2))
  
  kls2.1 = append(kls2.1, as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0, 1)), unit = "log")))
  kls2.2 = append(kls2.2, as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(1, 0)), unit = "log")))
  #kls2 = append(kls2, as.numeric((kls2.1 + kls2.2)/2))
  
  klp1.1 = append(klp1.1, as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(1, 0)), unit = "log")))
  klp1.2 = append(klp1.2, as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(1, 0)), unit = "log")))
  #klp1 = append(klp1, as.numeric((klp1.1 + klp1.2)/2))
  
  klp2.1 = append(klp2.1, as.numeric(KL(rbind(fit$estimate@transitionMatrix[1,], c(0, 1)), unit = "log")))
  klp2.2 = append(klp2.2, as.numeric(KL(rbind(fit$estimate@transitionMatrix[2,], c(0, 1)), unit = "log")))
  #klp2 = append(klp2, as.numeric((klp2.1 + klp2.2)/2))
  
}


a = data.frame( 'ID' = unique(mt1$WorkerId),
                'kl1' = sqrt(klr.1),
                'kl2' = sqrt(klr.2),
                #'ll' = ll,
                'klp1.1' = klp1.1,
                'klp1.2' = klp1.2,
                'klp2.1' = klp2.1,
                'klp2.2' = klp2.2,
                'kls1.1' = kls1.1,
                'kls1.2' = kls1.2,
                'kls2.1' = kls2.1,
                'kls2.2' = kls2.2
)

#a$sqrtkl1 = sqrt(a$kl1)
#a$sqrtkl2 = sqrt(a$kl2)

#View(a)




#a[a$klp1<cutoff.p2,]
#a[a$klp2<cutoff.p2,]
#a[a$kls1<cutoff.w1,]

#a[a$sqrtkl<cutoff.random,]


a[(a$kl1<=0.02810061) & (a$kl2<=0.02810061),]
a[(a$klp1.1<=3.367109) & (a$klp1.2<=3.367109),]
a[(a$klp2.1<=3.367109) & (a$klp2.2<=3.367109),]
a[(a$kls1.1<=3.924257) & (a$kls1.2<=3.924257),]
a[(a$kls2.1<=3.924257) & (a$kls2.2<=3.924257),]


mean(mt1$`Completion Time`)
sd(mt1$`Completion Time`)




r = a[(a$kl1<=0.02810061) & (a$kl2<=0.02810061),]
pc1 = a[(a$klp1.1<=3.367109) & (a$klp1.2<=3.367109),]
pc2 = a[(a$klp2.1<=3.367109) & (a$klp2.2<=3.367109),]
rp1 = a[(a$kls1.1<=3.924257) & (a$kls1.2<=3.924257),]
rp2 = a[(a$kls2.1<=3.924257) & (a$kls2.2<=3.924257),]



pc = rbind(pc1, pc2)

rp = rbind(rp1, rp2)

h = c(r$ID, pc$ID, rp$ID)
length(h)
dim(r)
dim(pc)
dim(rp)

h


t = c(2,  35,  65, 108, 118,  55,  62, 106, 109, 124, 126, 141, 155,
      3,  59,  56,  83, 143)

sum(r$ID %in% t)
sum(pc$ID %in% t)
sum(rp$ID %in% t)


table(mt[mt$WorkerId==unique(mt$WorkerId)[3],]$`True Label`)


for (i in ptn.spam.mt){
  fit<-markovchainFit(mt[mt$workerid==i,]$decision)
  print(fit$estimate@transitionMatrix)
}


del = c(50,  30,  72, 100,   1, 121)
for (i in del){
  fit<-markovchainFit(mt1[mt1$WorkerId==i,]$`Final Decision`)
  print(i)
  print(fit$estimate@transitionMatrix)
}

for (i in del){
  d = mt1[mt1$WorkerId==i,]$`Final Decision`
  run = rle(d)[1]$lengths
  print(i)
  print(run)
}

table(mt[mt$`Assignment ID`==50,]$`True Label`)


#########################################################################################

