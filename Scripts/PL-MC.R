library(readxl)
library(markovchain)

pl <- read_excel("Data/Prolific.xlsx")
colnames(pl)


pl1 = pl[,c('WorkerId','Time Spend','Final Decision', 'error')]
pl1 = pl1[pl1$WorkerId!='2f33f23f23',]
colnames(pl1)[2] = 'time'

length(unique(pl$WorkerId))

table(pl[pl$WorkerId==unique(pl$WorkerId)[3],]$`True Label`)

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

for (i in unique(pl1$WorkerId)){
  print(i)
  fit<-markovchainFit(pl1[pl1$WorkerId==i,]$`Final Decision`)
  
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


a = data.frame( 'ID' = unique((pl1$WorkerId)),
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

#View(a)


r = a[(a$kl1<=0.05109787) & (a$kl2<=0.05109787),]
pc1 = a[(a$klp1.1<=1.839218 ) & (a$klp1.2<=1.839218 ),]
pc2 = a[(a$klp2.1<=1.839218 ) & (a$klp2.2<=1.839218 ),]
rp1 = a[(a$kls1.1<=2.946133 ) & (a$kls1.2<=2.946133   ),]
rp2 = a[(a$kls2.1<=2.946133 ) & (a$kls2.2<=2.946133   ),]


mean(as.numeric(KL(rbind(c(0.51, 0.49), c(0, 1)), unit = "log")) + as.numeric(KL(rbind(c(0.75, 0.25), c(0, 1)), unit = "log"))) 
mean(as.numeric(KL(rbind(c(0.51, 0.49), c(0.5, 0.5)), unit = "log")) + as.numeric(KL(rbind(c(0.75, 0.25), c(0.5, 0.5)), unit = "log"))) 


pc = rbind(pc1, pc2)

rp = rbind(rp1, rp2)

h = c(r$ID, pc$ID, rp$ID)
length(h)
dim(r)
dim(pc)
dim(rp)

h


t = c(101, 117, 3, 107,   8)
sum(r$ID %in% t)
sum(pc$ID %in% t)
sum(rp$ID %in% t)



del = c(101)
mc = c(102,  74, 101, 136,  14, 130, 107,   8, 117,   3)
for (i in mc){
  fit<-markovchainFit(pl1[pl1$WorkerId==i,]$`Final Decision`)
  print(i)
  print(fit$estimate@transitionMatrix)
}


for (i in unique(c(del, mc))){
  run = sort(rle(pl1[pl1$WorkerId==i,]$`Final Decision`)[1]$lengths, decreasing =TRUE)[1]
  print(i)
  print(run)
}


sort(rle(pl[pl$WorkerId==unique(pl$WorkerId)[4],]$`True Label`)[1]$lengths, decreasing =TRUE)[1]

table(pl[pl$WorkerId==unique(pl$WorkerId)[2],]$`True Label`)


pl[pl$WorkerId==unique(pl$WorkerId)[2],]$`True Label`

#table(pl1[pl1$WorkerId=="611276eef2fedd783d9b30ff",])


fit<-markovchainFit(pl[pl$WorkerId==unique(pl$WorkerId)[4],]$`True Label`)
fit$estimate@transitionMatrix




#########################################################################################

