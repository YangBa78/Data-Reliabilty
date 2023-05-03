newT1$CT = ifelse(newT1$trueLable=='different', 1, 0)
newT1$C = ifelse(newT1$decision == newT1$CT, 1, 0)
a = newT1 %>% group_by(trueLable) %>% summarise(acc = sum(C), n = n())



# if I simulate very low accuracy to see loglik range !!!
# all possible situations


tbl = matrix(data=c(55, 45, 20, 30), nrow=2, ncol=2, byrow=T)
dimnames(tbl) = list(City=c('B', 'T'), Gender=c('M', 'F'))

chi2 = chisq.test(tbl, correct=F)
#table(newT1$task, newT1$decision)
c(chi2$statistic, chi2$p.value)
sqrt(chi2$statistic / sum(tbl))


tbl = matrix(data=c(51, 49, 24, 26), nrow=2, ncol=2, byrow=T)
dimnames(tbl) = list(City=c('B', 'T'), Gender=c('M', 'F'))

chi2 = chisq.test(tbl, correct=F)
c(chi2$statistic, chi2$p.value)

sqrt(chi2$statistic / sum(tbl))

chi2 = chisq.test(table(newT$task, newT$decision), correct=F)
chi2 = chisq.test(table(newT1$task, newT1$decision), correct=F)
chi2 = chisq.test(table(dt[dt$workerid==1,]$task, dt[dt$workerid==1,]$decision), correct=F)


for (i in unique(dt$workerid)){
  chi2 = chisq.test(table(dt[dt$workerid==i,]$task, dt[dt$workerid==i,]$decision), correct=F)
  x[i] <- sqrt(chi2$statistic / sum(table(dt[dt$workerid==i,]$task, dt[dt$workerid==i,]$decision)))
}

x[1]
x[2]


library(psych)
x = table(dt[dt$workerid==1,]$task, dt[dt$workerid==1,]$decision)
x = table(newT1$task, newT1$decision)
tetrachoric(x,y=NULL,correct=.5,smooth=TRUE,global=TRUE,weight=NULL,na.rm=TRUE,
            delete=TRUE)


