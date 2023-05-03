library(lavaan)
library(readxl)
library(semPlot)


data<- read.csv("~/Dropbox (ASU)/Facewise/SEM_Qualtric.csv")
head(data)
dim(data)
low = data[data$Condition=='Low',]
high = data[data$Condition=='High',] 



M1 = '
# regression 
  #Sourcing =~ Sourcing_item1 + Sourcing_item2 + Sourcing_item3 + Sourcing_item4 +
   #          Sourcing_item5 + Sourcing_item6 + Sourcing_item7 + Sourcing_item8 
    #         Sourcing_item9 + Sourcing_item10
# factor loadings
  distrust =~ Jian_item1 + Jian_item2 + Jian_item3 + Jian_item4 + Jian_item5
  trust =~ Jian_item6 + Jian_item7 + Jian_item8 + Jian_item9 + Jian_item10 + Jian_item11 + Jian_item12
  performance =~ Chancey_item1 + Chancey_item2 + Chancey_item3 + Chancey_item4 + Chancey_item5
  process =~ Chancey_item6 + Chancey_item7 + Chancey_item8 + Chancey_item9 + Chancey_item10
  purpose =~ Chancey_item11 + Chancey_item12 + Chancey_item13 + Chancey_item14 + Chancey_item15

#  Mean and Variance of the Factor
#    purpose ~ 0*1
#    purpose ~~ 1*purpose
#    process ~ 0*1
#    process ~~ 1*process
#    performance ~ 0*1
#    performance ~~ 1*performance
'


# estimator="WLS",
# group = 'Condition',
#parameterization = 'theta'

fit = cfa(M1, high, ordered = c('Jian_item1', 'Jian_item2', 'Jian_item3', 'Jian_item4', 'Jian_item5',
                                'Jian_item6', 'Jian_item7', 'Jian_item8', 'Jian_item9', 'Jian_item10',
                                'Jian_item11', 'Jian_item12', 'Chancey_item1', 'Chancey_item2', 'Chancey_item3',
                                'Chancey_item4', 'Chancey_item5', 'Chancey_item6', 'Chancey_item7', 'Chancey_item8',
                                'Chancey_item9', 'Chancey_item10','Chancey_item11', 'Chancey_item12', 'Chancey_item13',
                                'Chancey_item14', 'Chancey_item15'))

summary(fit, fit.measures=TRUE, standardized=TRUE)


M2 ='
     lv1 =~ Scourcing + Logic + Visualization
     lv2 =~ Uncertainty + Change + Accuracy
     lv3 =~ Distinguishing + Alternative + Relevance 
'

fit2 = cfa(M2, data, ordered = c('Scourcing', 'Logic', 'Visualization',
                                  'Uncertainty', 'Change', 'Accuracy',
                                  'Distinguishing', 'Alternative', 'Relevance'))

summary(fit2, fit.measures=TRUE, standardized=TRUE)


M3 ='
     lv1 =~ Scourcing + Logic + Visualization
     lv2 =~ Uncertainty + Change + Accuracy
     lv3 =~ Distinguishing + Alternative + Relevance
     distrust =~ Jian_item1 + Jian_item2 + Jian_item3 + Jian_item4 + Jian_item5
     trust =~ Jian_item6 + Jian_item7 + Jian_item8 + Jian_item9 + Jian_item10 + Jian_item11 + Jian_item12
     performance =~ Chancey_item1 + Chancey_item2 + Chancey_item3 + Chancey_item4 + Chancey_item5
     process =~ Chancey_item6 + Chancey_item7 + Chancey_item8 + Chancey_item9 + Chancey_item10
     purpose =~ Chancey_item11 + Chancey_item12 + Chancey_item13 + Chancey_item14 + Chancey_item15
    
    
    #regression
    trust ~ lv1 + lv2 + lv3 + performance + process + purpose
    distrust ~ lv1 + lv2 + lv3 + performance + process + purpose
'

fit3 = cfa(M3, data, ordered = c('Scourcing', 'Logic', 'Visualization',
                                 'Uncertainty', 'Change', 'Accuracy',
                                 'Distinguishing', 'Alternative', 'Relevance',
                                 'Jian_item1', 'Jian_item2', 'Jian_item3', 'Jian_item4', 'Jian_item5',
                                 'Jian_item6', 'Jian_item7', 'Jian_item8', 'Jian_item9', 'Jian_item10',
                                 'Jian_item11', 'Jian_item12', 'Chancey_item1', 'Chancey_item2', 'Chancey_item3',
                                 'Chancey_item4', 'Chancey_item5', 'Chancey_item6', 'Chancey_item7', 'Chancey_item8',
                                 'Chancey_item9', 'Chancey_item10','Chancey_item11', 'Chancey_item12', 'Chancey_item13',
                                 'Chancey_item14', 'Chancey_item15'))

summary(fit3, fit.measures=TRUE, standardized=TRUE)


