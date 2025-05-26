library(tidyverse)

########################################### MTurk 
mt <- read.csv("Data/MTurkPerformanceData.csv")

colnames(mt)

mt = mt[,c('Assignment.ID','Completion.Time','Final.Decision', 'Coded.Prediction', 'Task.difficulty', 'TID')]
colnames(mt)[1] = 'workerid'
colnames(mt)[2] = 'time'
colnames(mt)[3] = 'decision'
colnames(mt)[4] = 'accuracy'
colnames(mt)[5] = 'task'

colnames(mt)[6] = 'image'
#mt
mt1 = mt[, c('workerid', 'decision', 'image')]
head(mt1)

mt_pivot <- mt1 %>%
  pivot_wider(names_from = workerid, values_from = decision, names_prefix = "worker_")
dim(mt_pivot)
head(mt_pivot)

mt_matrix <- mt_pivot[, -1] 
fl.kappa.mt <- kappam.fleiss(mt_matrix)
print(fl.kappa.mt)

sum(is.na(mt_matrix)) #0 
# Fleiss' Kappa for m Raters
# 
#  Subjects = 77 
#    Raters = 160 
#     Kappa = 0.435 
# 
#         z = 430 
#   p-value = 0 

############################################################ Prolific
pl <- read_excel("Data/Prolific.xlsx")
pl = pl[pl['WorkerId']!='2f33f23f23',]

colnames(pl)

pl = pl[,c('WorkerId','Time Spend','Final Decision', 'error', 'task', 'Image 1')]
colnames(pl)[1] = 'workerid'
colnames(pl)[2] = 'time'
colnames(pl)[3] = 'decision'
colnames(pl)[6] = 'image'

pl1 = pl[, c('workerid', 'decision', 'image')]
pl_pivot <- pl1 %>%
  pivot_wider(names_from = workerid, values_from = decision, names_prefix = "worker_")
dim(pl_pivot)

head(pl_pivot)

pl_matrix <- pl_pivot[, -1] 
fl.kappa.pl <- kappam.fleiss(pl_matrix)
print(fl.kappa.pl)   
# Fleiss' Kappa for m Raters
# 
#  Subjects = 2 
#    Raters = 139 
#     Kappa = 0.00374 
# 
#         z = 0.518 
#   p-value = 0.604 


sum(is.na(pl_matrix)) #70 

# remove na columns
pl_matrix_na  <- pl_matrix [, colSums(is.na(pl_matrix)) == 0]
dim(pl_matrix_na)
sum(is.na(pl_matrix_na))

fl.kappa.pl_na <- kappam.fleiss(pl_matrix_na)
print(fl.kappa.pl_na)

# Fleiss' Kappa for m Raters
# 
#  Subjects = 72 
#    Raters = 138 
#     Kappa = 0.638 
# 
#         z = 526 
#   p-value = 0 


#############################airport data
fw = read.csv("DF_Facewise_Airports1.csv")

fw = fw[,c('ID','Time.Spend','Final.Decision', 'Easyness', 'Pair')]
colnames(fw)[1] = 'workerid'
colnames(fw)[2] = 'time'
colnames(fw)[3] = 'decision'
colnames(fw)[4] = 'task'
colnames(fw)[5] = 'image'

p = glmer(as.factor(decision) ~ (1 | workerid ) + (1 | image),      
          data = fw, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)
p.t = sum(data.frame(VarCorr(p))$vcov)
aii = data.frame(VarCorr(p))$vcov[1] / p.t
aii #  0.0789591



fw1 = fw[, c('workerid', 'decision', 'image')]
fw_pivot <- fw1 %>%
  pivot_wider(names_from = workerid, values_from = decision, names_prefix = "worker_")
dim(fw_pivot)

head(fw_pivot)

fw_matrix <- fw_pivot[, -1] 
fl.kappa.fw <- kappam.fleiss(fw_matrix)
print(fl.kappa.fw)  
# Fleiss' Kappa for m Raters
# 
#  Subjects = 79 
#    Raters = 152 
#     Kappa = 0.426 
# 
#         z = 405 
#   p-value = 0 

sum(is.na(fw_matrix)) #1

######################################## icc
fw_matrix_numeric <- fw_matrix %>%
  mutate(across(everything(), ~ ifelse(. == "Same", 0, 1)))

mt_matrix_numeric <- mt_matrix %>%
  mutate(across(everything(), ~ ifelse(. == "same", 0, 1)))

pl_matrix_numeric <- pl_matrix %>%
  mutate(across(everything(), ~ ifelse(. == "same", 0, 1)))

pl_matrix_na_numeric <- pl_matrix_na %>%
  mutate(across(everything(), ~ ifelse(. == "same", 0, 1)))


icc(fw_matrix_numeric,  model = "twoway", type = "consistency", unit = "single") # 0.441
icc(fw_matrix_numeric,  model = "twoway", type = "agreement", unit = "single") # 0.429
icc(fw_matrix_numeric) # 0.429

icc(mt_matrix_numeric, model = "twoway", type = "consistency", unit = "single") #0.467
icc(mt_matrix_numeric, model = "twoway", type = "agreement", unit = "single") #0.438
icc(mt_matrix_numeric) #0.438

icc(pl_matrix_numeric, model = "twoway", type = "consistency", unit = "single")
icc(pl_matrix_numeric, model = "twoway", type = "agreement", unit = "single")
icc(pl_matrix_numeric) 

# rm NA 
icc(pl_matrix_na_numeric, model = "twoway", type = "consistency", unit = "single") #0.655
icc(pl_matrix_na_numeric, model = "twoway", type = "agreement", unit = "single") #0.641
icc(pl_matrix_na_numeric) #0.641


######### 3.29 
p = glmer(as.factor(decision) ~ (1 | workerid ) + (1 | image),      
          data = fw, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)

p.t = sum(data.frame(VarCorr(p))$vcov)
icc_fw = data.frame(VarCorr(p))$vcov[1] / (p.t + 3.29)
icc_fw #  0.04826116  vs 0.0789591


############ mt 
p = glmer(as.factor(decision) ~ (1 | workerid ) + (1 | image) + (1 | workerid : image),      
          data = mt, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)


# Spammer Index
p.t = sum(data.frame(VarCorr(p))$vcov)
icc_mt = data.frame(VarCorr(p))$vcov[2] / (p.t + 3.29)
icc_mt # 0.1108496  vs 0.1663119

# # Spammer Index
p = glmer(as.factor(decision) ~ (1 | workerid ) + (1 | image),      
          data = pl, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)

p.t = sum(data.frame(VarCorr(p))$vcov)
icc_pl = data.frame(VarCorr(p))$vcov[1] / (p.t + 3.29)
icc_pl  #Spammer Index  0.05168359  vs 0.06448514










