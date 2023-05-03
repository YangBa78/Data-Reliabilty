dft = read_excel("/Users/yang/Dropbox (ASU)/Code/Data-Reliabilty/Data/df_times.xlsx")
View(dft)



colnames(dft)

p = glmer(as.factor(Human_pred) ~ (1 |Worker_id ) + (1 | Task_id) + (1 |Times ) + (1 |Worker_id: Times ) + (1 |Task_id: Times ) + (1 |Worker_id: Task_id),      
          data = df1, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)


ranef(p)$Times

View(ranef(p)$Worker_id)
View(ranef(p)$`Worker_id:Times`)
View(ranef(p)$`Worker_id:Task_id`)


table(dft[dft$Worker_id =='A28TUBRN9GF443', ]$Times, dft[dft$Worker_id =='A28TUBRN9GF443', ]$Human_pred)


table(dft[dft$Worker_id =='A10LNP2RHQTC1M', ]$Times, dft[dft$Worker_id =='A10LNP2RHQTC1M', ]$Human_pred)


table(dft[dft$Worker_id =='A14T33SPTGBI0U', ]$Times, dft[dft$Worker_id =='A14T33SPTGBI0U', ]$Human_pred)


table(dft$Times, dft$Human_pred)

library(vcd)
ftable(mytable)

dim(dft)
df1 = dft[(dft$Times==1) | (dft$Times==2),]
