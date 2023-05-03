log_odds = 0.41876318

plogis(log_odds)

logistic(log_odds)



b_0 = 0.059796

exp(b_0)/(1+exp(b_0))

inv.logit(b_0)

logit(b_0)

exp(0)

??logistic

x <- seq(-4, 4, length.out = 100)
p.2 <- exp(x)/(1 + exp(x))
plot(x, p.2, type = "l")


library(simstudy)
def <- defData(varname = "male", dist = "binary", 
               formula = .5 , id="cid")
def <- defData(def, varname = "over65", dist = "binary",  
               formula = "-1.7 + .8*male", link="logit")
def <- defData(def, varname = "rx", dist = "trtAssign",
               formula = "2;1", variance = "male;over65")
def <- defData(def, varname = "y", dist = "normal", 
               formula = "20 + 5*male + 10*over65 + 10*rx", variance = 40)

dtstudy <- genData(330, def)
dtstudy
def
sum(dtstudy$rx)
sum(dtstudy$male)
sum(dtstudy$over65)

# Here are the counts and average outcomes for each gender, age, and treatment combination:
dtstudy[, .(n = .N, avg = round(mean(y), 1)), keyby = .(male, over65, rx)]

formula1 <- c("-2 + 2*male - .5*over65", "-1 + 2*male + .5*over65")
dtExp <- trtObserve(dtstudy, formulas = formula1, logit.link = TRUE, grpName = "exposure")


library(simstudy)
library(ggplot2)

defc <- defData(varname = "ceffect", formula = 0, variance = 0.20, 
                dist = "normal", id = "cluster")
defc <- defData(defc, "m", formula = 15, dist = "nonrandom")

defa <- defDataAdd(varname = "Y", 
                   formula = "0 + ceffect + 0.1*period + trt*1.5", 
                   variance = 1.75, dist = "normal")

set.seed(608477)

dc <- genData(30, defc)
dp <- addPeriods(dc, 24, "cluster", timevarName = "t")
dp <- trtStepWedge(dp, "cluster", nWaves = 5, lenWaves = 4, 
                   startPer = 4, grpName = "trt")

dd <- genCluster(dp, cLevelVar = "timeID", "m", "id")
dd <- addColumns(defa, dd)

dd




