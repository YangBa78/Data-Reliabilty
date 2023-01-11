install.packages('simstudy')
library(simstudy)
library(ggplot2)


NPER <- 25
perDef <- defDataAdd(varname = "npatient",  formula = 20,
                     dist = "poisson")

patDef <- defDataAdd(varname = "S0", formula = "0.4;0.4;0.2",
                     dist = "categorical")

P <- t(matrix(c( 0.7, 0.2, 0.1, 0.0,
                 0.1, 0.3, 0.4, 0.2,
                 0.0, 0.1, 0.5, 0.4,
                 0.0, 0.0, 0.0, 1.0),
              nrow = 4))

set.seed(3837263)

dsite <- genData(5, id = "site")

dper <- addPeriods(dsite, nPeriods = NPER, idvars = "site", 
                   timeid = "site.time", perName = "period")
dper <- addColumns(perDef, dper)

dper


defc <- defData(varname = "ceffect", formula = 10, variance = 0.4, 
                dist = "normal", id = "cid")

defi <- defDataAdd(varname = "male", formula = .4, dist = "binary")
defi <- defDataAdd(defi, varname = "age", formula = 0, variance = 40)
defi <- defDataAdd(defi, varname = "bmi", formula = 0, variance = 5)

defr <- defDataAdd(varname = "y", 
                   formula = "-1 + 0.08*bmi - 0.3*male - 0.08*age + 0.45*rx + ceffect", 
                   dist = "binary", link = "logit")


dc <- genData(20, defc)

di <- genCluster(dc, "cid", 60, "id")
di <- addColumns(defi, di)

help(simstudy)
library(parallel)
library(Matching)
library(data.table)

RNGkind("L'Ecuyer-CMRG")  # to set seed for parallel process

### See addendum for dmatch code

dd <- rbindlist(mclapply(1:nrow(dc), 
                         function(x) dmatch(di[cid == x]),
                         mc.set.seed = TRUE
                        )
                )

### generate outcome

dd <- addColumns(defr, dd)
setkey(dd, pair)
dd




def <- defData(varname="x", formula = 10, variance = 2, dist = "normal")
def <- defData(def, varname="y", formula = "3 + 0.5 * x", variance = 1, dist = "normal")
dd <- genData(250, def)

dd <- trtAssign(dd, nTrt = 4, grpName = "grp", balanced = TRUE)

dd


def <- defRepeat(nVars = 4, prefix = "g", formula = "1/3;1/3;1/3",
                 variance = 0, dist = "categorical")
def <- defData(def, varname = "a", formula = "1;2", dist = "trtAssign")
def <- defRepeat(def, 3, "b", formula = "5 + a", variance = 3,
                 dist = "normal")
def <- defData(def, "y", formula = "0.10", dist = "binary")
def

dd <- genData(1000, def)
dd
var(dd$a)
hist(dd$b3)
sum(dd$a)
dd$a



gen.image <- defData(varname = "s0", dist = "normal", formula = 0, variance = 10,
                      id = "idImage")
#gen.image <- defData(gen.image, varname = "nClasses", formula = 5)

dtImage <- genData(30, gen.image)
dtImage <- trtAssign(dtImage, n = 2)
dtImage

sum(dtImage$trtGrp)
sum(dtImage$s0[dtImage$trtGrp==1]^2)
sum(dtImage$s0[dtImage$trtGrp==0]^2)



gen.school <- defData(varname = "s0", dist = "normal", formula = 0, variance = 3,
                      id = "idSchool")

gen.school <- defData(varname = "s0", dist = "normal", formula = 0, variance = 3,
                      id = "idSchool")
gen.school <- defData(gen.school, varname = "nClasses", formula = 5)

#set.seed(282721)




dtSchool <- genData(8, gen.school)
dtSchool <- trtAssign(dtSchool, n = 2)

dtSchool

gen.class <- defDataAdd(varname = "c0", dist = "normal", formula = 0, variance = 2)
gen.class <- defDataAdd(gen.class, varname = "nStudents",
                        formula = 20)

dtClass <- genCluster(dtSchool, "idSchool", numIndsVar = "nClasses", level1ID = "idClass")
dtClass <- addColumns(gen.class, dtClass)
dtClass

head(dtClass, 10)

gen.student <- defDataAdd(varname = "Male", dist = "binary",
                          formula = 0.5)
gen.student <- defDataAdd(gen.student, varname = "age", dist = "uniform",
                          formula = "9.5; 10.5")
gen.student <- defDataAdd(gen.student, varname = "test", dist = "binary", link = 'logit',
                          formula = "0.5 - 0.5*Male + s0 + c0 + 0.2 * trtGrp")
dtStudent <- genCluster(dtClass, cLevelVar = "idClass", numIndsVar = "nStudents",
                        level1ID = "idChild")

dtStudent <- addColumns(gen.student, dtStudent)
dtStudent

gen.add <- defDataAdd(gen.student, varname = "test", dist = "binary", link = 'logit',
                          formula = "0.5 - 0.5*Male + s0 + c0 + 0.2 * trtGrp")

starts <- "rep(1:20, 40)"
gen.add <- defData(varname = "studentID", dist = "nonrandom", 
                   formula = starts)

dtStudent <- addColumns(gen.add, dtStudent)
dtStudent
head(dtStudent,21)


library(lme4)

fit = lmer(test ~ Male + trtGrp + (1 | idSchool:idClass), data = dtStudent)
fit



dtStudent$idClass[dtStudent$idSchool==7]
unique(dtStudent$test)



d <- defData(varname = "x", formula = 0, variance = 9, dist = "normal")

dc <- defCondition(condition = "x <= -2", formula = "4 + 3*x",
                   variance = 2, dist = "normal")
dc <- defCondition(dc, condition = "x > -2 & x <= 2", formula = "0 + 1*x",
                   variance = 4, dist = "normal")
dc <- defCondition(dc, condition = "x > 2", formula = "-5 + 4*x",
                   variance = 3, dist = "normal")

dd <- genData(1000, d)
dd <- addCondition(dc, dd, newvar = "y")
dd
