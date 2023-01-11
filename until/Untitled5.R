library(simstudy)

# define data

cvar <- iccRE(0.20, dist = "binary")

d <- defData(varname = "a", formula = 0, variance = cvar, 
             dist = "normal", id = "cid")
d <- defData(d, varname = "nper", formula = 100, dist = "nonrandom")

da <- defDataAdd(varname = "y", formula = "-1 + .4*rx + a", 
                 dist="binary", link = "logit")

#set.seed(11265)

dc <- genData(100, d)

di <- genCluster(dc, "cid", "nper", "id")
di <- trtAssign(di, strata = "cid", grpName = "rx")
di <- addColumns(da, di)

di

glmer(y ~ rx + (1 | cid), data = di, family = binomial)

calc <- di[, .(estp = mean(y)), keyby = .(cid, rx)]
calc[, lo := log(odds(estp))]
calc[rx == 1, mean(lo)] - calc[rx == 0, mean(lo)] 


dt <- genData(15)

dt1 <- trtAssign(dt, nTrt = 3, balanced = TRUE)
dt1[, .N, keyby = trtGrp]

dt2 <- trtAssign(dt, nTrt = 3, balanced = FALSE)
dt2[, .N, keyby = trtGrp]

def <- defData(varname = "male", formula = .4, dist = "binary")
dt <- genData(1000, def)
dt

dt3 <- trtAssign(dt, nTrt = 5, strata = "male", balanced = TRUE, grpName = "Group")
dt3
dt3[, .N, keyby = .(male, Group)]
dt3[, .N, keyby = .(Group)]

dt4 <- trtAssign(dt, nTrt = 5, strata = "male", balanced = FALSE, grpName = "Group")
dt4[, .N, keyby = .(male, Group)]
dt4[, .N, keyby = .(Group)]

dt5 <- trtAssign(dt, nTrt = 5, balanced = TRUE, grpName = "Group")
dt5[, .N, keyby = .(male, Group)]
dt5[, .N, keyby = .(Group)]

dt6 <- trtAssign(dt, nTrt = 3, ratio = c(1, 2, 2), grpName = "Group")
dt6[, .N, keyby = .(Group)]

glmer(male ~ (1 | Group), data = dt3, family = binomial)







gen.school <- defData(
  varname = "s0", dist = "normal",
  formula = 0, variance = 3, id = "idSchool"
)
gen.school <- defData(gen.school,
                      varname = "nClasses",
                      dist = "noZeroPoisson", formula = 3
)

dtSchool <- genData(3, gen.school) #'
dtSchool

dtClass <- genCluster(dtSchool,
                      cLevelVar = "idSchool",
                      numIndsVar = "nClasses", level1ID = "idClass"
)
dtClass

dtClass <- genCluster(dtSchool,
                      cLevelVar = "idSchool",
                      numIndsVar = 3, level1ID = "idClass",
                      allLevel2 = FALSE
)
dtClass


targetICC <- seq(0.05, 0.20, by = .01)
iccRE(targetICC, "binary")








