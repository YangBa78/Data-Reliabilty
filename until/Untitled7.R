library(simstudy)

starts <- "rep(c(2 : 5), each = 10)"

starts <- "rep(2:5, 10)"

siteDef <- defData(varname = "bj", dist = "normal", formula = 0, 
                   variance = .01, id="site")
siteDef <- defData(siteDef, varname = "sj", dist = "nonrandom", 
                   formula = starts)
siteDef <- defData(siteDef, varname = "ips", dist = "nonrandom", 
                   formula = 100)

indDef <- defDataAdd(varname = "bi", dist = "normal", formula = 0,
                     variance = 0.01)

trtDef <- defDataAdd(varname = "Ijt" , 
                     formula = "as.numeric(period >= sj)", 
                     dist = "nonrandom")

f = "0.8 + .01 * period + 0.8 * Ijt + 0.001 * Ijt * (period-sj) + bi + bj"
trtDef <- defDataAdd(trtDef, varname = "Yijt", formula = f, 
                     dist = "binary", link = "logit")

dtSite 
dtSite <- genData(40, siteDef)
dtSite <- genCluster(dtSite, cLevelVar = "site", numIndsVar = "ips", 
                     level1ID = "id")
dtSite <- addColumns(indDef, dtSite)

dtSiteTm <- addPeriods(dtSite, nPeriods = 7, idvars = "id")
dtSiteTm <- addColumns(trtDef, dtSiteTm)

dtSiteTm
dtSiteTm$site[dtSiteTm$bj==0.06799]



library(simstudy)

### Group defintion

defg <- defData(varname = "gamma", formula=0, variance = 2, id = "cid")

### Individal definition

defi <- defDataAdd(varname = "alpha", formula = 0, variance = 1.3)

### Outcome definition

defC <- defCondition(condition = "period == 0", 
                     formula = "3 + gamma + alpha",
                     dist = "nonrandom")
defC <- defCondition(defC, condition = "period == 1", 
                     formula = "4 + gamma + alpha",
                     dist = "nonrandom")
defC <- defCondition(defC, condition = "period == 2", 
                     formula = "6 + gamma + alpha",
                     dist = "nonrandom")

defy <- defDataAdd(varname = "y", formula = "mu", variance = 1.1)


dgrp1 <- genData(100, defg)

dind1 <- genCluster(dgrp1, "cid", numIndsVar = 20, level1ID = "id")
dind1 <- addColumns(defi, dind1)

dper1 <- addPeriods(dind1, nPeriods = 3, idvars = "id")
dper1 <- addCondition(defC, dper1, newvar = "mu")

dper1 <- addColumns(defy, dper1)



defc <- defData( varname = "ceffect", formula = 0.0, variance = 0.15, 
                 dist = "normal", id = "cluster")
defc <- defData(defc, varname = "m", formula = 15, dist = "nonrandom")

defa <- defDataAdd(varname = "Y", 
                   formula = "0 + 0.10  * period + 1 * rx + ceffect", 
                   variance = 2, dist = "normal")

genDD <- function(defc, defa, nclust, nperiods, waves, len, start) {
  dc <- genData(nclust, defc)
  dp <- addPeriods(dc, nperiods, "cluster")
  dp <- trtStepWedge(dp, "cluster", nWaves = waves, lenWaves = len, 
                     startPer = start)
  
  dd <- genCluster(dp, cLevelVar = "timeID", numIndsVar = "m", 
                   level1ID = "id")
  dd <- addColumns(defa, dd)
  dd[]
}

set.seed(2822)
dx <- genDD(defc, defa, 60, 7, 4, 1, 2)

dx

lmerfit <- lmer(Y ~ period + rx + (1 | cluster) , data = dx)



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
                      numIndsVar = 3, level1ID = "idClass"
)
dtClass
