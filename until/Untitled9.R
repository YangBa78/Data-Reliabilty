#task
def1 <- defData(varname = "task", dist = "binary", formula = 0.5)
def1 <- defData(def1, varname = "nImage", dist = "nonrandom", formula = 15)

set.seed(1)
d <- genData(2, def1)
d
d$task<- ifelse(d$task==1, 'hard', 'easy')

# add image
def.image <- defData(varname = "s0", dist = "normal", formula = 0, variance = 10,
                     id = "imageid")

def.image <- defData(def.image, varname = "nWorker", dist = "nonrandom", formula = 10)


#dtSchool <- trtAssign(dtSchool, n = 2)

d <- genData(15, def.image)
d

dtI <- genCluster(d, "id", numIndsVar = "nImage", level1ID = "idImg")
dtI
dt.image <- addColumns(def.image, dtI)
dt.image

def.worker <- defData(varname = "c0", dist = "normal", formula = 0, variance = 2, 
                      id = "workerid")

dtW <- genCluster(dt.image, "idImg", numIndsVar = "nWorker", level1ID = "idWkr")
dtW <- genCluster(d, "imageid", numIndsVar = "nWorker", level1ID = "idWkr")
dtW

dtTime <- addPeriods(d,
                     nPeriods = 3, idvars = "imageid",
                      timevarName = "Y"
)%>% l_join(dtWorker, by = "imageid")

colnames(dtTime)[2] = 'idWkr'


a = dtTime%>% left_join(dt.worker, by = "idWkr")
a

dtTime
daa <- addColumns(a, dtW)

dt.worker <- addColumns(def.worker, dtW)
head(dt.worker, 11)
length(unique(dt.worker$c0))


da <- defDataAdd(varname = "decision", formula = "-0.5 + s0.x + c0", 
                 dist="binary", link = "logit")

dt <- addColumns(da, a)


x <- seq(-4, 4, length.out = 100)
p <- 1/(1 + exp(-x))
plot(x, p, type = "l")
