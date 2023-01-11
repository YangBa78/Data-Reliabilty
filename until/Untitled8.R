def.image <- defData(varname = "s0", dist = "normal", formula = 0, variance = 10,
                      id = "imageid")
def.image <- defData(def.image, varname = "nWorker", dist = "nonrandom", formula = 10)

dtImage <- genData(15, def.image)

dtImage$task <- ifelse(abs(dtImage$s0)>median(abs(dtImage$s0)), 'hard', 'easy')
dtImage


#set.seed(123)

def.worker <- defData(varname = "c0", dist = "normal", formula = 0, variance = 2, 
                      id = "workerid")

dtWorker <- genData(10, def.worker)
dtWorker

dtComb <- expand.grid(
  imageid = dtImage$imageid,
  workerid = dtWorker$workerid )%>% 
  left_join(dtImage, by = "imageid") %>% 
  left_join(dtWorker, by = "workerid") 


length(unique(dtComb$s0))

dtComb <- crossing(
  imageid = dtImage$imageid,
  workerid = dtWorker$workerid )%>% 
  left_join(dtImage, by = "imageid") %>% 
  left_join(dtWorker, by = "workerid") 

typeof(dtComb)

dtComb

expand.grid(wokerid=1:n_worker,taskid=1:n_task)

da <- defDataAdd(varname = "decision", formula = "-0.5 + s0 + c0", 
                 dist="binary", link = "logit")

dt <- addColumns(da, dtComb)


dt

test1 <- c(
  "varname,formula,variance,dist,link",
  "nr,7, 0,nonrandom,identity",
  "x1,.4, 0,binary,identity",
  "y1,nr + x1 * 2,8,normal,identity",
  "y2,nr - 0.2 * x1,0,poisson, log"
)


tfcsv <- tempfile()
writeLines(test1, tfcsv)

# Read external csv file stored in file "tfcsv"

defs <- defRead(dtComb)
defs

unlink(tfcsv)

# Generate data based on external definition

genData(5, defs)



p = 1 / (1 + exp(-(-4.5 + 1.8342 + 1.2518)))
log_odds = log(p / (1 - p))
y = rbinom(1, 1, p)
y
p







dtClass <- genCluster(dd,
                      cLevelVar = "idImage",
                      numIndsVar = "nWorker", level1ID = "id"
)

dtClass  
dtClass <- addColumns(gen.worker, dtClass)



ddd
defC <- defCondition(dd, condition = "", 
                     formula = "3 + gamma + alpha",
                     dist = "nonrandom")


length(unique(ddd$c0))



library(tidyverse)
set.seed(1)
sample_size <- 300
dat <- tibble(
  x1 = runif(sample_size, min = -1, max = 1),
  x2 = rbinom(sample_size, 1, 0.5),
  e = rnorm(sample_size, sd = 0.1),
  p = 1 / (1 + exp(-(-2 + 3 * x1^2 + x2 + e))),
  log_odds = log(p / (1 - p)),
  y = rbinom(sample_size, 1, p)
)
x1
dat
