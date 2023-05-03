# variance
var_image <- 3
var_worker <- 9
var_time <- 
# aii <- 0.5/(0.5+6)
# aii

n_worker <- 60
n_image <-50

#set.seed(123)
# define image task
def.image <- defData(varname = "image_eff", dist = "normal", formula = 0, variance = var_image,
                     id = "imageid")
def.image <- defData(def.image, varname = "nWorker", dist = "nonrandom", formula = n_worker)

# generate task data
dtImage <- genData(n_image, def.image)

# assign task difficulty and true label
dtImage$task <- ifelse(abs(dtImage$image_eff)< median(abs(dtImage$image_eff)), 'hard', 'easy')
# different: 1, same: 0
dtImage$trueLable <- ifelse(dtImage$image_eff > median(dtImage$image_eff), 1, 0)

# define workerid
dtTime <- addPeriods(dtImage,
                     nPeriods = n_worker, idvars = "imageid",
                     timevarName = "t")
dtTime$workerid <- rep(1:n_worker, n_image)
dtTime <- dtTime[, c('imageid', 'image_eff', 'trueLable', 'task', 'workerid')]

# generate worker data
def.worker <- defData(varname = "worker_eff", dist = "normal", formula = 0, variance = var_worker, 
                      id = "workerid")
dtWorker <- genData(n_worker, def.worker)

# combine task and worker data
dd <- dtTime%>% left_join(dtWorker, by = "workerid")

# add interaction( for random guess data, strong pattern data doesn't include this)
# define sd range
#int_sd = sample(runif(160, 0.01, 0.1))
#int_eff = c()

# for each worker i int_eff: (worker_i: easy, worker_i: hard) should be mean: 0, sd: random(range(int_sd))
#for(x in int_sd){
# int_eff <- append(int_eff, rnorm(n = 2, mean = 0, sd = x)) 
#}

for (i in int_eff){
  if (abs(i)<0.6 & abs(i+1)<0.6){
    print(i)
  }
}


for (i in 1:320){
  if (acc_task[i, 3]<0.65){
    print(i)
  }
}


interact = data.frame( 'workerid' = rep(1:n_worker, each = 2) ,
                       'task' = c('easy', 'hard'),
                       'int_eff' = int_eff)
# check int_eff: 
# sd(int_eff)
# mean(int_eff)
#dd <- dd %>% left_join(interact, by = c("workerid", "task"))

# response variable
def.d <- defDataAdd(varname = "decision", formula = "image_eff + worker_eff", 
                    dist="binary", link = "logit")
dt <- addColumns(def.d, dd)

