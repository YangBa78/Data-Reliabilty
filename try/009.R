# variance
sample = 10^5

for (x in 1:sample){
  var_image <- 6
  var_worker <- 3
  
  n_worker <- 160
  n_image <-80
  
  def.image <- defData(varname = "image_eff", dist = "normal", formula = 0, variance = var_image,
                       id = "imageid")
  def.image <- defData(def.image, varname = "nWorker", dist = "nonrandom", formula = n_worker)

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
  
  
  dd$RE <- dd$image_eff + dd$worker_eff
  #dd$logre <- plogis(dd$RE)

  # response variable
  def.d <- defDataAdd(varname = "decision", formula = "RE", 
                      dist="binary", link = "logit")
  dt <- addColumns(def.d, dd)
  
  a = c()
  for (i in 1: n_worker){
    a[i] = max(rle(dt[dt$workerid==i,]$decision)$lengths)
   }
    if (min(a) == 1){
      break
    }
  print(x)
}





for (i in 1:160){
  if (as.numeric(table(rle(dt[dt$workerid==i,]$decision)$lengths))[1]==n_image){
    print(i)
  }
}




table(dt$workerid, dt$decision)
as.numeric(table(rle(dt[dt$workerid==63,]$decision)$lengths))[1]

dt[dt$workerid==45,]$decision











