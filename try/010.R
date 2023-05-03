sample = 10^4

for (x in 1:sample){
  nworker= 160 # number of pupils per school
  nimage = 80 # number of schools
  #npupilstotal = npupils*nschools # to give each pupil a unique number
  
  # 1.1 Design data frame:
  simdf = data.frame("workerid" = rep(1:nworker,
                                      each = nimage),
                     "imageid" = rep(1:nimage,
                                     nworker))
  # task
  easy <- rep("easy", nimage/2)
  hard <- rep("hard", nimage/2)
  sample_task <- c(easy, hard)
  simdf$task <- sample(sample_task)
  
  simdf
  
  # GT 
  same <- rep("same", nimage/4)
  diff <- rep("different", nimage/4)
  sample_vec <- c(same, diff)
  sample_same <- sample(sample_vec)
  sample_diff <- sample(sample_vec)
  
  simdf$trueLable[simdf$task=='easy'] <- sample_same
  simdf$trueLable[simdf$task=='hard'] <- sample_diff
  
  df = simdf 
  
  df$decision <- NA
  df[df$workerid==1,]$decision <- rbinom(nimage,1,0.001)
  df[df$workerid==2,]$decision <- rbinom(nimage,1,1-0.001)
  for (i in 3:9) {
    df[df$workerid==i,]$decision <- rbinom(nimage,1,0.5)
  }
  for (i in 10:160){
    p = runif(1, 0.1, 0.8)
    df[df$workerid==i,]$decision <- rbinom(nimage,1,p)
  }
  a = c()
  for (i in 1: n_worker){
    a[i] = max(rle(df[dt$workerid==i,]$decision)$lengths)
  }
  if (min(a) == 1){
    break
  }
  print(x)
}




