library(simstudy)
#library(markovchain)
#library(philentropy)
#library(ggplot2)


################## define tasks 
# variance
var_image <- 6

n_image <- 80

set.seed(123)
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
dtImage$trueLable <- ifelse(dtImage$image_eff > 0, 1, 0)
dtImage

table(dtImage$trueLable)


############ random guess########## p = 0.5
sample_random = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  for (x in 1:nworker){
    d1 = dtImage
    d1$worker_eff = runif(1, -0.5, 0.5)
    d1$int_eff = 0 - (d1$image_eff + d1$worker_eff)
    d1$re = d1$image_eff + d1$worker_eff +d1$int_eff

    d1$p = 1/(1+(exp(-d1$re)))
    
    for (i in 1:n_image){
      d1$decision[i] = rbinom(1, 1, prob = d1$p[i])
    }
    
    d1$correct = ifelse(d1$decision == d1$trueLable, 1, 0)
    df = rbind(df, d1)
  }
  return(df)
}

table(sample_random(1)$correct)


############# primary choice ###############
sample_primary1 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  for (x in 1:nworker){
    d2 = dtImage
    d2$int_eff = runif(1, -0.3, 0.3)
    
    #d2$worker_eff = sample(c(1.4, -1.4), 1) + runif(1, -0.3, 0.3) - (d2$image_eff + d2$int_eff)
    d2$worker_eff = 1.4 + runif(1, -0.3, 0.3) - (d2$image_eff + d2$int_eff)
    d2$re = d2$image_eff + d2$worker_eff +d2$int_eff
    
    
    d2$p = 1/(1+(exp(-d2$re)))
    
    for (i in 1:n_image){
      d2$decision[i] = rbinom(1, 1, prob = d2$p[i])
    }
    
    d2$correct = ifelse(d2$decision == d2$trueLable, 1, 0)
    
    # check the longest run
    run = sort(rle(d2$decision)[1]$lengths, decreasing =TRUE)[1]
    if (run>=10){
      df = rbind(df, d2)
    }
  }
  return(df)
}


############# primary choice 2###############
sample_primary2 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  for (x in 1:nworker){
    d2 = dtImage
    d2$int_eff = runif(1, -0.3, 0.3)
    
    #d2$worker_eff = sample(c(1.4, -1.4), 1) + runif(1, -0.3, 0.3) - (d2$image_eff + d2$int_eff)
    d2$worker_eff = -1.4 + runif(1, -0.3, 0.3) - (d2$image_eff + d2$int_eff)
    d2$re = d2$image_eff + d2$worker_eff +d2$int_eff
    
    
    d2$p = 1/(1+(exp(-d2$re)))
    
    for (i in 1:n_image){
      d2$decision[i] = rbinom(1, 1, prob = d2$p[i])
    }
    
    d2$correct = ifelse(d2$decision == d2$trueLable, 1, 0)
    
    # check the longest run
    run = sort(rle(d2$decision)[1]$lengths, decreasing =TRUE)[1]
    if (run>=10){
      df = rbind(df, d2)
    }
  }
  return(df)
}

############ strong pattern ###############
sample_strong_pattern1 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  
  for (x in 1:nworker){
    d4 = dtImage
    d4$worker_eff = runif(1, -0.5, 0.5)
    
    #if (sample(c(0, 1), 1)==0){
    #  a = c(-2, 2)
    #}else{
    #  a = c(2, -2)
    #}
    
    for (i in 1:n_image){
      if (i%%2==0){
        d4$int_eff[i] = 2 + runif(1, -0.1, 0.1) - (d4$image_eff[i] + d4$worker_eff[i])
      }else{
        d4$int_eff[i] = -2 + runif(1, -0.1, 0.1) - (d4$image_eff[i] + d4$worker_eff[i])
      }
    }
    d4$re = d4$image_eff + d4$worker_eff +d4$int_eff
    
    
    d4$p = 1/(1+(exp(-d4$re)))
    
    for (j in 1:n_image){
      d4$decision[j] = rbinom(1, 1, prob = d4$p[j])
    }
    
    d4$correct = ifelse(d4$decision == d4$trueLable, 1, 0)
    df = rbind(df, d4)
  }
  return(df)
}


sample_strong_pattern2 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  
  for (x in 1:nworker){
    d4 = dtImage
    d4$worker_eff = runif(1, -0.5, 0.5)
    
    #if (sample(c(0, 1), 1)==0){
    #  a = c(-2, 2)
    #}else{
    #  a = c(2, -2)
    #}
    
    for (i in 1:n_image){
      if (i%%2==0){
        d4$int_eff[i] = -2 + runif(1, -0.1, 0.1) - (d4$image_eff[i] + d4$worker_eff[i])
      }else{
        d4$int_eff[i] = 2 + runif(1, -0.1, 0.1) - (d4$image_eff[i] + d4$worker_eff[i])
      }
    }
    d4$re = d4$image_eff + d4$worker_eff +d4$int_eff
    
    
    d4$p = 1/(1+(exp(-d4$re)))
    
    for (j in 1:n_image){
      d4$decision[j] = rbinom(1, 1, prob = d4$p[j])
    }
    
    d4$correct = ifelse(d4$decision == d4$trueLable, 1, 0)
    df = rbind(df, d4)
  }
  return(df)
}

sample_strong_pattern2(1)


############ weak pattern ###############
sample_weak_pattern1 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  
  for (x in 1:nworker){
    d4 = dtImage
    d4$worker_eff = runif(1, -0.5, 0.5)
    
    #if (sample(c(0, 1), 1)==0){
    #  a = c(-1.5, 1.5)
    #}else{
    #  a = c(1.5, -1.5)
    #}
    
    for (i in 1:n_image){
      if (i%%2==0){
        d4$int_eff[i] = 1.5 + runif(1, -0.1, 0.1) - (d4$image_eff[i] + d4$worker_eff[i])
      }else{
        d4$int_eff[i] = -1.5 + runif(1, -0.1, 0.1) - (d4$image_eff[i] + d4$worker_eff[i])
      }
    }
    d4$re = d4$image_eff + d4$worker_eff +d4$int_eff
    
    
    d4$p = 1/(1+(exp(-d4$re)))
    
    for (j in 1:n_image){
      d4$decision[j] = rbinom(1, 1, prob = d4$p[j])
    }
    
    d4$correct = ifelse(d4$decision == d4$trueLable, 1, 0)
    df = rbind(df, d4)
  }
  return(df)
}


sample_weak_pattern2 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  
  for (x in 1:nworker){
    d4 = dtImage
    d4$worker_eff = runif(1, -0.5, 0.5)
    
    #if (sample(c(0, 1), 1)==0){
    #  a = c(-1.5, 1.5)
    #}else{
    #  a = c(1.5, -1.5)
    #}
    
    for (i in 1:n_image){
      if (i%%2==0){
        d4$int_eff[i] = -1.5 + runif(1, -0.1, 0.1) - (d4$image_eff[i] + d4$worker_eff[i])
      }else{
        d4$int_eff[i] = 1.5 + runif(1, -0.1, 0.1) - (d4$image_eff[i] + d4$worker_eff[i])
      }
    }
    d4$re = d4$image_eff + d4$worker_eff +d4$int_eff
    
    
    d4$p = 1/(1+(exp(-d4$re)))
    
    for (j in 1:n_image){
      d4$decision[j] = rbinom(1, 1, prob = d4$p[j])
    }
    
    d4$correct = ifelse(d4$decision == d4$trueLable, 1, 0)
    df = rbind(df, d4)
  }
  return(df)
}


############ normal worker ################
sample_normal = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  
  for(t in 1:nworker){
    d6 = dtImage
    d6$worker_eff = runif(1, -1.5, 1.5)
    for (i in 1:n_image){
      d6$int_eff[i] = runif(1, -1.5, 1.5) 
    }
    
    d6$re = d6$image_eff + d6$worker_eff + d6$int_eff
    
    d6$p = 1/(1+(exp(-d6$re)))
    
    for (i in 1:n_image){
      #d6$decision[i] = rbinom(1, 1, prob = d6$p[i])
      d6$decision[i] <- ifelse(d6$re[i] > 0, 1, 0)
    }
    
    d6$correct = ifelse(d6$decision == d6$trueLable, 1, 0)
    df = rbind(df, d6)
  }
  return(df)
}


######## data simulation #######



