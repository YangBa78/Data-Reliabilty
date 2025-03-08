library(simstudy)

# Variance
var_image <- 6
n_image <- 80

set.seed(123)

# Define image task
def.image <- defData(varname = "image_eff", dist = "normal", formula = 0, variance = var_image,
                     id = "imageid")
dtImage <- genData(n_image, def.image)

# Define ordinal categories based on quantiles
category_labels <- c("A", "B", "C", "D", "E")
q <- quantile(dtImage$image_eff, probs = c(0.2, 0.4, 0.6, 0.8))
random_labels <- sample(category_labels) 


dtImage$trueLabel <- cut(
  dtImage$image_eff,
  breaks = c(-Inf, q[1], q[2], q[3], q[4], Inf),
  labels = random_labels, 
  include.lowest = TRUE
)

# Check distribution
table(dtImage$trueLabel)

# View dataset
head(dtImage)


############ random guess########## 
sample_random = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "trueLable", "worker_eff", "int_eff",   
                    "re",  "decision", "correct" )
  n_classes = 5
  
  for (x in 1:nworker){
    d1 = dtImage
    class_probs <- matrix(0.2, nrow = nrow(d1), ncol = n_classes)
    
    d1$decision <- apply(class_probs, 1, function(p) sample(category_labels, 1, prob = p))
    
    d1$correct <- ifelse(d1$decision == d1$trueLabel, 1, 0)
    
    df = rbind(df, d1)
  }
  return(df)
}

# sum(sample_random(1)$correct)


############# primary choice ###############
sample_primary1 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "trueLable", "worker_eff", "int_eff",   
                    "re",  "decision", "correct" )
  n_classes = 5
  
  for (x in 1:nworker){
    d2 = dtImage
    class_probs <- matrix(0.03, nrow = nrow(d2), ncol = n_classes)
    
    # Set all values in the first column to 0.88
    class_probs[, 1] <- 0.88
    
    d2$decision <- apply(class_probs, 1, function(p) sample(category_labels, 1, prob = p))
    
    d2$correct <- ifelse(d2$decision == d2$trueLabel, 1, 0)
    
    df = rbind(df, d2)
  }
  return(df)
}

# sample_primary1(1)

sample_primary2 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "trueLable", "worker_eff", "int_eff",   
                    "re",  "decision", "correct" )
  n_classes = 5
  
  for (x in 1:nworker){
    d2 = dtImage
    class_probs <- matrix(0.03, nrow = nrow(d2), ncol = n_classes)
    
    # Set all values in the first column to 0.88
    class_probs[, 2] <- 0.88
    
    d2$decision <- apply(class_probs, 1, function(p) sample(category_labels, 1, prob = p))
    
    d2$correct <- ifelse(d2$decision == d2$trueLabel, 1, 0)
    
    df = rbind(df, d2)
  }
  return(df)
}

# sample_primary2(1)

sample_primary3 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "trueLable", "worker_eff", "int_eff",   
                    "re",  "decision", "correct" )
  n_classes = 5
  
  for (x in 1:nworker){
    d2 = dtImage
    class_probs <- matrix(0.03, nrow = nrow(d2), ncol = n_classes)
    
    # Set all values in the first column to 0.88
    class_probs[, 3] <- 0.88
    
    d2$decision <- apply(class_probs, 1, function(p) sample(category_labels, 1, prob = p))
    
    d2$correct <- ifelse(d2$decision == d2$trueLabel, 1, 0)
    
    df = rbind(df, d2)
  }
  return(df)
}

# sample_primary3(1)

sample_primary4 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "trueLable", "worker_eff", "int_eff",   
                    "re",  "decision", "correct" )
  n_classes = 5
  
  for (x in 1:nworker){
    d2 = dtImage
    class_probs <- matrix(0.03, nrow = nrow(d2), ncol = n_classes)
    
    # Set all values in the first column to 0.88
    class_probs[, 4] <- 0.88
    
    d2$decision <- apply(class_probs, 1, function(p) sample(category_labels, 1, prob = p))
    
    d2$correct <- ifelse(d2$decision == d2$trueLabel, 1, 0)
    
    df = rbind(df, d2)
  }
  return(df)
}

# sample_primary4(1)

sample_primary5 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "trueLable", "worker_eff", "int_eff",   
                    "re",  "decision", "correct" )
  n_classes = 5
  
  for (x in 1:nworker){
    d2 = dtImage
    class_probs <- matrix(0.03, nrow = nrow(d2), ncol = n_classes)
    
    # Set all values in the first column to 0.88
    class_probs[, 5] <- 0.88
    
    d2$decision <- apply(class_probs, 1, function(p) sample(category_labels, 1, prob = p))
    
    d2$correct <- ifelse(d2$decision == d2$trueLabel, 1, 0)
    
    df = rbind(df, d2)
  }
  return(df)
}

# sample_primary5(1)


############ strong pattern ###############

sample_strong_pattern = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "trueLable", "worker_eff", "int_eff",   
                    "re",  "decision", "correct" )
  n_classes = 5
  
  for (x in 1:nworker){
    d3 = dtImage
    n_rows = dim(d3)[1]
    class_probs <- matrix(0.01, nrow = nrow(d3), ncol = n_classes)
    
    for (i in 1:n_rows) {
      class_probs[i, (i %% n_classes) + 1] <- 0.96
    }
    
    
    d3$decision <- apply(class_probs, 1, function(p) sample(category_labels, 1, prob = p))
    
    d3$correct <- ifelse(d3$decision == d3$trueLabel, 1, 0)
    
    df = rbind(df, d3)
  }
  return(df)
}

# sample_strong_pattern(1)
# 
# sum(sample_strong_pattern(1)$correct)/80

############ normal worker ################

sample_normal = function(nworker){
  df <- data.frame(matrix(ncol = 10, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  
  for(t in 1:nworker){
    d6 = dtImage
    d6$worker_eff = runif(1, -0.4, 0.4)
    for (i in 1:n_image){
      d6$int_eff[i] = runif(1, -0.4, 0.4) 
    }
    
    d6$re = d6$image_eff + d6$worker_eff + d6$int_eff
    
    d6$decision <- cut(
      d6$re,
      breaks = c(-Inf, q[1], q[2], q[3], q[4], Inf),
      labels = random_labels, 
      include.lowest = TRUE
    )
    
    d6$correct = ifelse(d6$decision == d6$trueLabel, 1, 0)
    df = rbind(df, d6)
  }
  df = df[, c( "imageid", "image_eff", "trueLabel", "decision", "correct" )]
  return(df)
}