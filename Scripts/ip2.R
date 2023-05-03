# load packages
library(lme4)
library(tidyverse)
library(simstudy)
library(influence.ME)
library(ggplot2)
library(Matrix)


# variance
var_image <- 20

#n_worker <- 160
n_image <-500

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
dtImage$trueLable <- ifelse(dtImage$image_eff > 0, 1, 0)
dtImage

# var(worker): 6
# var(worker_eff): 5.123436
# spammers
############ random guess########## p = 0.5
sample_random = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  for (x in 1:nworker){
    d1 = dtImage
    d1$worker_eff = runif(1, -0.5, 0.5)
    d1$int_eff = runif(1, -0.01, 0.01) - (d1$image_eff + d1$worker_eff)
    d1$re = d1$image_eff + d1$worker_eff +d1$int_eff
    
    var(d1$int_eff)
    
    d1$p = 1/(1+(exp(-d1$re)))
    
    for (i in 1:n_image){
      d1$decision[i] = rbinom(1, 1, prob = d1$p[i])
    }
    
    d1$correct = ifelse(d1$decision == d1$trueLable, 1, 0)
    df = rbind(df, d1)
  }
  return(df)
}


############# primary choice1 ###############
sample_primary1 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  for (x in 1:nworker){
    d2 = dtImage
    d2$int_eff = runif(1, -0.3, 0.3)
    d2$worker_eff = 2 + runif(1, -0.1, 0.1) - (d2$image_eff + d2$int_eff)
    d2$re = d2$image_eff + d2$worker_eff +d2$int_eff
    
    var(d2$int_eff)
    var(d2$worker_eff)
    
    d2$p = 1/(1+(exp(-d2$re)))
    
    for (i in 1:n_image){
      d2$decision[i] = rbinom(1, 1, prob = d2$p[i])
    }
    
    d2$correct = ifelse(d2$decision == d2$trueLable, 1, 0)
    df = rbind(df, d2)
  }
  return(df)
}

########## primary choice2 #################
############# primary choice 1###############
sample_primary2 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  for (x in 1:nworker){
    d3 = dtImage
    d3$int_eff = runif(1, -0.3, 0.3)
    d3$worker_eff = -2 + runif(1, -0.1, 0.1) - (d3$image_eff + d3$int_eff)
    d3$re = d3$image_eff + d3$worker_eff +d3$int_eff
    
    var(d3$int_eff)
    var(d3$worker_eff)
    
    d3$p = 1/(1+(exp(-d3$re)))
    
    for (i in 1:n_image){
      d3$decision[i] = rbinom(1, 1, prob = d3$p[i])
    }
    
    d3$correct = ifelse(d3$decision == d3$trueLable, 1, 0)
    df = rbind(df, d3)
  }
  return(df)
}

############ strong pattern ###############
sample_pattern1 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  
  for (x in 1:nworker){
    for (x in 1:500){
      d4 = dtImage
      d4$worker_eff = runif(1, -0.5, 0.5)
      
      for (i in 1:n_image){
        if (i%%2==0){
          d4$int_eff[i] = 4 + runif(1, -0.01, 0.01) - (d4$image_eff[i] + d4$worker_eff[i])
        }else{
          d4$int_eff[i] = -4 + runif(1, -0.01, 0.01) - (d4$image_eff[i] + d4$worker_eff[i])
        }
      }
      d4$re = d4$image_eff + d4$worker_eff +d4$int_eff
      
      var(d4$int_eff)
      
      d4$p = 1/(1+(exp(-d4$re)))
      
      for (j in 1:n_image){
        d4$decision[j] = rbinom(1, 1, prob = d4$p[j])
      }
      
      d4$correct = ifelse(d4$decision == d4$trueLable, 1, 0)
      len = length(unique(rle(d4$decision)[1]$lengths))
      u = unique(rle(d4$decision)[1]$lengths)[1]
      if ((len ==1)&(u==1)){
        df = rbind(df, d4)
        break
      }
    } 
  }
  return(df)
}


########## strong pattern2#############
sample_pattern2 = function(nworker){
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) <- c("imageid", "image_eff", "nWorker",  "task", "trueLable", "worker_eff", "int_eff",   
                    "re",  "p",  "decision", "correct" )
  
  for (x in 1:nworker){
    for (x in 1:500){
      d5 = dtImage
      d5$worker_eff = runif(1, -0.5, 0.5)
      
      for (i in 1:n_image){
        if (i%%2==0){
          d5$int_eff[i] = -4 + runif(1, -0.01, 0.01) - (d5$image_eff[i] + d5$worker_eff[i])
        }else{
          d5$int_eff[i] = 4 + runif(1, -0.01, 0.01) - (d5$image_eff[i] + d5$worker_eff[i])
        }
      }
      d5$re = d5$image_eff + d5$worker_eff +d5$int_eff
      
      var(d5$int_eff)
      
      d5$p = 1/(1+(exp(-d5$re)))
      
      for (j in 1:n_image){
        d5$decision[j] = rbinom(1, 1, prob = d5$p[j])
      }
      
      d5$correct = ifelse(d5$decision == d5$trueLable, 1, 0)
      len = length(unique(rle(d5$decision)[1]$lengths))
      u = unique(rle(d5$decision)[1]$lengths)[1]
      if ((len ==1)&(u==1)){
        df = rbind(df, d5)
        break
      }
    } 
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
    d6$worker_eff = runif(1, -0.5, 0.5)
    for (i in 1:n_image){
      d6$int_eff[i] = runif(1, -0.5, 0.5) 
    }
    
    d6$re = d6$image_eff + d6$worker_eff + d6$int_eff
    
    d6$p = 1/(1+(exp(-d6$re)))
    
    for (i in 1:n_image){
      d6$decision[i] = rbinom(1, 1, prob = d6$p[i])
    }
    
    d6$correct = ifelse(d6$decision == d6$trueLable, 1, 0)
    df = rbind(df, d6)
  }
  return(df)
}


###### combine together #######
spam1 = sample_random(2)
spam1$workerid = rep(1:2, each=n_image)

spam2 = sample_primary1(2)
spam2$workerid = rep(3:4, each=n_image)

spam3 = sample_primary2(2)
spam3$workerid = rep(5:6, each=n_image)

spam4 = sample_pattern1(2)
spam4$workerid = rep(7:8, each=n_image)

spam5 = sample_pattern2(2)
spam5$workerid = rep(9:10, each=n_image)

norm = sample_normal(50) 
norm$workerid = rep(11:60, each=n_image)


data = rbind(spam1, spam2, spam3, spam4, spam5, norm)


################################

hmm = initHMM(c("A","B"),c('different','same'),
              transProbs=matrix(c(0.5,0.5,0.5,0.5),2),
              emissionProbs=matrix(c(0.01,0.99,0.99,0.01),2))
print(hmm)

s1 = spam1[spam1$workerid==2,]$decision
s1 = ifelse(s1==1, 'different', 'same')

# Baum-Welch
bw = baumWelch(hmm,s1,10)
print(bw$hmm)
bw$difference

ss1


s1 = spam1[spam1$workerid==2,]$decision
s1 = ifelse(s1==1, 'different', 'same')

s1.1=s1[1:50]
s1.2=s1[51:100]
s1.3 = s1[1:500]

###############################
sample<- rep(NA, 20)
result<- matrix
for(i in seq_along(sample)){
  # variance
  var_image <- 6
  #n_worker <- 160
  n_image <-2000
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
  
  # data
  spam1 = sample_random(20)
  spam1$workerid = rep(1:20, each=n_image)
  
  norm = sample_normal(20) 
  norm$workerid = rep(21:40, each=n_image)
  
  data = rbind(spam1, norm)
  
  # HMM
  
  hmm = initHMM(c("A","B"),c('different','same'),
              transProbs=matrix(c(0.5,0.5,0.5,0.5),2),
              emissionProbs=matrix(c(0.999,0.001,0.001,0.999),2))
  
  for(j in 1:40){
    d = data[data$workerid==j,]$decision
    d = ifelse(d==1, 'different', 'same')
    bw = baumWelch(hmm,d,10)
    result[i, j] = bw$hmm$transProbs 
  }
}




data = rbind(spam1, spam2, spam3, norm)


for(j in unique(data$workerid)){
  d = data[data$workerid==j,]$decision
  d = ifelse(d==1, 'different', 'same')
  
  #breaks <- quantile(d, probs = seq(0, 1, 1/2))
  #my_parts <- cut(d, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  seq1 = d[1:1000]
  seq2 = d[1001:2000]
  s = sample(d)
  #seq1 <- d[my_parts == 1]
  #seq2 <- d[my_parts == 2]
  #seq3 <- d[my_parts == 3]
  
  bw = baumWelch(hmm,d,10)
  bw1 = baumWelch(hmm,seq1,10)
  bw2 = baumWelch(hmm,seq2,10)
  #bw3 = baumWelch(hmm,seq3,10)
  bw3 = baumWelch(hmm,s,10)
  
  if(j==1){
    result = as.data.frame(bw$hmm$transProbs)
    s1 = as.data.frame(bw1$hmm$transProbs)
    s2 = as.data.frame(bw2$hmm$transProbs)
    s3 = as.data.frame(bw3$hmm$transProbs)
  }else{
    result = rbind(result, as.data.frame(bw$hmm$transProbs))
    s1 = rbind(s1, as.data.frame(bw1$hmm$transProbs))
    s2 = rbind(s2, as.data.frame(bw2$hmm$transProbs))
    s3 = rbind(s3, as.data.frame(bw3$hmm$transProbs))
  }
}


a = cbind(result, s1, s2, s3)
View(a)


seq1 = d[1:round((n_image/3))]
seq2 = d[(round((n_image/3))+1): 2*round((n_image/3))]
seq3 = d[(2*round((n_image/3))+1): n_image]





R <- matrix(c(1.0, 0.1, 0.1, 
              0.1, 1.0, 0.1, 
              0.1, 0.1, 1.0), nrow=3)
N <- 100

chi <- -2*log(det(R)^(N/2))
df <- nrow(R)*(nrow(R)-1)/2
p <- 1 - pchisq(chi, df)  
chi
p



# Define the transition matrices
P <- matrix(c(	
  0.4883299, 0.5116701, 0.4990371, 0.5009629), nrow = 2, byrow = TRUE)
Q <- matrix(c(0.5, 0.5, 0.5, 0.5), nrow = 2, byrow = TRUE)

# Define a function to compute the KL divergence for each entry in the matrices
kl_divergence <- function(p, q) {
  p * log(p / q)
}

# Compute the KL divergence for each entry in the matrices
kl_divergence_matrix <- kl_divergence(P, Q)

# Sum up the individual KL divergences to get the overall divergence
kl_divergence_total <- sum(kl_divergence_matrix, na.rm = TRUE)

# Print the results
print("KL Divergence Matrix:")
print(kl_divergence_matrix)
print(paste0("KL Divergence Total: ", kl_divergence_total))


# Define the observed KL divergence value between P and Q
obs_kl_div <- 0.3686907

# Set the number of permutations to perform
n_perms <- 1000

# Perform the permutation test
perm_kl_div <- numeric(n_perms)
for (i in 1:n_perms) {
  # Shuffle the rows and columns of Q
  Q_perm <- Q[sample(nrow(Q)), sample(ncol(Q))]
  
  # Compute the KL divergence between P and the shuffled Q matrix
  perm_kl_div[i] <- sum(kl_divergence(P, Q_perm), na.rm = TRUE)
}

# Calculate the p-value as the proportion of permuted KL divergences that are greater than or equal to the observed KL divergence
p_value <- sum(perm_kl_div >= obs_kl_div) / n_perms

# Print the results
print(paste0("Observed KL Divergence: ", obs_kl_div))
print(paste0("Permutation-based p-value: ", p_value))



# Define the transition matrices
P <- matrix(c(0.8, 0.2, 0.4, 0.6), nrow = 2, byrow = TRUE)
Q <- matrix(c(0.7, 0.3, 0.5, 0.5), nrow = 2, byrow = TRUE)

# Extract the probabilities for a given state from each matrix
state_probs_p <- c(P[1,], P[2,])
state_probs_q <- c(Q[1,], Q[2,])

# Use the KS test to compare the distributions of the state probabilities
ks_test_result <- ks.test(state_probs_p, state_probs_q)

# Print the results
print(ks_test_result)


set.seed(1)
samp <- rnorm(50,2,3)
vs.test(x = samp, densfun = 'dnorm', param = c(2,3), B = 500) #Simple null hypothesis
vs.test(x = samp, densfun='dnorm', B = 500) #Composite null hypothesis
## Using asymptotic distribution to compute the p-value
vs.test(x = samp, densfun='dnorm', simulate.p.value = FALSE) #Composite null hypothesis



# Create a simulated transition matrix
P <- matrix(c(0.4, 0.6, 0.2, 0.8), nrow=2, byrow=TRUE)

# Calculate the observed transition probabilities
observed <- P

# Calculate the expected transition probabilities under the null hypothesis of stationarity
stationary <- eigen(t(P))$vectors[,1] / sum(eigen(t(P))$vectors[,1])
expected <- outer(stationary, stationary)
expected <- matrix(c(0.5, 0.5, 0.5, 0.5), nrow=2, byrow=TRUE)

# Calculate the KL divergence
KL_divergence <- sum(observed * log(observed / expected))

# Calculate the p-value
n <- sum(observed)
df <- (nrow(P) * (nrow(P) - 1)) / 2 # degrees of freedom
p_value <- 1 - pchisq(KL_divergence * n, df)

# Print the results
cat("Kullback-Leibler (KL) divergence test for Markov Chain transition matrix\n")
cat("Test statistic:", round(KL_divergence, 2), "\n")
cat("Degrees of freedom:", df, "\n")
cat("p-value:", round(p_value, 4), "\n")


# Generate two simulated transition matrices
P1 <- matrix(c(0.7, 0.3, 0.2, 0.8), nrow=2, byrow=TRUE)
P2 <- matrix(c(0.6, 0.4, 0.3, 0.7), nrow=2, byrow=TRUE)

# Calculate the observed transition probabilities for both matrices
observed1 <- P1
observed2 <- P2

# Calculate the expected transition probabilities under the null hypothesis of stationarity for both matrices
stationary1 <- eigen(t(P1))$vectors[,1] / sum(eigen(t(P1))$vectors[,1])
expected1 <- outer(stationary1, stationary1)
stationary2 <- eigen(t(P2))$vectors[,1] / sum(eigen(t(P2))$vectors[,1])
expected2 <- outer(stationary2, stationary2)

# Calculate the KL divergence between the observed and expected probabilities for both matrices
KL_divergence1 <- sum(observed1 * log(observed1 / expected1))
KL_divergence2 <- sum(observed2 * log(observed2 / expected2))

# Calculate the test statistic (absolute difference between KL divergences)
test_statistic <- abs(KL_divergence1 - KL_divergence2)

# Calculate the p-value using a chi-squared test with degrees of freedom equal to the number of non-zero transitions in the matrices
df <- sum(P1 != 0) + sum(P2 != 0)
p_value <- 1 - pchisq(test_statistic * n, df)

# Print the results
cat("Kullback-Leibler (KL) divergence test for Markov Chain transition matrices\n")
cat("Test statistic:", round(test_statistic, 2), "\n")
cat("Degrees of freedom:", df, "\n")
cat("p-value:", round(p_value, 4), "\n")



# Generate two simulated transition matrices
set.seed(123)
P1 <- matrix(runif(16), nrow=4)
P2 <- matrix(runif(16), nrow=4)

# Normalize matrices to ensure rows sum to 1
P1 <- t(apply(P1, 1, function(x) x/sum(x)))
P2 <- t(apply(P2, 1, function(x) x/sum(x)))

# Calculate observed transition counts for each matrix
n1 <- c(10, 20, 30, 40, 20, 30, 10, 40, 15, 15, 25, 45, 30, 20, 10, 40)
n2 <- c(20, 10, 40, 30, 30, 20, 15, 35, 10, 30, 20, 40, 25, 35, 10, 30)

# Calculate expected transition counts for each matrix
E1 <- P1 * sum(n1)
E2 <- P2 * sum(n2)

# Calculate G test statistic
G <- 2 * sum(n1 * log(n1/E1) + n2 * log(n2/E2))

# Calculate degrees of freedom
df <- (nrow(P1) * ncol(P1) - nrow(P1))

# Calculate p-value
pval <- 1 - pchisq(G, df)

# Print results
cat("G test statistic:", round(G, 2), "\n")
cat("Degrees of freedom:", df, "\n")
cat("p-value:", format(pval, scientific = FALSE), "\n")



# Generate two example transition matrices
set.seed(123)
m1 <- matrix(runif(16), nrow = 4)
m2 <- matrix(runif(16), nrow = 4)

# Compute KL divergence between the two matrices
kl_div <- sum(m1 * log2(m1 / m2))

# Set the number of bootstraps
n_bootstraps <- 1000

# Generate bootstrapped matrices and compute KL divergence
kl_div_boot <- replicate(n_bootstraps, {
  m1_boot <- matrix(sample(c(m1), size = length(m1), replace = TRUE), nrow = nrow(m1))
  m2_boot <- matrix(sample(c(m2), size = length(m2), replace = TRUE), nrow = nrow(m2))
  sum(m1_boot * log2(m1_boot / m2_boot))
})

# Compute the empirical p-value
p_value <- mean(kl_div_boot >= kl_div)

# Print the results
cat("KL divergence between the original matrices:", kl_div, "\n")
cat("Empirical p-value of the bootstrapped test:", p_value, "\n")

# Define the transition matrices
P <- matrix(c(0.8, 0.2, 0.4, 0.6), nrow = 2, byrow = TRUE)
Q <- matrix(c(0.7, 0.3, 0.5, 0.5), nrow = 2, byrow = TRUE)

# Define a function to compute the KL divergence for each entry in the matrices
kl_divergence <- function(p, q) {
  p * log(p / q)
}

# Compute the KL divergence for each entry in the matrices
kl_divergence_matrix <- kl_divergence(P, Q)

# Sum up the individual KL divergences to get the overall divergence
kl_divergence_total <- sum(kl_divergence_matrix, na.rm = TRUE)

# Print the results
print("KL Divergence Matrix:")
print(kl_divergence_matrix)
print(paste0("KL Divergence Total: ", kl_divergence_total))



######
install.packages('ape')
library(ape)

q1 <- matrix(runif(36), nrow = 6)
q2 <- matrix(runif(36), nrow = 6)
diag(q1) <- diag(q2) <- 0
mantel.test(q1, q2, graph = TRUE,
            main = "Mantel test: a random example with 6 X 6 matrices
representing asymmetric relationships",
            xlab = "z-statistic", ylab = "Density",
            sub = "The vertical line shows the observed z-statistic")




library(philentropy)
# create two sample transition matrices
P1 <- matrix(c(0.4, 0.6, 0.3, 0.7), nrow = 2)
P2 <- matrix(c(0.5, 0.5, 0.2, 0.8), nrow = 2)

# set the number of simulations
nsim <- 1000

# create an empty vector to store the simulated KL divergences
sim_KLDs <- numeric(nsim)

# loop through the number of simulations and calculate the KL divergence for each simulation
for (i in 1:nsim) {
  # generate a random matrix with the same dimensions as P2
  P2_sim <- matrix(runif(n = 4), nrow = 2)
  
  # normalize the rows of P2_sim to sum to 1
  P2_sim <- P2_sim / rowSums(P2_sim)
  
  # calculate the KL divergence between P1 and P2_sim
  sim_KLDs[i] <- kl_divergence(P1, P2_sim)
}

# calculate the 95th percentile of the simulated KL divergences
cutoff <- quantile(sim_KLDs, 0.95)

# compare the observed KL divergence to the cutoff
obs_KLD <- kl_divergence(P1, P2)
if (obs_KLD > cutoff) {
  cat("The observed KL divergence is significant.")
} else {
  cat("The observed KL divergence is not significant.")
}



# function to randomly permute rows and columns of a matrix
sample_rows_cols <- function(mat) {
  rows <- sample(nrow(mat))
  cols <- sample(ncol(mat))
  mat_perm <- mat[rows, cols]
  return(mat_perm)
}

# function to calculate KL divergence between two matrices
KLD <- function(P, Q) {
  P <- as.matrix(P)
  Q <- as.matrix(Q)
  KL <- sum(P * log2(P / Q), na.rm = TRUE)
  return(KL)
}

# create two sample transition matrices
P1 <- matrix(c(0.4, 0.6, 0.3, 0.7), nrow = 2)
P2 <- matrix(c(0.9, 0.1, 0.1, 0.9), nrow = 2)

# calculate the KL divergence between the two matrices
KL_div <- KLD(P1, P2)

# set the number of permutations
nperm <- 1000

# create an empty vector to store the permutation test results
perm_results <- numeric(nperm)

# loop through the number of permutations and calculate the KL divergence for each permutation
for (i in 1:nperm) {
  # permute the rows and columns of the second matrix
  P2_perm <- sample_rows_cols(P2)
  
  # calculate the KL divergence between the two permuted matrices
  perm_results[i] <- KLD(P1, P2_perm)
}

# calculate the p-value as the proportion of permutations with a KL divergence greater than or equal to the observed KL divergence
p_value <- mean(perm_results >= KL_div)

# print the p-value
cat("p-value:", p_value)



# define a function to compute KL divergence between two matrices
KLD <- function(p, q) {
  sum(p * log(p / q), na.rm = TRUE)
}

# create two sample transition matrices
P1 <- matrix(c(0.4, 0.6, 0.3, 0.7), nrow = 2)
P2 <- matrix(c(0.5, 0.5, 0.2, 0.8), nrow = 2)

# calculate the KL divergence between the two matrices
KL_div <- KLD(P1, P2)

# set the cutoff for similarity
cutoff <- 0.1

# compare the KL divergence to the cutoff and print the result
if (KL_div <= cutoff) {
  cat("The two matrices are similar.")
} else {
  cat("The two matrices are different.")
}


library(vegan)
P1 <- matrix(c(0.4, 0.6, 0.3, 0.7), nrow = 2)
P2 <- matrix(c(0.5, 0.5, 0.2, 0.8), nrow = 2)

# calculate the KL divergence between the two matrices
KL_div <- KLD(P1, P2)

# create a distance matrix using the KL divergence
dist_matrix <- matrix(0, nrow = 2, ncol = 2)
dist_matrix[1, 2] <- KL_div
dist_matrix[2, 1] <- KL_div

# run the Mantel test with 999 permutations
mantel_test <- mantel(dist_matrix, dist_matrix, method = "spearman", permutations = 999)
mantel(dist_matrix, dist_matrix, method = "spearman", nperm = 999)

# print the result
mantel_test


# create two sample transition matrices
P1 <- matrix(c(0.4, 0.6, 0.3, 0.7), nrow = 2)
P2 <- matrix(c(0.5, 0.5, 0.2, 0.8), nrow = 2)

# calculate the observed KL divergence between the two matrices
KL_div_obs <- KLD(P1, P2)

# set the number of simulations
nsim <- 1000

# create an empty vector to store the simulation results
sim_results <- numeric(nsim)

# loop through the number of simulations and calculate the KL divergence for each simulation
for (i in 1:nsim) {
  # generate a random transition matrix with the same dimensions as P2
  P2_sim <- matrix(runif(4), nrow = 2)
  
  # normalize the rows to ensure they sum to 1
  P2_sim <- P2_sim / rowSums(P2_sim)
  
  # calculate the KL divergence between the two simulated matrices
  sim_results[i] <- KLD(P1, P2_sim)
}

# calculate the p-value as the proportion of simulations with a KL divergence greater than or equal to the observed KL divergence
p_value <- mean(sim_results >= KL_div_obs)

# print the p-value
cat("p-value:", p_value)



# Sample KL divergence values
kl_value1 <- 2.15
kl_value2 <- 2.18

# Null hypothesis: No significant difference
# Alternative hypothesis: Significant difference
# Assuming a significance level of 0.05 (5%)

# Perform t-test
result <- t.test(c(kl_value1, kl_value2))

# Extract p-value
p_value <- result$p.value

# Compare p-value to significance level
if (p_value < 0.05) {
  # Significant difference
  print("There is a significant difference between the KL divergence values.")
} else {
  # No significant difference
  print("There is no significant difference between the KL divergence values.")
}



# Sample KL divergence value
kl_value <- 2.5

# Null hypothesis: No significant difference (distribution = null distribution)
# Alternative hypothesis: Significant difference
# Assuming a significance level of 0.05 (5%)
num_permutations <- 1000

# Generate null distribution by permuting the data
null_distribution <- replicate(num_permutations, {
  # Permute the KL divergence value
  permuted_kl_value <- sample(kl_value, size = 1)
  # Calculate the permuted test statistic (e.g., absolute difference)
  abs_difference <- abs(permuted_kl_value - kl_value)
  # Return the permuted test statistic
  abs_difference
})

# Calculate the observed test statistic (e.g., absolute difference)
observed_statistic <- abs(kl_value - kl_value)

# Calculate the p-value as the proportion of permuted test statistics greater than or equal to the observed statistic
p_value <- sum(null_distribution >= observed_statistic) / num_permutations

# Compare p-value to significance level
if (p_value < 0.05) {
  # Significant difference
  print("The KL divergence value is significantly different from the null distribution.")
} else {
  # No significant difference
  print("The KL divergence value is not significantly different from the null distribution.")
}



# Example log likelihood values
log_likelihood_null <- -100
log_likelihood_alternative <- -80

# Calculate the likelihood ratio statistic
LLR <- -2 * (log_likelihood_null - log_likelihood_alternative)

# Set the significance level (alpha)
alpha <- 0.05

# Degrees of freedom (parameter difference)
df <- 1

# Calculate the critical value
critical_value <- qchisq(1 - alpha, df)

# Compare the likelihood ratio statistic with the critical value
if (LLR > critical_value) {
  # Reject the null hypothesis
  cat("The two log likelihoods are significantly different.\n")
} else {
  # Fail to reject the null hypothesis
  cat("There is no significant difference between the log likelihoods.\n")
}



# Example log likelihood values
log_likelihood_1 <- c(-100, -110, -90, -95, -105)
log_likelihood_2 <- c(-80, -85, -75, -90, -82)

# Compute the observed difference
observed_diff <- mean(log_likelihood_1) - mean(log_likelihood_2)

# Number of bootstraps
n_bootstraps <- 1000

# Initialize vector to store bootstrapped differences
bootstrapped_diffs <- numeric(n_bootstraps)

# Perform bootstrapping
for (i in 1:n_bootstraps) {
  # Generate bootstrap samples by resampling with replacement
  bootstrap_sample_1 <- sample(log_likelihood_1, replace = TRUE)
  bootstrap_sample_2 <- sample(log_likelihood_2, replace = TRUE)
  
  # Calculate the difference between bootstrap samples
  bootstrapped_diffs[i] <- mean(bootstrap_sample_1) - mean(bootstrap_sample_2)
}

# Compute the threshold as the quantile of the bootstrapped differences
alpha <- 0.05
threshold <- quantile(bootstrapped_diffs, 1 - alpha)

# Compare the observed difference with the threshold
if (observed_diff > threshold) {
  cat("The difference between log likelihoods is statistically significant.\n")
} else {
  cat("There is no significant difference between the log likelihoods.\n")
}



# Sample KL divergence value
kl_value <- 2.5

# Null hypothesis: No significant difference (mean = threshold)
# Alternative hypothesis: Significant difference (mean != threshold)
# Assuming a significance level of 0.05 (5%)
threshold <- 2.0

# Perform one-sample t-test
result <- t.test(kl_value, mu = threshold)

# Extract p-value
p_value <- result$p.value

# Compare p-value to significance level
if (p_value < 0.05) {
  # Significant difference
  print("The KL divergence value is significantly different from the threshold.")
} else {
  # No significant difference
  print("The KL divergence value is not significantly different from the threshold.")
}



set.seed(0)
treeVolume <- c(rnorm(75, mean = 36500, sd = 2000))
t.test(treeVolume, mu = 39000) # Ho: mu = 39000


set.seed(2820)

preTreat <- c(rnorm(1000, mean = 145, sd = 9))
postTreat <- c(rnorm(1000, mean = 138, sd = 8))

t.test(preTreat, postTreat, paired = TRUE)


set.seed(0)

ClevelandSpending <- rnorm(50, mean = 250, sd = 75)
NYSpending <- rnorm(50, mean = 300, sd = 80)

t.test(ClevelandSpending, NYSpending, var.equal = TRUE)

install.packages("coin")  # Run this line if you haven't installed the package before
library(coin)


# Load the mtcars dataset
data(mtcars)

# Subset the data for automatic and manual transmissions
auto_mpg <- mtcars$mpg[mtcars$am == 0]  # Automatic transmission (am = 0)
manual_mpg <- mtcars$mpg[mtcars$am == 1]  # Manual transmission (am = 1)

# Perform the independent t-test
t.test(auto_mpg, manual_mpg)

data <- rnorm(1000000)
ks.test(data, "pnorm")
data
ks.test(dfl$loglik, "pnorm")



group1 <- rnorm(50, 0, 1)
group2 <- rnorm(60, 2, 1)

# Perform the Mann-Whitney U test
wilcox.test(group1, group2)

wilcox.test(s1.1, s2)
s2
s1.1



# Define the transition matrices
P <- matrix(c(0.8, 0.2, 0.4, 0.6), nrow = 2, byrow = TRUE)
Q <- matrix(c(0.7, 0.3, 0.5, 0.5), nrow = 2, byrow = TRUE)

# Define a function to compute the KL divergence for each entry in the matrices
kl_divergence <- function(p, q) {
  p * log(p / q)
}

# Compute the KL divergence for each entry in the matrices
kl_divergence_matrix <- kl_divergence(P, Q)

# Sum up the individual KL divergences to get the overall divergence
kl_divergence_total <- sum(kl_divergence_matrix, na.rm = TRUE)

# Print the results
print("KL Divergence Matrix:")
print(kl_divergence_matrix)
print(paste0("KL Divergence Total: ", kl_divergence_total))

# Define the transition matrices
P <- matrix(c(0.488, 0.512, 0.496, 0.504), nrow = 2, byrow = TRUE)
P <- matrix(c(0.888, 0.112, 0.897, 0.103), nrow = 2, byrow = TRUE)
Q <- matrix(c(0.999, 0.001, 0.999, 0.001), nrow = 2, byrow = TRUE)

# Define a function to compute the Jensen-Shannon divergence for each entry in the matrices
jensen_shannon_divergence <- function(p, q) {
  m <- 0.5 * (p + q)
  jsd <- 0.5 * (kl_divergence(p, m) + kl_divergence(q, m))
  return(jsd)
}

# Define a function to compute the KL divergence for each entry in the matrices
kl_divergence <- function(p, q) {
  p * log(p / q)
}

# Compute the Jensen-Shannon divergence for each entry in the matrices
js_divergence_matrix <- jensen_shannon_divergence(P, Q)


# Sum up the individual Jensen-Shannon divergences to get the overall divergence
js_divergence_total <- sum(js_divergence_matrix, na.rm = TRUE)

# Print the results
print("Jensen-Shannon Divergence Matrix:")
print(js_divergence_matrix)
print(paste0("Jensen-Shannon Divergence Total: ", js_divergence_total))


# Define the observed KL divergence value between P and Q
obs_kl_div <- 0.3686907

# Set the number of permutations to perform
n_perms <- 1000

# Perform the permutation test
perm_kl_div <- numeric(n_perms)
for (i in 1:n_perms) {
  # Shuffle the rows and columns of Q
  Q_perm <- Q[sample(nrow(Q)), sample(ncol(Q))]
  
  # Compute the KL divergence between P and the shuffled Q matrix
  perm_kl_div[i] <- sum(kl_divergence(P, Q_perm), na.rm = TRUE)
}

# Calculate the p-value as the proportion of permuted KL divergences that are greater than or equal to the observed KL divergence
p_value <- sum(perm_kl_div >= obs_kl_div) / n_perms

# Print the results
print(paste0("Observed KL Divergence: ", obs_kl_div))
print(paste0("Permutation-based p-value: ", p_value))


n_sample = rep(NA, 10^4)
for(i in seq_along(n_sample)){
  sample(dfl$loglik)
  # mean, variance
  # 
}







# Generate a vector of example data
data <- c(10, 12, 15, 14, 13, 16, 19, 18, 17, 100)

# Calculate the z-scores
mean_data <- mean(data)
sd_data <- sd(data)
z_scores <- abs((data - mean_data) / sd_data)

# Set the threshold for outliers
threshold <- 2  # Adjust as needed

# Identify outliers
outliers <- data[z_scores > threshold]

# Print the outliers
cat("Outliers:", outliers)



# Create two sample transition matrices
P1 <- matrix(c(0.4, 0.6, 0.3, 0.7), nrow = 2)
P2 <- matrix(c(0.5, 0.5, 0.2, 0.8), nrow = 2)

# Calculate the KL divergence from P1 to P2
KL_div_1to2 <- sum(P1 * log2(P1 / P2))

# Calculate the KL divergence from P2 to P1
KL_div_2to1 <- sum(P2 * log2(P2 / P1))

# Calculate the average KL divergence
KL_div_avg <- (KL_div_1to2 + KL_div_2to1) / 2

# Print the results
cat("KL divergence from P1 to P2:", KL_div_1to2, "\n")
cat("KL divergence from P2 to P1:", KL_div_2to1, "\n")
cat("Average KL divergence:", KL_div_avg, "\n")

a = c(0.54, 0.46)
b = c(0.5, 0.5)

##kl_divergence(a, b)

KL(rbind(a, b))

library(philentropy)

# Kulback-Leibler Divergence between P and Q
P <- 1:10/sum(1:10)
Q <- 20:29/sum(20:29)
x <- rbind(P,Q)
KL(x)

# Kulback-Leibler Divergence between P and Q using different log bases
KL(rbind(a, b), unit = "log2") # Default
KL(rbind(a, b), unit = "log")
KL(rbind(a, b), unit = "log10")

# Kulback-Leibler Divergence between count vectors P.count and Q.count
P.count <- 1:10
Q.count <- 20:29
x.count <- rbind(P.count,Q.count)
KL(x, est.prob = "empirical")

# Example: Distance Matrix using KL-Distance

Prob <- rbind(1:10/sum(1:10), 20:29/sum(20:29), 30:39/sum(30:39))

# compute the KL matrix of a given probability matrix
KLMatrix <- KL(Prob)

# plot a heatmap of the corresponding KL matrix
heatmap(KLMatrix)

# mean: np. variance: npq
80*0.5
80*0.5*0.5
1/12

(0.5*0.5)/80*1.96

0.5+ c(-1, 1)*sqrt((0.5*0.5)/2000)*1.96




# Generate example data for Groups A and B
group_A <- c(5, 7, 10, 12, 15)
group_B <- c(8, 9, 11, 13, 16, 20)

# Perform the Mann-Whitney U test
result <- wilcox.test(group_A, group_B, alternative = "less")

# Extract the p-value
p_value <- result$p.value

# Print the p-value
cat("p-value:", p_value)


# Generate example data
group_A <- c(5, 7, 10, 12, 15)
specific_value <- 14

# Perform the one-sample Wilcoxon signed-rank test
result <- wilcox.test(group_A, mu = specific_value, alternative = "less")

# Extract the p-value
p_value <- result$p.value

# Print the p-value
cat("p-value:", p_value)

# Generate example data
group_data <- c(10, 12, 15, 14, 13, 16, 19, 18, 17)
new_data <- 20

# Perform the one-sample t-test
result <- t.test(group_data, mu = new_data, alternative = "less")

# Extract the p-value
p_value <- result$p.value

# Print the p-value
cat("p-value:", p_value)


library(car)
white_matter <- read.table(text="   Control Patient
1   0.3329  0.3306
2   0.3458  0.3375
3   0.3500  0.3874
4   0.3680  0.3485
5   0.3421  0.3548
6   0.3403  0.3876
7   0.3447  0.3755
8   0.3330  0.3644
9   0.3450  0.3206
10  0.3764  0.3587
11  0.3646  0.3570
12  0.3482  0.3423
13  0.3734  0.3583
14  0.3436  0.3457
15  0.3348  0.3770
16  0.3553  0.3419
17  0.3281  0.3416
18  0.3567  0.3703
19  0.3390  0.3525
20  0.3287  0.3596
21  0.3603  0.3519
22  0.3533  0.3443", header=T)

set.seed(1315)
B      <- 1000
t.vect <- vector(length=B)
p.vect <- vector(length=B)
for(i in 1:B){
  boot.c <- sample(white_matter$Control, size=22, replace=T)
  boot.p <- sample(white_matter$Patient, size=22, replace=T)
  ttest  <- t.test(boot.c, boot.p)
  t.vect[i] <- ttest$statistic
  p.vect[i] <- ttest$p.value
}

windows()
  qqPlot(t.vect, distribution="t", df=42)

  
windows()
  qqPlot(p.vect, distribution="unif")
  
  
  
library(boot)
  # Define the test statistic function
test_statistic <- function(data, indices) {
  group <- data[indices]
  
  sample_mean <- mean(group)
  sample_sd <- sd(group)
  n <- length(group)
  
  t_stat <- (sample_mean - hypothesized_mean) / (sample_sd / sqrt(n))
  return(t_stat)
}

# Set the hypothesized mean
hypothesized_mean <- 0

# Set the number of bootstrap iterations (e.g., 1000)
num_iterations <- 1000
group = dfl$loglik
# Perform the bootstrap
bootstrap_results <- boot(group, test_statistic, R = num_iterations)


# Calculate the observed test statistic
observed_statistic <- (mean(group) - hypothesized_mean) / (sd(group) / sqrt(length(group)))

# Calculate the p-value
p_value <- mean(abs(bootstrap_results$t) >= abs(observed_statistic))

p_value <- mean(abs(bootstrap_results$t) >= 114)
# Calculate the confidence interval using the "bca" method
ci <- boot.ci(bootstrap_results, type = "bca")






