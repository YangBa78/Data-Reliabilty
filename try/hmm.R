#update H,L,M to 1,2,3 for function. 
spring$park_obs <- ifelse(spring$parks < -10, 1,
                          ifelse(spring$parks >= -10 & spring$parks < 40, 2, 3))

weather_mob <- read.csv("/Users/andrewleonard/Library/Mobile Documents/com~apple~CloudDocs/Grad School/Linear:Non Linear/weather_mobility.csv")

#pull out spring dates only
spring <- weather_mob[1:90,]

forward = function(v, a, b, initial_distribution){
  
  T = length(v)
  m = nrow(a)
  alpha = matrix(0, T, m)
  
  alpha[1, ] = initial_distribution*b[, v[1]]
  
  for(t in 2:T){
    tmp = alpha[t-1, ] %*% a
    alpha[t, ] = tmp * b[, v[t]]
  }
  return(alpha)
}


backward = function(V, A, B){
  T = length(V)
  m = nrow(A)
  beta = matrix(1, T, m)
  
  for(t in (T-1):1){
    tmp = as.matrix(beta[t+1, ] * B[, V[t+1]])
    beta[t, ] = t(A %*% tmp)
  }
  return(beta)
}

#BaumWelch
BaumWelch = function(v, a, b, initial_distribution, n.iter = 100){
  
  for(i in 1:n.iter){
    T = length(v)
    M = nrow(a)
    K=ncol(b)
    alpha = forward(v, a, b, initial_distribution)
    beta = backward(v, a, b)
    xi = array(0, dim=c(M, M, T-1))
    
    for(t in 1:T-1){
      denominator = ((alpha[t,] %*% a) * b[,v[t+1]]) %*% matrix(beta[t+1,]) 
      for(s in 1:M){
        numerator = alpha[t,s] * a[s,] * b[,v[t+1]] * beta[t+1,]
        xi[s,,t]=numerator/as.vector(denominator)
      }
    }
    
    
    xi.all.t = rowSums(xi, dims = 2)
    a = xi.all.t/rowSums(xi.all.t)
    
    gamma = apply(xi, c(1, 3), sum)  
    gamma = cbind(gamma, colSums(xi[, , T-1]))
    for(l in 1:K){
      b[, l] = rowSums(gamma[, which(v==l)])
    }
    b = b/rowSums(b)
    
  }
  return(list(a = a, b = b, initial_distribution = initial_distribution))
}

M=3; K=3
A = matrix(1, M, M)
A = A/rowSums(A)
B = matrix(1:9, M, K)
B = B/rowSums(B)
initial_distribution = c(1/3,1/3, 1/3)


obs = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)

(myout = BaumWelch(obs, A, B, initial_distribution, n.iter = 100))


pi <- c(1/3,1/3,1/3)
S <- c("rain", "clear","warm")
hmm2 = initHMM(S, V_labels, startProbs = pi, transProbs = myout$a, emissionProbs = myout$b)

print(hmm2)



###################

if(!require('HMM')) {
  install.packages('HMM')
  library('HMM')
}


# Initial HMM
hmm = initHMM(c("A","B"),c("L","R"),
              transProbs=matrix(c(.5,.5,.5,.5),2),
              emissionProbs=matrix(c(.5,.51,.5,.49),2))
print(hmm)
# Sequence of observation
a = sample(c(rep("L",100),rep("R",300)))
b = sample(c(rep("L",300),rep("R",100)))
observation = c(a,b)
# Baum-Welch
bw = baumWelch(hmm,observation,10)
print(bw$hmm)
bw$difference


########
hmm = initHMM(c("A","B"),c('different','same'),
              transProbs=matrix(c(0.5,0.5,0.5,0.5),2),
              emissionProbs=matrix(c(0.01,0.99,0.99,0.01),2))
print(hmm)


hmm = initHMM(c("A","B"),c('different','same'),
              transProbs=matrix(c(0.5,0.5,0.5,0.5),2),
              emissionProbs=matrix(c(0.5,0.5,0.5,0.5),2))
print(hmm)

# Baum-Welch
bw = baumWelch(hmm,s1,10)
bw$hmm$transProbs

print(bw$hmm)
bw$difference

ss1


s1 = spam1[spam1$workerid==2,]$decision
s1 = ifelse(s1==1, 'different', 'same')

s1.1=s1[1:1000]
s1.2=s1[1001:2000]
s1.3 = s1[1:500]


s2 = spam1[spam1$workerid==2,]$decision
s2 = ifelse(s2==1, 'different', 'same')
s2.1=s2[1:1000]
s2.2=s2[1001:2000]


n2 = norm[norm$workerid==21,]$decision
n2 = ifelse(n2==1, 'different', 'same')
n2.1=n2[1:1000]
n2.2=n2[1001:2000]

for ( i in norm$workerid){
  norm[norm$workerid==21,]$decision
  
}


ss1 = spam2[spam2$workerid==3,]$decision
ss1 = ifelse(ss1==1, 'different', 'same')
ss1.1=n1[1:1000]
ss1.2=n1[1001:2000]

ss1.1=n1[1:(n_image/2)]
n_image

ss1 = spam4[spam4$workerid==7,]$decision
ss1 = ifelse(ss1==1, 'different', 'same')
ss1.1=n1[1:1000]
ss1.2=n1[1001:2000]

r = rep(c('different', 'same'), 1000)
r = rep(c('same', 'different'), 1000)
r


###################
library(markovchain)
library(dplyr)
# SDR Funnel is our sales representative stages, AE Funnel is our account executive stages, and CW is a successfully closed deal
seq <- c('SDR Funnel','SDR Funnel','AE Funnel','AE Funnel','AE Funnel','AE Funnel','AE Funnel','AE Funnel','CW')
verifyMarkovProperty(seq)

###############
# Initialise HMM
hmm = initHMM(c("A","B"), c("L","R"), transProbs=matrix(c(.8,.2,.2,.8),2),
              emissionProbs=matrix(c(.6,.4,.4,.6),2))
print(hmm)# Sequence of observations
observations = c("L","L","R","R")
# Calculate backward probablities
logBackwardProbabilities = backward(hmm,observations)
print(exp(logBackwardProbabilities))


# 
install.packages('randtests')
library(randtests)
runs.test(casino)
casino<-sample(c(0:36), 100000, replace = TRUE)



library(hmm)

# Generate some example data
set.seed(123)
states <- c("A", "B", "C")
symbols <- c("0", "1")
transition_matrix <- matrix(c(0.7, 0.2, 0.1, 0.3, 0.5, 0.2, 0.2, 0.3, 0.5), nrow = 3)
emission_matrix <- matrix(c(0.6, 0.4, 0.2, 0.8, 0.3, 0.7), nrow = 3)
initial_probs <- c(0.3, 0.4, 0.3)
hmm_model <- initHMM(states, symbols, transition_matrix, emission_matrix, initial_probs)

# Generate a sequence of length 10
sequence <- sample(symbols, 10, replace = TRUE)

# Calculate the log-likelihood of the sequence under the HMM model
loglik <- logLik.HMM(hmm_model, sequence)

# Print the log-likelihood value
print(loglik)



library(HMM)

# Create a sequence of observations
obs_seq <- c(1, 1, 2, 2, 2, 1, 2, 1, 1)

# Create an HMM with three states and two possible observations
hmm_model <- initHMM(c("S1", "S2", "S3"), c("A", "B"), 
                     transProbs=matrix(c(0.4, 0.4, 0.2, 0.5, 0.3, 0.2, 0.3, 0.3, 0.4), nrow=3), 
                     emissionProbs=matrix(c(0.6, 0.4, 0.3, 0.7, 0.3, 0.4), nrow=3), 
                     startProbs=c(0.4, 0.3, 0.3))

# Calculate the log-likelihood of the sequence under the HMM
loglik <- HMMforward(obs_seq, hmm_model)

# Print the log-likelihood
print(loglik)


library(HMM)

# Create a sequence of observations
obs_seq <- c(1, 1, 2, 2, 2, 1, 2, 1, 1)

# Create an HMM with three states and two possible observations
hmm_model <- initHMM(c("S1", "S2", "S3"), c("A", "B"), 
                     transProbs=matrix(c(0.4, 0.4, 0.2, 0.5, 0.3, 0.2, 0.3, 0.3, 0.4), nrow=3), 
                     emissionProbs=matrix(c(0.6, 0.4, 0.3, 0.7, 0.3, 0.4), nrow=3), 
                     startProbs=c(0.4, 0.3, 0.3))

# Calculate the log-likelihood of the sequence under the HMM
hmm_model$loglikelihood <- forward_backward(obs_seq, hmm_model)$logLik

# Print the log-likelihood
print(hmm_model$loglikelihood)


library(HMM)

# Create a sequence of observations
obs_seq <- c(1, 1, 2, 2, 2, 1, 2, 1, 1)

# Create an HMM with three states and two possible observations
hmm_model <- initHMM(c("S1", "S2", "S3"), c("A", "B"), 
                     transProbs=matrix(c(0.4, 0.4, 0.2, 0.5, 0.3, 0.2, 0.3, 0.3, 0.4), nrow=3), 
                     emissionProbs=matrix(c(0.6, 0.4, 0.3, 0.7, 0.3, 0.4), nrow=3), 
                     startProbs=c(0.4, 0.3, 0.3))

# Train the HMM using the Baum-Welch algorithm
hmm_model <- BaumWelch(obs_seq, hmm_model)

# Calculate the log-likelihood of the sequence under the trained HMM
loglik <- forward(obs_seq, hmm_model)

# Print the log-likelihood
print(loglik)



# Initialise HMM
hmm = initHMM(c("A","B"), c("L","R"), transProbs=matrix(c(.8,.2,.2,.8),2),
              emissionProbs=matrix(c(.6,.4,.4,.6),2))

print(hmm)
# Sequence of observations
observations = c("L","L","R","R", "L")
# Calculate backward probablities
logBackwardProbabilities = backward(hmm,observations)
print(exp(logBackwardProbabilities))

logBackwardProbabilities = forward(hmm,observations)
print(exp(logBackwardProbabilities))

posterior = posterior(hmm,observations)
print(posterior)


bw = baumWelch(hmm,observations,10)
print(bw$hmm)
