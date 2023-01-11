install.packages('tidyverse')

library(tidyverse)

set.seed(0)
n_subjects = 10
sigma_gamma = 0.25
n_obs = 10 # Number of times each subject is observed
n = n_obs * n_subjects # Total number of observations

subjects = rep(1:n_subjects, n_obs)
sigma_0 = 0.25
z_0 = rnorm(n_subjects, 0, sigma_0) #Each subject gets their own random effect

x = rnorm(n)
b = 2
b0 = 1
sigma_0 = 0.25
y = b*x + (b0 + z_0[subjects]*sigma_0)


