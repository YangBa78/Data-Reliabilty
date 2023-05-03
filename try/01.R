library(coin)

install.packages('coin')

# Generate some data
set.seed(123)
data <- data.frame(x = rbinom(100, 1, 0.5), y = rbinom(100, 1, 0.5), z = rbinom(100, 1, 0.5))

# Test for conditional independence between x and y given z
result <- independence_test(x ~ y | z, data = data)

# Print the result
print(result)

chisq.test(table(data$x, data$y), correct = FALSE)

require('cmdstanr')
library(rethinking)
data(reedfrogs)
d <- reedfrogs
library(rstan)
cmdstanr::install_cmdstan()

# we recommend running this is a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
devtools::install_github("rmcelreath/rethinking")
data(reedfrogs)
d <- reedfrogs
library(tidyverse)
d <- 
  d %>%
  mutate(tank = 1:nrow(d))
library(brms)
b12.1 <- brms::brm(data = d, family = binomial,
      surv | trials(density) ~ 0 + factor(tank),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 12)
summary(b12.1)
