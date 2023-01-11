# load required packages
library("lme4") library("lmerTest") library("tidyverse")
# model specification / estimation
# provides p-values in the output
# data wrangling and visualisation

# ensure this script returns the same results on each run
set.seed(8675309)

# set fixed effect parameters
beta_0 <- 800 # intercept; i.e., the grand mean 
beta_1 <- 50 # slope; i.e, effect of category

# set random effect parameters
tau_0 <- 100 # by-subject random intercept sd 
omega_0 <- 80 # by-item random intercept sd

# set more random effect and error parameters
tau_1 <- 40 # by-subject random slope sd
rho <- .2 # correlation between intercept and slope 
sigma <- 200 # residual (error) sd


# set number of subjects and items
n_subj <- 100 # number of subjects 
n_ingroup <- 25 # number of ingroup stimuli 
n_outgroup <- 25 # number of outgroup stimuli


# simulate a sample of items
# total number of items = n_ingroup + n_outgroup 
items <- data.frame(
  item_id = seq_len(n_ingroup + n_outgroup),
  category = rep(c("ingroup", "outgroup"), c(n_ingroup, n_outgroup)), 
  O_0i = rnorm(n = n_ingroup + n_outgroup, mean = 0, sd = omega_0)
)

# effect-code category
items$X_i <- recode(items$category, "ingroup" = -0.5, "outgroup" = +0.5)

# simulate a sample of subjects
# calculate random intercept / random slope covariance
covar <- rho * tau_0 * tau_1
# put values into variance-covariance matrix
cov_mx <- matrix( c(tau_0^2, covar,
                    covar, tau_1^2), nrow = 2, byrow = TRUE)
# generate the by-subject random effects
subject_rfx <- MASS::mvrnorm(n = n_subj,
                             mu = c(T_0s = 0, T_1s = 0),
                             Sigma = cov_mx)
# combine with subject IDs
subjects <- data.frame(subj_id = seq_len(n_subj), subject_rfx)

# simulate a sample of subjects
# sample from a multivariate random distribution
subjects <- faux::rnorm_multi(
  n = n_subj,
  mu = 0, # means for random effects are always 0
  sd = c(tau_0, tau_1), # set SDs
  r = rho, # set correlation, see ?faux::rnorm_multi varnames = c("T_0s", "T_1s")
)

# add subject IDs
subjects$subj_id <- seq_len(n_subj)

# cross subject and item IDs; add an error term # nrow(.) is the number of rows in the table 
trials <- crossing(subjects, items) %>%
  mutate(e_si = rnorm(nrow(.), mean = 0, sd = sigma)) %>% 
  select(subj_id, item_id, category, X_i, everything())

# calculate the response variable
dat_sim <- trials %>%
  mutate(RT = beta_0 + T_0s + O_0i + (beta_1 + T_1s) * X_i + e_si) %>% 
  select(subj_id, item_id, category, X_i, RT)


# set up the custom data simulation function
my_sim_data <- function(
    n_subj = 100, # number of subjects n_ingroup = 25, # number of ingroup stimuli n_outgroup = 25, # number of outgroup stimuli
    beta_0 = 800, # grand mean
    beta_1 = 50, # effect of category
    omega_0 = 80, # by-item random intercept sd
    tau_0 = 100, # by-subject random intercept sd
    tau_1 = 40, # by-subject random slope sd
    rho = 0.2, # correlation between intercept and slope sigma = 200) { # residual (standard deviation)
    items <- data.frame(
      item_id = seq_len(n_ingroup + n_outgroup),
      category = rep(c("ingroup", "outgroup"), c(n_ingroup, n_outgroup)), X_i = rep(c(-0.5, 0.5), c(n_ingroup, n_outgroup)),
      O_0i = rnorm(n = n_ingroup + n_outgroup, mean = 0, sd = omega_0))
    # variance-covariance matrix
    cov_mx <- matrix(
      c(tau_0^2, rho * tau_0 * tau_1,
        rho * tau_0 * tau_1, tau_1^2 ), nrow = 2, byrow = TRUE)
    subjects <- data.frame(subj_id = seq_len(n_subj), MASS::mvrnorm(n = n_subj,
                                                                    mu = c(T_0s = 0, T_1s = 0), Sigma = cov_mx))
    crossing(subjects, items) %>%
      mutate(e_si = rnorm(nrow(.), mean = 0, sd = sigma),
             RT = beta_0 + T_0s + O_0i + (beta_1 + T_1s) * X_i + e_si) %>% select(subj_id, item_id, category, X_i, RT)
    }

# fit a linear mixed-effects model to data
mod_sim <- lmer(RT ~ 1 + X_i + (1 | item_id) + (1 + X_i | subj_id), data = dat_sim)
summary(mod_sim, corr = FALSE)


                    
                    
                    
