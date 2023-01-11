# set up the custom data simulation function
my_bin_data <- function(
    n_worker     = 100, # number of subjects
    n_easy  =  25,  # number of faces in ingroup
    n_hard =  25,  # number of faces in outgroup
    beta_0     =   0.5, # intercept
    omega_0    =   1, # by-image random intercept sd
    tau_0      =   1 # by-worker random intercept sd
) {
  # simulate a sample of items
  image <- data.frame(
    image_id = 1:(n_easy + n_hard),
    category = rep(c("easy", "hard"), 
                   c(n_easy, n_hard)),
    O_0i = rnorm(n_easy + n_hard, 0, omega_0)
  )
  
  # effect code category
  image$X_i <- recode(items$category, 
                      "easy" = -0.5, 
                      "hard" = 0.5)
  
  # simulate a sample of subjects
  worker <- faux::rnorm_multi(
    n = n_worker, 
    mu = 0,
    sd = tau_0,
    varnames = "T_0s"
  )
  worker$worker_id <- 1:n_worker
  
  # cross subject and item IDs 
  crossing(worker, image)  %>%
    mutate(
      # calculate gaussian DV
      Y = beta_0 + T_0s + O_0i, #+ (beta_1 + T_1s) * X_i,
      pr = inv_logit(Y), # transform to probability of getting 1
      Y_bin = rbinom(nrow(.), 1, pr) # sample from bernoulli distribution
    ) %>%
    select(worker_id, image_id, category, Y, Y_bin)
}

dat_sim <- my_bin_data()
head(dat_sim)




