library(lme4)

n <- 2400
n_image <- 40
n_worker <- 60

task <-  rep(c(rep("easy", n_worker), rep("hard", n_worker)), n)
task

task_eff



worker <- paste0(rep(letters[23:23], n_image), rep(1:n_image, each = n_worker))
worker_contribution <- rnorm(n_worker, mean = 0, sd = 2)
worker_eff <- rep(worker_contribution, n_image)
  
image <- paste0(rep(LETTERS[9:9], n_worker), rep(1:n_worker, each = n_image))
image_contribution <- rnorm(n_image, mean = 0, sd = 10)
image_eff <- rep(image_contribution, n_worker)

simu.df <- data.frame(task, worker, worker_eff, image, image_eff)
simu.df






