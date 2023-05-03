# Set seed for reproducibility
set.seed(123)

# Number of workers and tasks
n_worker <- 10
n_task <- 5

# Generate random effects for workers and tasks
worker_effect <- rnorm(n_worker, sd = sqrt(2))
task_effect <- rnorm(n_task, sd = sqrt(6))

# Simulate binary response for workerID == 1
workerID <- rep(1, n_task)
response <- rbinom(n_task, size = 1, prob = plogis(worker_effect[workerID] + task_effect))

# Print the simulated response for workerID == 1
print(response)


# Set seed for reproducibility
set.seed(123)

# Number of workers and tasks
n_worker <- 10
n_task <- 5

# Generate random effects for workers and tasks
worker_effect <- rnorm(n_worker, sd = sqrt(2))
task_effect <- rnorm(n_task, sd = sqrt(6))

# Set worker effect for workerID == 1 such that the sum of worker and task effects results in alternating 0s and 1s
worker_effect[1] <- -mean(task_effect) + ifelse(rep(c(0, 1), length.out = n_task), qlogis(0.75), qlogis(0.25))

# Simulate binary response for workerID == 1
workerID <- rep(1, n_task)
response <- rbinom(n_task, size = 1, prob = plogis(worker_effect[workerID] + task_effect))

# Print the simulated response for workerID == 1
print(response)

n <- 10

# Generate predictor variable
x <- rnorm(n)

# Set linear predictor such that it results in alternating 0s and 1s
eta <- ifelse(rep(c(0, 1), length.out = n), qlogis(0.75), qlogis(0.25))

response <- rbinom(n, size = 1, prob = plogis(eta))
