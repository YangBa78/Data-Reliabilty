# Set the number of workers and tasks
n_workers <- 10
n_tasks <- 20

# Set the probability of value being 1
p_value_1 <- 0.5

# Set the variances for the random effects
worker_var <- 1
task_var <- 1
interaction_var <- 1

# Create the worker and task vectors
workers <- sample(1:n_workers, n_tasks, replace = TRUE)
tasks <- 1:n_tasks

# Generate the random effect vectors using the normal distribution
worker_effect <- rnorm(n_workers, sd = sqrt(worker_var))
task_effect <- rnorm(n_tasks, sd = sqrt(task_var))
interaction_effect <- rnorm(n_tasks, sd = sqrt(interaction_var))

# Generate the value vector using the Bernoulli distribution
value <- rbinom(n_tasks, 1, p_value_1)

# Combine the worker, task, and value vectors into a data frame
df <- data.frame(worker = workers, task = tasks, value = value)

# Add the random effect vectors to the data frame
df$worker_effect <- worker_effect[df$worker]
df$task_effect <- task_effect[df$task]
df$interaction_effect <- interaction_effect

# Fit the mixed-effects model using lme4
library(lme4)
model <- lmer(value ~ worker_effect + task_effect + interaction_effect + (1 | worker) + (1 | task), data = df)

# Print the summary of the model
summary(model)


head(df)
