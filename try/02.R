
symptom <- sample(c("yes","no"), 10000, prob=c(0.02, 0.98), rep=TRUE)
symptom

dataset <- data.frame(symptom, disease=NA)
dataset

dataset$disease[dataset$symptom == "yes"] <- 
  sample(c("yes","no"), sum(dataset$symptom == "yes"), prob=c(0.15, 1-0.15), rep=TRUE)

dataset$disease[dataset$symptom == "no"] <- 
  sample(c("yes","no"), sum(dataset$symptom == "no"), prob=c(0.15, 1-0.05), rep=TRUE)


dst_S_D <-with(dataset, table(symptom, disease)); dst_S_D

library(dplyr)
xtabs(~disease + symptom, data = dataset) %>%
  prop.table() %>%
  round(4) %>%
  addmargins()

0.0213*0.8666
0.0213*0.1334


chisq.test(table(dataset$symptom, dataset$disease), correct = FALSE)


# Generate some data
set.seed(123)
data <- data.frame(x = rnorm(100), y = rnorm(100))

# Generate random duration of task completion
data$duration <- difftime(rnorm(100, mean = 3600, sd = 600), units = "secs")


# Generate some data
set.seed(123)
data <- data.frame(x = rnorm(100), y = rnorm(100))

# Generate random times of task completion
data$time_of_completion <- as.POSIXct(rnorm(100, mean = as.numeric(as.POSIXct("2022-01-01")), sd = 86400), origin = "1970-01-01")



def <- defData(varname = "male", dist = "binary", 
               formula = .5 , id="cid")

dtstudy <- genData(300, def)
dtstudy


table(dtstudy$male)


import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(-6, 6, 1000)
y = np.exp(x) / (1 + np.exp(x))**2
plt.plot(x, y)
plt.xlabel("x")
plt.ylabel("Density")
plt.show()


plot logit function pdf

