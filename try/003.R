library(psych)
library(ggplot2)
library(lme4)
library(MASS)



# create vector with 40 "easy"s and 40 "different"s
vec <- c(rep("easy", 40), rep("different", 40))

# randomly sample from the vector with total size 80
sample_vec <- sample(vec, 80, replace = TRUE)

# check the frequency of "easy" and "different" in the sample
table(sample_vec)


#
# create vector with 40 "easy"s and 40 "different"s
easy <- rep("easy", 40)
diff <- rep("different", 40)
vec <- c(easy, diff)

# randomly sample 40 "easy"s and 40 "different"s
sample_easy <- sample(easy, 40, replace = TRUE)
sample_diff <- sample(diff, 40, replace = TRUE)

# concatenate the two samples
sample_vec <- c(sample_easy, sample_diff)

# randomly permute the order of the elements in the concatenated sample
sample_vec <- sample(sample_vec)

# check the frequency of "easy" and "different" in the sample
table(sample_vec)




##
## Grouped-data
##
data(pistonrings)
diameter <- qccGroups(data = pistonrings, diameter, sample)

q <- cusum(diameter[1:25,], decision.interval = 4, se.shift = 1)
summary(q)
plot(q)


q <- cusum(diameter[1:25,], newdata=diameter[26:40,])
summary(q)
plot(q, chart.all=FALSE)


##
## Individual observations
##
data(viscosity)
q <- with(viscosity, cusum(viscosity[trial], newdata = viscosity[!trial]))
plot(q)

data(pistonrings)
attach(pistonrings)
diameter <- qcc.groups(diameter, sample)
diameter
(q = cusum(diameter[1:25,], decision.interval = 4, se.shift = 1))
## ── Cusum Chart ─────────────────────────────────── 
## 
## Data (phase I)             = diameter[1:25, ] 
## Number of groups           = 25 
## Group sample size          = 5 
## Center of group statistics = 74.00118 
## Standard deviation         = 0.009785039 
## 
## Decision interval (StdErr) = 4 
## Shift detection   (StdErr) = 1
plot(q)


install.packages("boot",dep=TRUE)
library(boot)
hsb2 <- read.table("https://stats.idre.ucla.edu/stat/data/hsb2.csv", sep=",", header=T)

fc <- function(d, i){
  d2 <- d[i,]
  return(cor(d2$write, d2$math))
}

set.seed(626)
bootcorr <- boot(hsb2, fc, R=500)
bootcorr

