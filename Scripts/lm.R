{for (i in 1:35) {for (j in 1:7) {for (k in 1:2) {
  y[i,j,k] ~ dbern(p[i,j,k])
  logit(p[i,j,k]) <- beta + b.S[i] + b.R[i,j] + b.T[i,j,k]
}
  b.T[i,j,1:2] ~ dmnorm(mean.T[],Tau.T[j,,]) # time-specific random effect
}
  b.R[i,1:7] ~ dmnorm(mean.R[],Tau.R[,]) # rater-specific random effect
  b.S[i] ~ dnorm(0,tau.S) # subject-specific random effect
} 


install.packages('yarrr')
library(yarrr)
data(poopdeck)

time.lm <- lm(formula = time ~ type + cleaner,
              data = poopdeck)


time.II.aov1 <- car::Anova(time.lm, type=1)
summary(time.II.aov)
print(time.lm)

time.I.aov <- aov(time.lm)
summary(time.I.aov)

library("bayesplot")
library("ggplot2")
library("rstanarm")    


install.packages('rstanarm')
head(mtcars) # see help("mtcars")
fit <- stan_glm(mpg ~ ., data = mtcars, seed = 1111)
posterior <- as.array(fit)
plot(posterior)
data.frame(posterior)
posterior + posterior


df0 = as.data.frame(ranef(mod6)$image1)
View(df0)
df0 <- cbind(newColName = rownames(df0), df0)
rownames(df0) <- 1:nrow(df0)
colnames(df0)[0] = 'id'
df_l = list(Prolific, Prolific_value)



