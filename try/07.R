library(brms)
library(tidyverse)
library(lme4)


df = read.csv("~/Dropbox (ASU)/Facewise/DF_Facewise_Airports1.csv")
View(df)

p = glmer(as.factor(Final.Decision) ~ (1 | ID ) + (1 | Pair),      
          data = df, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p)

m <- brm(as.factor(Final.Decision) ~  1+ (1 | ID ) + (1 | Pair),
                          data = df,
                          bernoulli(link = "logit"),
                          #family = 'binomial',
                          warmup = 1000, iter = 3000, 
                          cores = 2, chains = 2, 
                          seed = 123) #to run the model


# Simulate data
set.seed(2022^2)
x <- runif(100, min = 10, max = 100)
y <- 2 + 1.25*x + rnorm(100, mean = 0, sd = 10)
# Randomly pick 10 observations for which to inflate y
to_inflate <- sample(length(y), 10, replace = F)
# Inflate
y[to_inflate] <- y[to_inflate] * 2

# Model
mod <- lm(y ~ x)

mod
# DFBETAS values (placing them in a data frame ensures easy compatibility with ggplot2)
dfbetas_vals <- data.frame(dfbetas(mod))

# Plot
library(ggplot2)
ggplot(dfbetas_vals) +
  geom_point(aes(x = 1:nrow(dfbetas_vals), y = x)) +
  geom_segment(aes(x = 1:nrow(dfbetas_vals), xend = 1:nrow(dfbetas_vals), y = 0, yend = x),
               color = 'cornflowerblue') +
  geom_hline(yintercept = c(2, -2) / sqrt(nrow(dfbetas_vals)), color = 'salmon') +
  labs(x = 'Observation index', y = 'DFBETAS',
       title = paste0('DFBETAS values for coefficient of ', colnames(dfbetas_vals)[2]),
       subtitle = 'Thresholds are at \u00B1(2\u00F7\u221An)') +
  geom_text(aes(x = 1:nrow(dfbetas_vals), y = ifelse(x > 0, x + .05, x - .05),
                label = ifelse(abs(x) > (2/sqrt(nrow(dfbetas_vals))),
                               paste0(round(x, digits = 2), ' (', 1:nrow(dfbetas_vals), ')'), '')))

install.packages("predictmeans")
library(predictmeans)
Oats$nitro <- factor(Oats$nitro)
fm <- lme(yield ~ nitro*Variety, random=~1|Block/Variety, data=Oats)
# library(lme4)
fm <- lmer(yield ~ nitro*Variety+(1|Block/Variety), data=Oats)
CookD(fm)


library(lavaan)
data("PDII")
model <- "
  F1 =~ y1+y2+y3+y4
"
# fit0 <- sem(model, data=PDII)
# LD <-Likedist(model,data=PDII)
# plot(LD,pch=19,xlab="observations",ylab="Likelihood distances")

## not run: this example take several minutes
## an example in which the deletion of a case yelds a solution 
## with negative estimated variances
model <- "
  F1 =~ x1+x2+x3
  F2 =~ y1+y2+y3+y4
  F3 =~ y5+y6+y7+y8
"

# fit0 <- sem(model, data=PDII)
# LD <-Likedist(model,data=PDII)
# plot(LD,pch=19,xlab="observations",ylab="Likelihood distances")