data("Exam", package = "mlmRev")
head(Exam)

fm1 <- lme4::lmer(normexam ~ standLRT + (1 | school), Exam, REML = FALSE)
fm1

library(HLMdiag)
hlm_resid(p)
p
0

#### Logistic regression 

r=c(10,17,12,7,23,22,29,29,23)
n=c(31,30,31,27,26,30,31,30,30)
logconc=c(2.68,2.76,2.82,2.90,3.02,3.04,3.13,3.20,3.21)
counts=cbind(r,n-r)
result=glm(counts~logconc,family=binomial("logit"))
summary(result,correlation=TRUE,symbolic.cor = TRUE)
result$coefficients

#### plot residuals vs. linear predictor 

plot(residuals(result, type="pearson"),result$linear.predictors)

#### plot logconc vs. empirical logits 

emplogit=log((r+0.5)/(n-r+0.5))
plot(logconc,emplogit)
plot(result)

std(r)


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

