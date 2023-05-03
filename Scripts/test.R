# Data
Y1 <- rnorm(100,1,1.5)
X1 <- rnorm(100,1,1.5)
X2 <- rnorm(100,1,1.5)

data<-cbind.data.frame(Y1,X1,X2)

library(car)
install.packages("car")

# VIF
m1<-lm(Y1~X1+X2, data)
car::vif(m1)

# Invert Correlation Matrix
solve(cor(data[2:3]))

cor(data[2:3])


library("modelbased")
random <- estimate_grouplevel(fm1)
reshaped <- reshape_grouplevel(random, indices = c("Coefficient", "SE"))



library(lme4)
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
lattice::dotplot(ranef(mod6, condVar=TRUE))
rr <- ranef(fm1)
aa <- attr(rr$Subject, "postVar")
sqrt(c(t(apply(aa, 3, diag))))
summary(fm1)
dd <- as.data.frame(rr)
transform(dd, lwr = condval - 1.96*condsd, upr = condval + 1.96*condsd)


no_pool <- lmList(bounce_time ~ std_age|county, data = bounce_data)
library(tidyverse)
bounce_data <- bounce_data %>% 
  mutate(std_age = scale(age)[,1]) %>% 
  relocate(std_age, .after=age)

2.2585509/12.07086
pnorm(0.1871077, )
2*pnorm(q=0.1871077, lower.tail=FALSE)
