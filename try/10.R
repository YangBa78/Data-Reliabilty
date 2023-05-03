set.seed(1234) #for reproducability
nG <- 20 #number of groups
nJ <- 30 #cluster size
W1 <- 2 #level 2 coeff
X1 <- 3 #level 1 coeff

tmp2 <- rnorm(nG) #generate 20 random numbers, m = 0, sd = 1
l2 <- rep(tmp2, each = nJ) #all units in l2 have the same value
group <- gl(nG, k = nJ) #creating cluster variable
tmp2 <- rnorm(nG) #error term for level 2
err2 <- rep(tmp2, each = nJ) #all units in l2 have the same value

l1 <- rnorm(nG * nJ) #total sample size is nG * nJ
err1 <- rnorm(nG * nJ) #level 1 

#putting it all together
y <- W1 * l2 + X1 * l1 + err2 + err1
dat <- data.frame(y, group, l2, err2,l1, err1)


library(lme4) #to run multilevel models
library(jtools) #to get nicer output
mlm0 <- lmer(y ~ (1|group), data = dat) #unconditional
summ(mlm0) #shows the ICC, close


mlm1 <- lmer(y ~ l2 + l1  + (1|group), data = dat)
ols1 <- lm(y ~ l2 + l1, data = dat)

stargazer::stargazer(mlm1, ols1, type = 'text', no.space = T, star.cutoffs = c(.05,.01,.001))

summ(mlm0, cluster = 'group', robust = T)
