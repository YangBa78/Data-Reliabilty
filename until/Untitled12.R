library(lme4)
set.seed(15)

N <- 10    # number of unique values of a and b for each participant
N.participant <- 20   # number of participants
betas <- c(10, 1, 2, 3, 4, 5, 6, 7)   # fixed effects
s <- 0.5   # variance of random effects
e <- 1  # residual variance

# form the balanced data frame
dt <- expand.grid(a = rnorm(N), b = rnorm(N), c = c(0,1), d = c(0,1), time = c(0,1,2), group = c(0,1), participant = LETTERS[1:N.participant])

# form the model matrix for fixed effects
X <- model.matrix(~ a + b + c + d + time + group + time:group, data = dt)

# set a vector of 1s for the outcome. This is needed so we can easily compute the
# model matrix for random effects (in this case just the random intercetps)
dt$y <- 1

# The final model formula
myFormula <- "y ~ a + b + c + d + time + group + time:group + (1 | participant)"

# obtain the model matrix for random effects
foo <- lFormula(eval(myFormula), dt)
Z <- t(as.matrix(foo$reTrms$Zt))

# simulate the random effects
u <- rnorm(N.participant , 0, s)

# simulate y using the general mixed model equation and residual variance e
dt$y <- X %*% betas + Z %*% u + rnorm(nrow(dt), 0, e)

# fit the model
m0 <- lmer(myFormula, dt)
summary(m0)




set.seed(101)
## generate fully crossed design:
d <- expand.grid(Year=2000:2010,Site=1:30)
## sample 70% of the site/year comb to induce lack of balance
d <- d[sample(1:nrow(d),size=round(0.7*nrow(d))),]
## now get Poisson-distributed number of obs per site/year
library(plyr)
d <- ddply(d,c("Site","Year"),transform,rep=seq(rpois(1,lambda=10)))
library(lme4)
d$ticks <- simulate(~1+(1|Year)+(1|Site)+(1|Year:Site),
                    family=poisson,newdata=d,
                    newparams=list(beta=2, ## mean(log(ticks))=2
                                   theta=c(1,1,1)))[[1]]
mm <- glmer(ticks~1+(1|Year)+(1|Site)+(1|Year:Site),
            family=poisson,data=d)
mm

