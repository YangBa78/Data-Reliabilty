## test whether fitted models are consistent with the
##  observed number of zeros in CBPP data set:
library(lme4)
gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
             data = cbpp, family = binomial)
gg <- simulate(gm1,1000)
zeros <- sapply(gg,function(x) sum(x[,"incidence"]==0))
plot(table(zeros))
abline(v=sum(cbpp$incidence==0),col=2)
##
## simulate from a non-fitted model; in this case we are just
## replicating the previous model, but starting from scratch
params <- list(theta=0.5,beta=c(2,-1,-2,-3))
simdat <- with(cbpp,expand.grid(herd=levels(herd),period=factor(1:4)))
simdat$size <- 15
simdat$incidence <- sample(0:1,size=nrow(simdat),replace=TRUE)
form <- formula(gm1)[-2]  ## RHS of equation only
simulate(form,newdata=simdat,family=binomial,newparams=params)
## simulate from negative binomial distribution instead
simulate(form,newdata=simdat,family=negative.binomial(theta=2.5),
         newparams=params)

cbind(cbpp$incidence, cbpp$size-cbpp$incidence)

gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
             data = cbpp, family = binomial)

cbpp$gt = cbpp$incidence/cbpp$size

gm2 <- glmer(gt ~ period + (1 | herd), data = cbpp, family = binomial)
summary(gm2)


library(dplyr)
df <- tibble(treatment_status = c("treatment", "no_treatment"),
             disease = c(55, 42),
             no_disease = c(67,34)) %>% 
  mutate(total = no_disease + disease,
         proportion_disease = disease / total) 

model_weighted <- glm(proportion_disease ~ treatment_status, data = df, family = binomial("logit"), weights = total)

df_expanded <- tibble(disease_status = c(1, 1, 0, 0), 
                      treatment_status = rep(c("treatment", "control"), 2)) %>%
  .[c(rep(1, 55), rep(2, 42), rep(3, 67), rep(4, 34)), ]

model_expanded <- glm(disease_status ~ treatment_status, data = df_expanded, family = binomial("logit"))



set.seed(15)
n.part <- 20  # number of parts
n.oper <- 20  # number of opers
n.reps <- 2   # number of replications

dt <- expand.grid(part = LETTERS[1:n.part], oper = 1:n.oper, reps = 1:n.reps)

dt$Y <- 10 + rnorm(n.part*n.oper*n.reps)

myformula <- "Y ~ (1|part) + (1|oper) + (1|part:oper)"  # model formula

mylF <- lFormula(eval(myformula), data = dt) # Process the formula against the data
Z <- mylF$reTrms$Zt %>% as.matrix() %>% t()  # Extract the Z matrix

b1 <- rnorm(n.part * n.oper, 0 , 4)   # random interecepts for the interaction
b2 <- rnorm(n.oper, 0, 3)             # random interecepts for oper
b3 <- rnorm(n.part, 0, 2)             # random interecepts for part

b <- c(b1, b2, b3)  

dt$Y <- 10 + Z %*% b + rnorm(nrow(dt))
lmer(eval(myformula), data = dt ) %>% summary()



set.seed(15)
n.part <- 20  # number of parts
n.oper <- 20  # number of opers
n.reps <- 2   # number of replications

dt <- expand.grid(part = LETTERS[1:n.part], oper = 1:n.oper, reps = 1:n.reps)

dt$Y <- 10 + rnorm(n.part*n.oper*n.reps)

myformula <- "Y ~ (1|part) + (1|oper) + (1|part:oper)"  # model formula

mylF <- lFormula(eval(myformula), data = dt) # Process the formula against the data
Z <- mylF$reTrms$Zt %>% as.matrix() %>% t()  # Extract the Z matrix
