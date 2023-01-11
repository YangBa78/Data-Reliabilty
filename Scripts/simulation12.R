# create a high quality dataset
n.part <- 20  # number of parts
n.oper <- 20  # number of opers
n.reps <- 2   # number of replications

dt <- expand.grid(part = LETTERS[1:n.part], oper = 1:n.oper, reps = 1:n.reps)

dt$Y <- 10 + rnorm(n.part*n.oper*n.reps)

myformula <- "Y ~ (1|part) + (1|oper) + (1|part:oper)"  # model formula

mylF <- lFormula(eval(myformula), data = dt) # Process the formula against the data
Z <- mylF$reTrms$Zt %>% as.matrix() %>% t()  # Extract the Z matrix




library(MASS)
library(lme4)

generate_data = function(
    n # number of units
    , k # number of trials within each condition within each unit
    , I # population intercept
    , vI # across-units variance of intercepts
    , A # population A effect
    , vA # across-units variance of A effects
    , rIA # across-units correlation between intercepts and A effects
){
  Sigma = c(
    vI , sqrt(vI*vA)*rIA
    , sqrt(vI*vA)*rIA , vA
  )
  Sigma = matrix(Sigma,2,2)
  means = mvrnorm(n,c(I,A),Sigma)
  temp = expand.grid(A=c('a1','a2'),value=0)
  temp$A = factor(temp$A)
  contrasts(temp$A) = contr.sum
  from_terms = terms(value~A)
  mm = model.matrix(from_terms,temp)
  data = expand.grid(A=c('a1', 'a2'),unit=1:n,trial=1:k)
  for(i in 1:n){
    data$value[data$unit==i] = rbinom(k*2,1,plogis(as.numeric(mm %*% means[i,])))
  }
  data$unit = factor(data$unit)
  data$A = factor(data$A)
  contrasts(data$A) = contr.sum
  return(data)
}


this_data = generate_data(
  n = 100 # number of units
  , k = 100 # number of trials within each condition within each unit
  , I = 2 # population intercept
  , vI = .1 # across-units variance of intercepts
  , A = .5 # population A effect
  , vA = .2 # across-units variance of A effects
  , rIA = .6 # across-units correlation between intercepts and A effects
)

fit = glmer(
  data = this_data
  , formula = value ~ (1+A|unit) + A 
  #formula = value ~ (1 |unit) + (1 |trial) + (1 |unit:trial)
  , family = binomial
)
print(fit)


l = c()
for(i in 1:100){
  l = append(l, sum(this_data[this_data$trial==i, ]$value))
}

l1 = c()
for(i in 1:100){
  l1 = append(l1, sum(this_data[this_data$unit==i, ]$value))
}


n_subj     = 100 # number of subjects
n_ingroup  =  25  # number of faces in ingroup
n_outgroup =  25  # number of faces in outgroup
beta_0     =   0 # intercept
beta_1     =   0 # effect of category
omega_0    =   1 # by-item random intercept sd
tau_0      =   1 # by-subject random intercept sd
tau_1      =   1 # by-subject random slope sd
rho        =   0 # correlation between intercept and slope

subjects <- faux::rnorm_multi(
    n = n_subj, 
    mu = 0,
    sd = c(tau_0, tau_1),
    r = rho, 
     varnames = c("T_0s", "T_1s")
)

items <- data.frame(
  item_id = 1:(n_ingroup + n_outgroup),
  category = rep(c("ingroup", "outgroup"), 
                 c(n_ingroup, n_outgroup)),
  O_0i = rnorm(n_ingroup + n_outgroup, 0, omega_0)
)
subjects$subj_id <- 1:n_subj

crossing(subjects, items)  %>%
  mutate(
    # calculate gaussian DV
    Y = beta_0 + T_0s + O_0i + (beta_1 + T_1s) * X_i,
    pr = inv_logit(Y), # transform to probability of getting 1
    Y_bin = rbinom(nrow(.), 1, pr) # sample from bernoulli distribution
  ) 





# create a low quality dataset
