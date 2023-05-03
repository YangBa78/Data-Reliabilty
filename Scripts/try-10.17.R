p1 = glmer(error ~ (1 | WorkerId)  + (1 | image1) + (1 | task: image1), 
           data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p1)


p2 = glmer(error ~ (1 | WorkerId) + (1 | task/image1), 
           data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p2)


p2.2 = glmer(error ~ (1 | WorkerId) + (1 | task) + (1 |image1), 
           data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p2.2)


p3 = glmer(error ~ (1 | WorkerId) + (1 | image1), 
           data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p3)


p4 = glmer(error ~ (1 | WorkerId) + (1 |image1/ task), 
           data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p4)



p5 = glmer(error ~ (1 | WorkerId)  + (1 | task/image1), 
           data = Prolific, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(p5)


mod_m = glmer(decision ~ (1 | assgID) + (1 | task/TID),  
              data = Mturk, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)), nAGQ = 1)
summary(mod_m)


betahat <- coef(p2)
S <- vcov(p2)
betasamp <- MASS::mvrnorm(1, mu=betahat, Sigma=S)
intsamp <- betasamp[1]; slopesamp <- betasamp[2]


h = p2@optinfo$derivs$Hessian
v = forceSymmetric(solve(p2@optinfo$derivs$Hessian) + t(p2@optinfo$derivs$Hessian))

all.equal(unname(as.matrix(vcov(p2))),
          unname(as.matrix(v)[-1,-1])) ## TRUE
vcov(p2)


#variance -covariance matrix
v = VarCorr(p2)
a = as.matrix(Matrix::bdiag(v))
tr(a)



trace <- function(A) {
  n <- dim(A)[1] # get dimension of matrix
  tr <- 0 # initialize trace value
  
  # Loop over the diagonal elements of the supplied matrix and add the element to tr
  for (k in 1:n) {
    l <- A[k,k]
    tr <- tr + l
  }
  return(tr[[1]])
}


library(GLMMadaptive)


devtools::install_github("drizopoulos/GLMMadaptive")

xtabs(~ image1 + task, Prolific)




# define trace function
trace <- function(A) {
  n <- dim(A)[1] # get dimension of matrix
  tr <- 0 # initialize trace value
  
  # Loop over the diagonal elements of the supplied matrix and add the element to tr
  for (k in 1:n) {
    l <- A[k,k]
    tr <- tr + l
  }
  return(tr[[1]])
}

# trace
p.t = trace(v.matrix)

