data(gpa)
View(sleepstudy)
install.packages("rstan", type = "source")



# if you dont have these packages installed yet, please use the install.packages("package_name") command.
library(tidyverse) # for data manipulation and plots
library(haven) #for reading sav data
library(sjstats) #for calculating intra-class correlation (ICC)
library(ROCR) #for calculating area under the curve (AUC) statistics
library(brms) #for Bayesian (multilevel) generalised linear modelling
library(modelr) #for data manipulation
library(tidybayes) #for analysis of posterior draws of a Bayesian model
install.packages('brms')


p1 <- prior("norm", c(mean = 0, sd = .3), lower = 0)
p1
p1(c(-1, 1, 3))
plot(p1, -.1, 1)

if(!require('metaBMA')) {
  install.packages('metaBMA')
  library('metaBMA')
}


bayes_t = brm(
  decision ~ (1 | WorkerId) + (1 | image1) + (1 | image1:WorkerId),
  data = Prolific, 
  family = bernoulli(link = "logit"),
  prior = c(set_prior("student_t(4, 0, 1)", class = "sd"), 
            #set_prior("beta(5, 5)", class = "sd", group='task:image1'),
            set_prior("beta(5, 5)", class = "sd", group='image1')),
  iter = 2000, 
  chains = 2, 
  cores=4, 
  control=list(adapt_delta=0.99))



bayes_m = brm(
      decision ~ (1 | assgID) + (1 | TID)  + (1 | assgID:TID)  + (1 | assgID:task) + (1 | task/TID),
      data = Mturk, 
      family = bernoulli(link = "logit"),
      prior = c(set_prior("student_t(4, 0, 1)", class = "sd"), 
                set_prior("beta(5, 5)", class = "sd", group='task:TID'),
                set_prior("beta(5, 5)", class = "sd", group='TID')),
      iter = 4000, 
      chains = 4, 
      cores=4, 
      control=list(adapt_delta=0.99))


bayes_p = brm(
  decision ~ (1 | WorkerId) + (1 | image1) + (1 | image1:WorkerId) + (1 | task:WorkerId)  + (1 | task/image1),
  data = Prolific, 
  family = bernoulli(link = "logit"),
  prior = c(set_prior("student_t(4, 0, 1)", class = "sd"), 
            set_prior("beta(5, 5)", class = "sd", group='task:image1'),
            set_prior("beta(5, 5)", class = "sd", group='image1')),
  iter = 4000, 
  chains = 4, 
  cores=4, 
  control=list(adapt_delta=0.99))



seed = 123







# get_prior
prior <- get_prior(error ~ (1 | WorkerId) + (1 | image1) + (1 | image1:WorkerId) + (1 | task:WorkerId)  + (1 | task/image1), 
                         data = Prolific, 
                         family = bernoulli(link = "logit"))

prior
prior$prior[4] =  set_prior("cauchy(0,2)")


fit1 <- brm(formula = time | cens(censored) ~ age * sex + disease 
            + (1 + age | patient), 
            data = kidney, family = lognormal(),
            prior = c(set_prior("normal(0,1)", class = "sd", group='Worker_id'),
                      set_prior("cauchy(0,2)", class = "sd", group=''),
            warmup = 1000, iter = 2000, chains = 4,
            control = list(adapt_delta = 0.99))

bayes_m$ranef
posterior_summary(bayes_m)


