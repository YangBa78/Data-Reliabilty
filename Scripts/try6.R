Sigma <- matrix(c(10,3,3,2),2,2)
Sigma
var(mvrnorm(n = 1000, rep(0, 2), Sigma))
var(mvrnorm(n = 1000, rep(0, 2), Sigma, empirical = TRUE))


temp = expand.grid(A=c('a1','a2'),value=0)
temp$A = factor(temp$A)
contrasts(temp$A) = contr.sum
from_terms = terms(value~A)
mm = model.matrix(from_terms,temp)


sum(rbinom(500, size=1, prob=.9))



data = expand.grid(unit=1:n,trial=1:k)


sium = expand.grid(wokerid=1:50,taskid=1:50)
sium$task = ifelse(sium$taskid<=25, 'easy', 'difficult')

mu <- c(0, 4, 6, 10)
df <- sim_design(between = list(time = c("am", "pm"),
                                pet = c("cat", "dog")),
                 n = c(50, 60, 70, 80), mu = mu, empirical = TRUE)



head(sium)
n_worker1 = 10
worker_var = 5

worker_eff = rnorm(n = 50, 
                       mean = 0, 
                       sd = sqrt(worker_var))

#( task_eff = rep( rnorm(n = n_worker1, 
#                         mean = 0, 
#                        sd = sqrt(task_var) ), 
#                 each = 2) )

#log_odds = with(simulation1, b0  + worker_eff ) 
log_odds = with( , worker_eff ) 
prop = plogis(log_odds)


simulation1$num_task = 50
num_samp = sample(40:50, size = 80, replace = TRUE)
simulation1$y = rbinom(n = n_worker1*2, size = simulation1$num_task, prob = prop)

########
n_worker = 50
n_task = 30
n_worker_task = n_worker * n_task
worker_var = 1
task_var = 10
task_worker_var = 2

worker_eff = rnorm(n = n_worker, 
                   mean = 0, 
                   sd = sqrt(worker_var))

task_eff = rnorm(n = n_task, 
                   mean = 0, 
                   sd = sqrt(task_var))

task_worker_eff = rnorm(n = n_worker_task, 
                 mean = 0, 
                 sd = sqrt(task_worker_var))

simul = expand.grid(wokerid=1:n_worker,taskid=1:n_task)
simul$task = ifelse(simul$taskid<=15, 'easy', 'difficult')
head(simul)


for (i in 1:n_worker) {
  for(j in 1:n_task)
  {  log_odds = worker_eff[i] + task_eff[j] + task_worker_eff[i*j]
    simul$decision[simul$wokerid==i & simul$taskid==j] = rbinom(1, 1, plogis(log_odds))
  }
}

# set GT, majority voting
for (i in 1:n_task){
  simul$label[simul$taskid==i] = ifelse(sum(simul$decision[simul$taskid==i])/50>0.5, 1, 0)
}

#####################


acc = c()
for (i in 1:n_worker) {
   #print(sum(simu$decision[simu$taskid==i])/50)
  
  acc = append(acc, sum(diag(table(simul[simul$wokerid==i, ]$decision, simul[simul$wokerid==i, ]$label)))/30)
}

plot(sort(acc))

# disagree rate for worker
dis = c()
for (i in 1:n_task){
  dis <- append(dis, min(table(simul[simul$taskid==i, ]$decision))/sum(table(simul[simul$taskid==i, ]$decision)))
}

plot(sort(dis))

# check correctness: easy vs difficult 
min(table(simul[simul$task=='easy', ]$decision))/sum(table(simul[simul$task=='easy', ]$decision))
min(table(simul[simul$task=='difficult', ]$decision))/sum(table(simul[simul$task=='difficult', ]$decision))


w = c()
for (i in 1:n_worker) {
  #print(sum(simu$decision[simu$wokerid==i])/30)
  w = append(w, sum(simu$decision[simu$wokerid==i])/30)
}

plot(sort(w))

fit = glmer(
  data = simul
  , formula = decision ~ (1|wokerid) + (1|task/taskid) + (1|wokerid:taskid)
  , family = binomial
)
print(fit)


for (i in )




