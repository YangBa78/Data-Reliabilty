######## simulation
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
  #print(sum(diag(table(simul[simul$taskid==i, ]$decision, simul[simul$taskid==i, ]$label)))/50)
  #dis<- append(dis, sum(diag(table(simul[simul$taskid==i, ]$decision, simul[simul$taskid==i, ]$label)))/50)
  dis <- append(dis, min(table(simul[simul$taskid==i, ]$decision))/sum(table(simul[simul$taskid==i, ]$decision)))
}


dis = ifelse(dis==1, 0, dis)
plot(sort(dis))

# check correctness: easy vs difficult 
min(table(simul[simul$task=='easy', ]$decision))/sum(table(simul[simul$task=='easy', ]$decision))
min(table(simul[simul$task=='difficult', ]$decision))/sum(table(simul[simul$task=='difficult', ]$decision))





#w = c()
#for (i in 1:n_worker) {
  #print(sum(simu$decision[simu$wokerid==i])/30)
 # w = append(w, sum(simu$decision[simu$wokerid==i])/30)}

#plot(sort(w))

fit = glmer(
  data = simul
  , formula = decision ~ (1|wokerid) + (1|taskid) + (1|wokerid:taskid)
  , family = binomial
)
print(fit)


