set.seed(16)
#========================normal data simulation=====================
easy_odds1 = .8/(1 - .8)
diff_odds1 = .6/(1 - .6)
easy_odds1/diff_odds1
log(easy_odds1/diff_odds1)
#log(codds)
b0 = 0
b1 = 0.9808293
worker_var = 1
task_var = 8
n_worker1 = 40
#n_task1 = 30
# ground truth for images: 50% same, 50% different
# col: workerid, task, num_sample(n_task), y(number_correct)
# expand n_correct, and ground truth 


# large letter
letterwrap <- function(n, depth = 1) {
  args <- lapply(1:depth, FUN = function(x) return(LETTERS))
  x <- do.call(expand.grid, args = list(args, stringsAsFactors = F))
  x <- x[, rev(names(x)), drop = F]
  x <- do.call(paste0, x)
  if (n <= length(x)) return(x[1:n])
  return(c(x, letterwrap(n - length(x), depth = depth + 1)))
}


worker = rep(letterwrap(40), each = 2)
#worker = rep(LETTERS[1:n_worker1], each = 2)
uidworker = paste(worker, rep(1:2, times = n_worker1), sep = "." )
task = rep(c("easy", "difficult"), times = n_worker1)
simulation1 = data.frame(worker, uidworker, task)
simulation1

worker_eff = rep(rnorm(n = n_worker1, 
                       mean = 0, 
                       sd = sqrt(worker_var)), 
                       each = 2) 

#( task_eff = rep( rnorm(n = n_worker1, 
 #                         mean = 0, 
  #                        sd = sqrt(task_var) ), 
   #                 each = 2) )

#log_odds = with(simulation1, b0  + worker_eff ) 
log_odds = with(simulation1, b0 + b1*(task == "easy") + worker_eff ) 
prop = plogis(log_odds)

simulation1$num_task = 50
num_samp = sample(40:50, size = 80, replace = TRUE)
simulation1$y = rbinom(n = n_worker1*2, size = simulation1$num_task, prob = prop)


mod = glmer(cbind(y, num_task - y) ~ task + (1|worker), data = simulation1, family = binomial(link = "logit") )
mod

View(simulation1)
# taskID
easy = rep(letterwrap(50))
task_easy = paste(easy, rep('easy', times = n_worker1), sep = "." )
task_diff = paste(easy, rep('difficult', times = n_worker1), sep = "." )

correctness = c()
taskid = c()
workerid = c()

for (row in 1:nrow(simulation1)){
  correctness = append(correctness, rbernoulli(50, simulation1[row, "y"]/50))
  if (simulation1[row, "task"]=='easy')
    taskid =append(taskid, task_easy)
  else
    taskid =append(taskid, task_diff)
  workerid = append(workerid, rep(simulation1[row, "worker"], 50))
} 

norm_df = data.frame(taskid, workerid, correctness)
View(norm_df)

#colnames(norm_df)[3] = 'correctness'
norm_df$groundtruth = rep(c("same", "different"), times = 2000)


decision = c()
for (row in 1:nrow(norm_df)){
  if (norm_df[row, "correctness"]== "TRUE"){
    decision = append(decision, norm_df[row, "groundtruth"])}
  else{
    if (norm_df[row, "groundtruth"] == "same")
      {decision = append(decision, "different")}
    else
      {decision = append(decision, "same")}
  }
}

norm_df$decision = decision



#===========simulate spamming data==================
easy_odds2 = .5/(1 - .5)
diff_odds2 = .5/(1 - .5)
easy_odds2/diff_odds2
log(easy_odds2/diff_odds2)
#log(codds)
b0 = 0
b1 = 0
worker_var2 = 2
#task_var = 4
n_worker2 = 10
#n_task = 30
# ground truth for images: 50% same, 50% different
# col: workerid, task, num_sample(n_task), y(number_correct)
# expand n_correct, and ground truth 

# small letter
myLetters <- function(length.out) {
  a <- rep(letters, length.out = length.out)
  grp <- cumsum(a == "a")
  vapply(seq_along(a), 
         function(x) paste(rep(a[x], grp[x]), collapse = ""),
         character(1L))
}
myLetters(10)


worker2 = rep(myLetters(10), each = 2)
#worker = rep(LETTERS[1:n_worker1], each = 2)
uidworker2 = paste(worker2, rep(1:2, times = n_worker2), sep = "." )
#task = rep( c("easy", "difficult"), times = n_worker1)
simulation2 = data.frame(worker2, uidworker2)
simulation2$task = task[1:20]
head(simulation2)


worker_eff2 = rep(rnorm(n = n_worker2, 
                        mean = 0, 
                        sd = sqrt(worker_var2) ), 
                        each = 2)


#( task_eff = rep( rnorm(n = n_worker1, 
#                         mean = 0, 
#                        sd = sqrt(task_var) ), 
#                 each = 2) )

log_odds2 = with(simulation2, b0 + b1*(task == "easy") + worker_eff2)
prop2 = plogis(log_odds2)

simulation2$num_task = 50
#num_samp = sample(40:50, size = 16, replace = TRUE)
simulation2$y = rbinom(n = n_worker2*2, size = simulation2$num_task, prob = prop2)
View(simulation2)
hist(simulation2$y)
hist(simulation1$y)



# taskID
#myLetters(10)
#easy2 = rep(myLetters(10))
#task_easy2 = paste(easy2, rep('easy', times = n_worker2), sep = "." )
#task_diff2 = paste(easy2, rep('difficult', times = n_worker2), sep = "." )

correctness2 = c()
taskid2 = c()
workerid2 = c()


for (row in 1:nrow(simulation2)){
  correctness2 = append(correctness2, rbernoulli(50, simulation1[row, "y"]/50))
  if (simulation2[row, "task"]=='easy')
    taskid2 =append(taskid2, task_easy)
  else
    taskid2 =append(taskid2, task_diff)
  workerid2 = append(workerid2, rep(simulation2[row, "worker2"], 50))
} 

length(workerid2)

spam_df = data.frame(taskid2, workerid2, correctness2)
View(spam_df)

#colnames(norm_df)[3] = 'correctness'
spam_df$groundtruth = rep( c("same", "different"), times = 500)

str(spam_df)

decision2 = c()
for (row in 1:nrow(spam_df)){
  if (spam_df[row, "correctness2"]== "TRUE"){
    decision2 = append(decision2, spam_df[row, "groundtruth"])}
  else{
    if (spam_df[row, "groundtruth"] == "same")
    {decision2 = append(decision2, "different")}
    else
    {decision2 = append(decision2, "same")}
  }
}

spam_df$decision = decision2


#combine two dataframe 
colnames(spam_df) = c("taskid", "workerid", "correctness", "groundtruth", "decision")

colnames(simulation2)= c("worker", "uidworker", "task",  "num_task",  "y")

simulated_df = rbind(norm_df, spam_df)
simulated_df = rbind(simulation1, simulation2)
View(simulated_df)

simulated_df$task = ifelse(grepl("easy", simulated_df$taskid), "easy", 'difficult')
norm_df$task = ifelse(grepl("easy", norm_df$taskid), "easy", 'difficult')
spam_df$task = ifelse(grepl("easy", spam_df$taskid), "easy", 'difficult')















mod = glmer(as.factor(decision) ~ task + (1|workerid),data = norm_df,family = binomial(link = "logit") )
mod



mod = glmer(as.factor(decision) ~ (1|taskid) + (1|workerid),data = norm_df,family = binomial(link = "logit") )
mod

bin_glmm_fun = function(n_sites = 10,
                        b0 = 0,
                        b1 = 1.735,
                        num_samp = 50,
                        site_var = 0.5) {
  site = rep(LETTERS[1:n_sites], each = 2)
  plot = paste(site, rep(1:2, times = n_sites), sep = "." )
  treatment = rep( c("treatment", "control"), times = n_sites)
  dat = data.frame(site, plot, treatment)           
  
  site_eff = rep( rnorm(n = n_sites, mean = 0, sd = sqrt(site_var) ), each = 2)
  
  log_odds = with(dat, b0 + b1*(treatment == "treatment") + site_eff)
  prop = plogis(log_odds)
  dat$num_samp = num_samp
  dat$y = rbinom(n = n_sites*2, size = num_samp, prob = prop)
  
  glmer(cbind(y, num_samp - y) ~ treatment + (1|site),
        data = dat,
        family = binomial(link = "logit") )
}


set.seed(16)
bin_glmm_fun()

sims = replicate(1000, bin_glmm_fun(), simplify = FALSE )
sims[[100]]

overdisp_fun = function(model) {
  sum( residuals(model, type = "pearson")^2)/df.residual(model)
}
overdisp_fun(mod)

alldisp = map_dfr(sims, ~data.frame(disp = overdisp_fun(.x) ) )

ggplot(alldisp, aes(x = disp) ) +
  geom_histogram(fill = "blue", 
                 alpha = .25, 
                 bins = 100) +
  geom_vline(xintercept = 1) +
  scale_x_continuous(breaks = seq(0, 2, by = 0.2) ) +
  theme_bw(base_size = 14) +
  labs(x = "Disperson",
       y = "Count")

mean(alldisp$disp > 1)
mean(alldisp$disp > 1.5)


rbinom(100, 1, 0.2)
rbernoulli(10, 0.8)



