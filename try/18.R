nworker= 30 # number of pupils per school
nimage = 50 # number of schools
#npupilstotal = npupils*nschools # to give each pupil a unique number

# 1.1 Design data frame:
simdf = data.frame("workerid" = rep(1:nworker,
                                    each = nimage),
                   "imageid" = rep(1:nimage,
                                   nworker))
# task
easy <- rep("easy", nimage/2)
hard <- rep("hard", nimage/2)
sample_task <- c(easy, hard)
simdf$task <- sample(sample_task)

simdf

# GT 
same <- rep("same", nimage/2)
diff <- rep("different", nimage/2)
sample_vec <- c(same, diff)
sample_same <- sample(sample_vec)
sample_diff <- sample(sample_vec)


simdf$trueLable[simdf$task=='easy'] <- sample_same
simdf$trueLable[simdf$task=='hard'] <- sample_diff


df = simdf 
# random guess

df$decision <- NA
#newT$correct <- NA
df[df$workerid ==1, ][df[df$workerid ==1, ]$trueLable=='same',]$decision <- 
  sample(c(0,1), dim(df[df$workerid ==1, ][df[df$workerid ==1, ]$trueLable=='same',])[1], prob=c(0.3, 1-0.3), rep=TRUE)
df[df$workerid ==1, ][df[df$workerid ==1, ]$trueLable=='different',]$decision <- 
  sample(c(0,1), dim(df[df$workerid ==1, ][df[df$workerid ==1, ]$trueLable=='different',])[1], prob=c(0.3, 1-0.3), rep=TRUE)


#newT$correct <- NA
df[df$workerid ==2, ][df[df$workerid ==2, ]$task=='easy',]$decision <- 
  sample(c(0,1), dim(df[df$workerid ==2, ][df[df$workerid ==2, ]$task=='easy',])[1], prob=c(0.5, 1-0.5), rep=TRUE)
df[df$workerid ==2, ][df[df$workerid ==2, ]$task=='hard',]$decision <- 
  sample(c(0,1), dim(df[df$workerid ==2, ][df[df$workerid ==2, ]$task=='hard',])[1], prob=c(0.5, 1-0.5), rep=TRUE)

# repeat pattern
df[df$workerid==3,]$decision <- rep(c(0,1), times = nimage/2)  

df[df$workerid==4,]$decision <- rep(c(1,0), times = nimage/2)  


df$correct = NA

df[df$workerid==5,]$decision = rbinom(nimage,1,0.5)
  

for (x in 6: nworker){
  r = runif(1, 0.75, 0.9)
  r_minus = runif(1, 0.1, 0.2)
  #r = rnorm(1, 0.75, 0.1)
  df[df$workerid ==x, ][df[df$workerid ==x, ]$task=='easy',]$correct<- 
    sample(c(1,0), dim(df[df$workerid ==x, ][df[df$workerid ==x, ]$task=='easy',])[1], prob=c(r, 1-r), rep=TRUE)
  df[df$workerid ==x, ][df[df$workerid ==x, ]$task=='hard',]$correct<- 
    sample(c(1,0), dim(df[df$workerid ==x, ][df[df$workerid ==x, ]$task=='hard',])[1], prob=c(r-0.1, 1-r+0.1), rep=TRUE)
}

df


df$convert = ifelse(df$trueLable=='different', 1, 0)

for (x in 6: nworker){
  df[df$workerid==x,]$decision = ifelse(df[df$workerid==x,]$correct==1, df[df$workerid==x,]$convert, 1- df[df$workerid==x,]$convert)
}


for (x in 1:5){
  df[df$workerid==x,]$correct = ifelse(df[df$workerid==x,]$decision== df[df$workerid==x,]$convert, 1, 0)
}

p <- glmer(as.factor(decision) ~  (1 | workerid) + (1 | imageid) ,   
           data = df, family = binomial)
summary(p)


0.03/(0.03+0.315)
# [1] 0.08695652.   -  high quality

table(df$workerid, df$decision)
table(df$imageid, df$decision)



#####################
acc = df %>% group_by(workerid) %>% summarise(accuracy = sum(correct)/(nimage), 
                                              .groups = 'drop') %>%as.data.frame()


wid = data.frame(ranef(p)$workerid)
wid <- cbind(newColName = rownames(wid), wid)
rownames(wid) <- 1:nrow(wid)
colnames(wid) = c('workerid', 'worker_eff')

wid$workerid = as.integer(wid$workerid)
df = df%>% left_join(wid, by='workerid')


iid = data.frame(ranef(p)$imageid)
iid <- cbind(newColName = rownames(iid), iid)
rownames(iid) <- 1:nrow(iid)
colnames(iid) = c('imageid', 'image_eff')

iid$imageid = as.integer(iid$imageid )
df = df%>% left_join(iid, by='imageid')


df$re = df$image_eff + df$worker_eff
df$p = 1/(1+(exp(-df$re)))

df$ratio = df$image_eff / df$re
df$p_1 = 1/(1+(exp(-df$image_eff)))



plot(df$image_eff, df$p)

cor(abs(df$ratio), df$p)

plot(abs(df$ratio), df$p)

ggplot(df[df$workerid==11,], aes(image_eff)) +
  geom_line(aes(y=p), colour="black") + 
  geom_line(aes(y=correct), colour="red")


sum(df[df$ID==355,]$image_eff>unique(df[df$ID==355,]$worker_eff))

sum(ifelse(abs(df[df$ID==119,]$re)<0.1, 1, 0))/dim(df[df$ID==119,])[1]
sum(ifelse(abs(df[df$ID==234,]$re)<0.1, 1, 0))/dim(df[df$ID==234,])[1]
sum(ifelse(abs(df[df$ID==355,]$re)<0.1, 1, 0))/dim(df[df$ID==355,])[1]
sum(ifelse(abs(df[df$ID==234,]$re)<0.1, 1, 0))/dim(df[df$ID==234,])[1]


which(int_eff== - min(abs(int_eff)))


ggplot(ddd, aes(image_eff)) +
  geom_line(aes(y=p_i), colour="black") +  # accuracy 
  geom_line(aes(y=p1), colour="red") +  # 0.7333
  geom_line(aes(y=p2), colour="green") + # 0.7
  geom_line(aes(y=p3), colour="blue") + # 0.6
  geom_line(aes(y=p4), colour="pink") + # 0.666
  geom_line(aes(y=p5), colour="brown") + # 0.9
  geom_line(aes(y=p6), colour="yellow") + #+ # 0.73
  #geom_vline(xintercept = -0.9413365, linetype="dashed") + 
  #geom_vline(xintercept = -0.2652010, linetype="dashed") + 
  #geom_vline(xintercept = 0.4848510, linetype="dashed") 
  geom_abline(slope = coef(l1)[["temp$image_eff"]], 
              intercept = coef(l1)[["(Intercept)"]], colour = 'black') + 
  geom_abline(slope = coef(l1)[["temp$image_eff"]], 
              intercept = coef(l1)[["(Intercept)"]], colour = 'black') + 
  geom_abline(slope = coef(l1)[["temp$image_eff"]], 
              intercept = coef(l1)[["(Intercept)"]], colour = 'black')



x <- c('aa', 'bb', 'cc', 'dd')
y <- c('aa', 'b', 'cc', 'dd')

sum(x!=y)





