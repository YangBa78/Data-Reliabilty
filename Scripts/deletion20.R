library(influence.ME)
simulated_df = simul

p = glmer(
  data = simulated_df
  , formula = decision ~ (1|wokerid) + (1|taskid) + (1|wokerid:taskid)
  , family = binomial
)
print(p)

colnames(simulated_df)
colnames(simulated_df)[1] <- 'workerid'
colnames(simulated_df)[2] <- 'imageid'


p = glmer(
  data = simulated_df
  , formula = decision ~ (1|workerid) + (1|imageid) + (1|workerid:imageid)
  , family = binomial
)
print(p)

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


p.vcov = VarCorr(p)
v.matrix = as.matrix(Matrix::bdiag(p.vcov))
rankMatrix(v.matrix )[1]  # rank of matrix
# another method for trace
p.t = sum(data.frame(VarCorr(p))$vcov)


p.var.image = data.frame(VarCorr(p))$vcov[3] / p.t
p.var.imageint = (data.frame(VarCorr(p))$vcov[1] + data.frame(VarCorr(p))$vcov[3]) /p.t

#################### worker 

workerid <- c()
loglik <- c()
total.var <- c()
var.image <- c()
varint.image <- c()
vartrace <- c()
vartrace2 <- c()
vartrace3 <- c()
total.var.change <- c()
var.image.change <- c()
varint.image.change <- c()
accuracy <- c()


for (x in unique(simulated_df$workerid)) {
  workerid <- append(workerid, x)
  p_temp <- exclude.influence(p, "workerid", x)
  loglik <- append(loglik, -2*(logLik(p)[1]- logLik(p_temp)[1]))
  
  var.image  <- append(var.image, data.frame(VarCorr(p_temp))$vcov[3])
  varint.image <- append(varint.image , data.frame(VarCorr(p_temp))$vcov[1] + data.frame(VarCorr(p_temp))$vcov[3])
  total.var <- append(total.var, sum(data.frame(VarCorr(p_temp))$vcov))
  
  var.image.change <- append(var.image.change, data.frame(VarCorr(p_temp))$vcov[3]/ sum(data.frame(VarCorr(p_temp))$vcov) - p.var.image )
  varint.image.change <- append(varint.image.change, (data.frame(VarCorr(p_temp))$vcov[1] + data.frame(VarCorr(p_temp))$vcov[3]) /sum(data.frame(VarCorr(p_temp))$vcov) - p.var.imageint)
  
  total.var.change <- append(total.var.change, sum(data.frame(VarCorr(p_temp))$vcov) / p.t)
  
  vartrace <- append(vartrace,  trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp)))))
  vartrace2 <- append(vartrace2, trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp))))- rankMatrix(v.matrix)[1])
  vartrace3 <- append(vartrace3, abs(trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp))))- rankMatrix(v.matrix)[1]))
  accuracy <- append(accuracy, sum(diag(table(simulated_df[simulated_df$workerid==x, ]$decision, simulated_df[simulated_df$workerid==x, ]$label)))/30)
}


length(workerid)
length(loglik)
length(total.var)
length(var.image)
length(varint.image)
length(vartrace)
length(vartrace2)
length(vartrace3)
length(total.var.change)
length(var.image.change)
length(varint.image.change)
length(accuracy)


df.i = data.frame(workerid,loglik, total.var, var.image, varint.image, vartrace, vartrace2, vartrace3,
                  total.var.change, var.image.change, varint.image.change, accuracy)

View(df.i)


cor(df.i$loglik, df.i$accuracy)
cor(df.i$vartrace, df.i$accuracy)
cor(df.i$total.var.change, df.i$accuracy)



ggplot(data = df.i, aes(x = accuracy, y = loglik)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point()


ggplot(data = df.i, aes(x = accuracy, y = total.var.change)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) + geom_point()


ggplot(data = df.i, aes(x = accuracy, y = vartrace)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) + geom_point()


#################### image
imageid <- c()
loglik <- c()
total.var <- c()
var.image <- c()
varint.image <- c()
vartrace <- c()
vartrace2 <- c()
vartrace3 <- c()
total.var.change <- c()
var.image.change <- c()
varint.image.change <- c()
disagreerate <- c()


for (x in unique(simulated_df$imageid)) {
  imageid <- append(imageid, x)
  p_temp <- exclude.influence(p, "imageid", x)
  loglik <- append(loglik, -2*(logLik(p)[1]- logLik(p_temp)[1]))
  
  var.image  <- append(var.image, data.frame(VarCorr(p_temp))$vcov[3])
  varint.image <- append(varint.image , data.frame(VarCorr(p_temp))$vcov[1] + data.frame(VarCorr(p_temp))$vcov[3])
  total.var <- append(total.var, sum(data.frame(VarCorr(p_temp))$vcov))
  
  var.image.change <- append(var.image.change, data.frame(VarCorr(p_temp))$vcov[3]/ sum(data.frame(VarCorr(p_temp))$vcov) - p.var.image )
  varint.image.change <- append(varint.image.change, (data.frame(VarCorr(p_temp))$vcov[1] + data.frame(VarCorr(p_temp))$vcov[3]) /sum(data.frame(VarCorr(p_temp))$vcov) - p.var.imageint)
  
  total.var.change <- append(total.var.change, sum(data.frame(VarCorr(p_temp))$vcov) / p.t)
  
  vartrace <- append(vartrace,  trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp)))))
  vartrace2 <- append(vartrace2, trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp))))- rankMatrix(v.matrix)[1])
  vartrace3 <- append(vartrace3, abs(trace(v.matrix - as.matrix(Matrix::bdiag(VarCorr(p_temp))))- rankMatrix(v.matrix)[1]))
  disagreerate <- append(disagreerate, min(table(simulated_df[simulated_df$imageid==x, ]$decision))/sum(table(simulated_df[simulated_df$imageid==x, ]$decision)))
}


length(imageid)
length(loglik)
length(total.var)
length(var.image)
length(varint.image)
length(vartrace)
length(vartrace2)
length(vartrace3)
length(total.var.change)
length(var.image.change)
length(varint.image.change)

disagreerate = ifelse(disagreerate==1, 0, disagreerate)
df.p = data.frame(imageid,loglik, total.var, var.image, varint.image, vartrace, vartrace2, vartrace3,
                  total.var.change, var.image.change, varint.image.change, disagreerate)

View(df.p)


cor(df.p$loglik, df.p$disagreerate)
cor(df.p$vartrace, df.p$disagreerate)
cor(df.p$total.var.change, df.p$disagreerate)


ggplot(data = df.p, aes(x = disagreerate, y = loglik)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point()


ggplot(data = df.p, aes(x = disagreerate, y = total.var.change)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) + geom_point()


ggplot(data = df.p, aes(x = disagreerate, y = vartrace)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) + geom_point()




