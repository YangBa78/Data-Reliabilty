library("lme4")
fit1 = lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
newdata <- expand.grid(
  Days = c(12, 14),
  Subject = unique(sleepstudy$Subject)
)

newdata$Predicted_Response <- predict(fit1, newdata = newdata)



d <- sleepstudy
d$Subject <- factor(rep(1:18, each=10))

fm1 <- lmer(Reaction ~ Days + (Days|Subject), d)
summary(fm1)

d <- rbind(sleepstudy, sleepstudy)
d$Subject <- factor(rep(1:36, each=10))
d$Reaction <- ifelse(d$Subject %in% 19:36, NA, d$Reaction)

d$predicted1 <- predict (fm1, newdata=d, allow.new.levels=T)
d$simulated <- simulate(fm1, seed=1, newdata=d[-1], re.form=NA,
                        allow.new.levels=T)$sim_1
d$simulated1 <- simulate(fm1, seed=1, newdata=d[-1], re.form=NULL,
                        allow.new.levels=T)$sim_1


f <- function(x, ...) {
  plot(x, xlab="Subject", ylim=c(0, 500), ...)
  grid()
}
par(mfrow=c(1,4), mar=c(4,4,1,1))
with(d, f(tapply(Reaction,  Subject, mean), main="Original data", ylab="Reaction", xlim=c(1, 36)))
with(d, f(tapply(predicted, Subject, mean), main="Predicted data", ylab="", col=rep(1:2, each=18)))
with(d, f(tapply(simulated, Subject, mean), main="Simulated data", ylab="", col=rep(1:2, each=18)))
with(d, f(tapply(simulated1, Subject, mean), main="Simulated data2", ylab="", col=rep(1:2, each=18)))
legend("bottomright", pch=c(1,1), col=1:2, c("old subjects", "new subjects"), bg="white")


fm_pre <- lmer(simulated1 ~ Days + (Days|Subject), d)
summary(fm_pre)



x <- with(d, as.matrix(tapply(Reaction,  list(Subject, Days), mean), ncol=9))
y <- with(d, as.matrix(tapply(predicted, list(Subject, Days), mean), ncol=9))
z <- with(d, as.matrix(tapply(simulated, list(Subject, Days), mean), ncol=9))
t <- with(d, as.matrix(tapply(simulated1, list(Subject, Days), mean), ncol=9))

f <- function(xlab="Day", ...) {
  plot(c(1, 10), c(0, 500), ylab="Raction", xlab=xlab, t="n", ...)
  grid()
}
par(mfrow=c(3,3), mar=c(4,4,3,1))

f(main="Original data", "")
for (i in 1:18) lines(x[i,])

f(main="Predicted data\nold subjects", "")
for (i in 1:18) lines(y[i,])

f(main="Predicted data\nnew subjects", "")
for (i in 19:36) lines(y[i,], col="red")


f(main="Original data")
for (i in 1:18) lines(x[i,])

f(main="Simulated data\nold subjects")
for (i in 1:18) lines(z[i,])

f(main="Simulated data\nnew subjects")
for (i in 19:36) lines(z[i,], col="red")

f(main="Original data")
for (i in 1:18) lines(x[i,])

f(main="Simulated data2\nold subjects")
for (i in 1:18) lines(t[i,])

f(main="Simulated data2\nnew subjects")
for (i in 19:36) lines(t[i,], col="red")


legend("bottomright", lty=c(1,1), col=1:2, c("old subjects", "new subjects"), bg="white")
