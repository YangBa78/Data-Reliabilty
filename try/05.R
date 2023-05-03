#test logitistic regression

library(simstudy)
def <- defRepeat(nVars = 4, prefix = "g", formula = "1/3;1/3;1/3",
                 variance = 0, dist = "categorical")
def <- defData(def, varname = "a", formula = "1;1", dist = "trtAssign")
def <- defRepeat(def, 3, "b", formula = "5 + a", variance = 3,
                 dist = "normal")
def <- defData(def, "y", formula = "0.50", dist = "binomial")

def

dd <- genData(100, def)
dd
sum(dd$y)


d1 <- defData(varname = "x1", formula = 0, variance = 1,
              dist = "normal")
d1 <- defData(d1, varname = "x2", formula = 0.5, dist = "binary")

d2 <- defRepeatAdd(nVars = 2, prefix = "q", formula = "5 + 3*rx",
                   variance = 4, dist = "normal")
d2 <- defDataAdd(d2, varname = "y", formula = "-2 + 0.5*x1 + 0.5*x2 + 1*rx",
                 dist = "binary", link = "logit")

dd <- genData(10000, d1)
dd <- trtAssign(dd, nTrt = 2, grpName = "rx")
dd
dd <- addColumns(d2, dd)
dd

summary(model <- glm(y ~x1+x2+rx,family=binomial(link='logit'),data=dd))

dd$b <- -2 + 0.5*dd$x1 + 0.5*dd$x2 + 1*dd$rx
dd$prob1 <- 1/(1 + exp(-(dd$b)))
dd$y2 <- ifelse(dd$prob>=0.5, 1, 0)
dd$y1 <- rbinom(n = dim(dd)[1], size = 1, prob = dd$prob)
dim(dd[dd$y==1,])
sum(dd[dd$y==1,]$y1)
table(dd$y, dd$y1)
dd
dd[dd$y==1,]
table(dd$y2, dd$y1)
table(dd$y, dd$y2)
table(dd$y, dd$y1)



gender <- sample(c(0,1), size = 1000, replace = TRUE)
age <- round(runif(1000, 18, 80))
xb <- -9 + 3.5*gender + 0.2*age
p <- 1/(1 + exp(-xb)) 
p1 <- plogis(xb)
y3 <- 1/(1 + exp(-xb))
y <- rbinom(n = 1000, size = 1, prob = p)
y1 <- rbinom(n = 1000, size = 1, prob = p)
mod <- glm(y ~ gender + age, family = "binomial")
summary(mod)
mod1<- glm(y ~ gender + age, family = binomial(link=logit))
summary(mod1)
y1 <- ifelse(p>=0.5, 1, 0)
table(y, y1)
