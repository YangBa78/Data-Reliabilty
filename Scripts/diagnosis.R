# HLMdiag
devtools::install_github("aloy/HLMdiag")
install.packages("HLMdiag")
library(HLMdiag)
hlm_influence(mod0)
leverage(mod0)
hlm_resid(mod0)
remove.packages("HLMdiag")


library(influence.ME)
install.packages("influence.ME")


data(school23)
school23 <- within(school23,
                   homework <- unclass(homework))

View(school23)
m23 <- lmer(math ~ homework + structure
            + (1 | school.ID),
            data=school23)


p3 <- exclude.influence(p1, "image1", "2395_1.jpg")


summary(m23)
estex.m23 <- influence(m23, "school.ID")
dfbetas(estex.m23)
plot(estex.m23,
     which="dfbetas",
     parameters=c(2,3),
     xlab="DFbetaS",
     ylab="School ID")

plot(estex.m23, which="cook",
     cutoff=.17, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="School ID")


est1 <- influence(w1, "WorkerId")

cooks.distance(estex.m23, parameter=3, sort=TRUE)

plot(est1, which="cook",
     cutoff=.056, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="workerId")



#library(influence.ME)
plot(dfbetas(estex.m0))
estex.pimage <- influence(mod0, "image1")
plot(estex.pimage, which="cook",
     cutoff=.056, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="image1")


plot(estex.pimage,
     which="pchange",
     #parameters=c(2,3),
     xlab="pchange",
     ylab="image1")

dfbetas(estex.p1, parameters=c(3))

??dfbetas()

cooks.distance(estex.p1, sort=TRUE)



dt <- read.table("http://bayes.acs.unt.edu:8083/BayesContent/class/Jon/R_SC/Module9/lmm.data.txt", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
dt$classID <- paste(dt$school, dt$class, sep=".")

m0 <- lmer(extro ~ open + agree + social + (1 | school) + (1 | classID), data = dt)
summary(m0)

m1 <- lmer(extro ~ open + agree + social + (1 | school) + (1|classID:school), data = dt)
summary(m1)

View(dt)

