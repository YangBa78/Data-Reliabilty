# influenceME

library(influence.ME)
library(lattice)
data(school23) 

school23
school23 <- within(school23, homework <- unclass(homework)) 
m23 <- lmer(math ~ homework + structure + (1 | school.ID), data=school23) 

print(m23, cor=FALSE)


struct <- unique(subset(school23, select=c(school.ID, structure))) 

struct$mathAvg <- with(school23, tapply(math, school.ID, mean)) 

dotplot(mathAvg ~ factor(structure), struct, type=c("p","a"), xlab="Class structure level", ylab="Average Math Test Score")

estex.m23 <- influence(m23, "school.ID")
dfbetas(estex.m23, parameters=c(2,3))

plot(estex.m23, which="dfbetas", parameters=c(2,3), xlab="DFbetaS", ylab="School ID")


cooks.distance(estex.m23, parameter=3, sort=TRUE) 
plot(estex.m23, which="cook", cutoff=.17, sort=TRUE, xlab="Cook ́s Distance", ylab="School ID")

sigtest(estex.m23, test=-1.96)$structure[1:10,]

estex.obs <- influence(m23, obs=TRUE) 

cks.d <- cooks.distance(estex.obs, parameter=3) which(cks.d > 4/519)

struct
View(influence.ME::sigtest)
