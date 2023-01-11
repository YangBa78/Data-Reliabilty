library(lme4)
m2 <- lmer(Reaction~Days+(1|Subject)+(0+Days|Subject),sleepstudy, REML = TRUE)
m1 <- update(m2,.~Days+(1|Subject))
m0 <- lm(Reaction~Days,sleepstudy)

anova(m2,m1,m0) 
G2 = -2 * logLik(m2) + 2 * logLik(m1)
pchisq(as.numeric(G2), df=1, lower.tail=F)
anova(m2,m1) 

varTest(m1,m2,pval.comp="bounds")
install.packages('varTestnlme')
library(varTestnlme)

var.test(m1, m2)
library(nlme)

library(lme4)
#> Le chargement a nécessité le package : Matrix
data(Orthodont, package = "nlme")

# fit the two models under H1 and H0
m1 <- lmer(distance ~ 1 + Sex + age + age*Sex + 
             (0 + age | Subject), data = Orthodont, REML = FALSE)
m0 <- lm(distance ~ 1 + Sex + age + age*Sex, data = Orthodont)

# compare them (order is important: m1 comes first)
varTest(m1,m0, alternative = "two.sided", conf.level=0.95)
library("EnvStats")

varCompTest(m1, m2)
anova(m1, m2)
G2 = -2 * logLik(m0) + 2 * logLik(m1)
G2

h = c()
h = append(h, as.numeric(varCompTest(m1, m2)[[6]][1]))

library(dyn)
install.packages('dyn')

G2 = -2 * logLik(p) + 2 * logLik(p_t)
pchisq(as.numeric(G2), df=66, lower.tail=F)
pchisq(85, df=66, lower.tail=F)



if (require(nlme)){
  print(fm1 <- lme(distance ~ age, data = Orthodont))
  infIndexPlot(influence(fm1, "Subject"))
  infIndexPlot(influence(fm1))
}
library('lme')
install.packages('lme')
library('car')
