library(HLMdiag)
install.packages('mlmRev')
data(sleepstudy, package = 'lme4')
ss <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)

# covratio for individual observations
ss.cr1 <- covratio(ss)

# covratio for subject-level deletion
ss.cr2 <- covratio(ss, level = "Subject")

## Not run: 
## A larger example
data(Exam, package = 'mlmRev')
fm <- lme4::lmer(normexam ~ standLRT * schavg + (standLRT | school), data = Exam)

# covratio for individual observations
cr1 <- covratio(fm)

# covratio for school-level deletion
cr2 <- covratio(fm, level = "school")
cr2 <- covratio(p, level = "workerid")

## End(Not run)

# covtrace for individual observations
ss.ct1 <- covtrace(ss)

# covtrace for subject-level deletion
ss.ct2 <- covtrace(ss, level = "Subject")

## Not run: 
## Returning to the larger example
# covtrace for individual observations
ct1 <- covtrace(fm)

# covtrace for school-level deletion
ct2 <- covtrace(fm, level = "school")

## End(Not run)