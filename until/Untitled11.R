# http://stats.stackexchange.com/q/79360/67822

n         <- 30 # Number of trees

organ <-  rep(c(rep("root", 6), rep("stem", 6)), n)
site <- c(rep("L", 1/2 * length(organ)), rep("R", 1/2*length(organ)))
tree <- c(rep("LT01",12), rep("LT02",12), rep("LT03",12),
          rep("LT04",12), rep("LT05",12), rep("LT06",12),
          rep("LT07",12), rep("LT08",12), rep("LT09",12),
          rep("LT10",12), rep("LT11",12), rep("LT12",12),
          rep("LT13",12), rep("LT14",12), rep("LT15",12),
          rep("RT01",12), rep("RT02",12), rep("RT03",12),
          rep("RT04",12), rep("RT05",12), rep("RT06",12),
          rep("RT07",12), rep("RT08",12), rep("RT09",12),
          rep("RT10",12), rep("RT11",12), rep("RT12",12),
          rep("RT13",12), rep("RT14",12), rep("RT15",12))
sample <- rep(c(1,1,2,2,3,3), n * 2)
treatment <- c(rep("T", 1/2 * length(organ)), rep("C", 1/2 * length(organ)))
tissue <- rep(c("phloem","xylem"), n * 6)


# Now we build the dependent variable:

# We choose two random intercept "contributions" from N(0,3) with a fixed effect of 6 for organ:
organ_contribution     <- 6 + rnorm(2, mean = 0, sd = 3) 
organ_c                <- as.vector(replicate(n, c(rep(organ_contribution[1],6),
                                                   rep(organ_contribution[2],6))))

# There is a fixed effect with two distinct intercepts for treatment versus controls,
# separated by 30:
treatment_c            <- c(rep(100, 180), rep(70, 180)) + rnorm(360, 0, 20)

# We have prominent fixed effects very different intercepts for phloem versus xylem separated by 3, 
# and random effects with sd of 3:
tissue_contribution    <- c(3 + rnorm(1, mean = 0, sd = 3), 6 + rnorm(1, mean = 0, sd = 3)) 
tissue_c               <- rep(c(tissue_contribution[1], 
                                tissue_contribution[2]), 1/2*length(tissue))

# Just random effects for the sample contribution with sd of 5:
a = rnorm(1, mean = 0, sd = 5); b = rnorm(1, mean = 0, sd = 5); c = rnorm(1, mean = 0, sd = 5)
sample_c               <- rep(c(rep(a,2), rep(b,2), rep(c,2)), length(organ)*1/6)

# And for tree contribution we have just random eff's with sd of 7:
tree_c                 <- as.vector(replicate(n, rep(rnorm(1,0,7), 12)))

# And for site also just random eff's with sd of 3:
site_c                 <- c(rep(rnorm(1,0,3), 180), rep(rnorm(1,0,3), 180))

# Combining all these effects:
length                 <- site_c + tree_c + sample_c + tissue_c + treatment_c + organ_c


trees <- data.frame(site, site_c, tree, tree_c, treatment, treatment_c, organ, organ_c, 
                    sample, sample_c, tissue, tissue_c, length)
trees[1:15,]
str(trees)

require(lme4); require(lmerTest)
fit <- lmer(length ~ treatment + organ + tissue + (1|tree/organ/sample), data = trees) 
summary(fit)

# Interpreting coefficients: http://stats.stackexchange.com/a/186994/67822

predict(fit, newdata = data.frame(treatment = "T", organ = "root", tissue = "phloem"), re.form = NA)

predict(fit, newdata = data.frame(treatment = "T", organ = "stem", tissue = "phloem"), re.form = NA)

predict(fit, newdata = data.frame(treatment = "T", organ = "stem", tissue = "xylem"), re.form = NA)

predict(fit, newdata = data.frame(treatment = "C", organ = "stem", tissue = "xylem"), re.form = NA)

predict(fit, newdata = data.frame(treatment = "C", organ = "root", tissue = "xylem"), re.form = NA)


xtabs(~ school + class, dt)

dt <- read.table("http://bayes.acs.unt.edu:8083/BayesContent/class/Jon/R_SC/Module9/lmm.data.txt",
                   header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
m0 <- lmer(extro ~ open + agree + social + (1 | school/class), data = dt)
summary(m0)


c = ()

