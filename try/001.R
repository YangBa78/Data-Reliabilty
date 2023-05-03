library(lme4)
data(Oxide, package = "nlme")
head(Oxide, 12)

mod3 <- lmer(Thickness ~ 1 + (1|Lot/Wafer), data = Oxide)
summary(mod3)

mod2 <- lmer(Thickness ~ 1 + (1|Wafer), data = Oxide)
summary(mod2)

rc3 <- rescov(mod3, Oxide)
image(rc3)

table(rle(c(1,1,1, 1, 1, 1, 1, 0, 1,1, 1, 1, 1))[1]$lengths)
as.data.frame.matrix(table(rle(c(1,1,1, 1, 1, 1, 1, 0, 1,1, 1, 1, 1))[1]$lengths))
