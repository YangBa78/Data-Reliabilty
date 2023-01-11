data(Oxide, package = "nlme")
head(Oxide, 12)

mod3 <- lmer(Thickness ~ 1 + (1|Lot/Wafer), data = Oxide)
mod3
ranef(mod3)

mod3 <- lmer(Thickness ~ 1 + (1|Lot/Wafer), data = Oxide)

mod3.1 <- lmer(Thickness ~ 1 + (1|Lot/Wafer), data = s1)
mod3.1
mod3.2 <- lmer(Thickness ~ 1 + (1|Lot/Wafer), data = s2)
mod3.2

mod4 <- lmer(Thickness ~ 1 + (1|Source/Lot/Wafer), data = Oxide)
mod4

mod3b <- lmer(Thickness ~ 1 + (1|Lot) + (1|Wafer), data = Oxide)
mod3b

str(Oxide)

s1 <- Oxide[Oxide$Source==1, ]
s2 <- Oxide[Oxide$Source==2, ]


dim(s1)
dim(s2)
dim(Oxide)


