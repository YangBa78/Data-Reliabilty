rescov <- function(model, data) {
  var.d <- crossprod(getME(model,"Lambdat"))
  Zt <- getME(model,"Zt")
  vr <- sigma(model)^2
  var.b <- vr*(t(Zt) %*% var.d %*% Zt)
  sI <- vr * Diagonal(nrow(data))
  var.y <- var.b + sI
  invisible(var.y)
}

library(tidyr)
library(pillar)
install.packages('tidyr')


data(Penicillin)
head(Penicillin, 10)

mod1 <- lmer(diameter ~ 1 + (1 | sample), data = Penicillin)
mod1 <- lmer(diameter ~ 1 + (1 | sample), data = Penicillin %>% arrange(sample))
rc1 <- rescov(mod1, Penicillin)
image(rc1[1:3, 1:3])
rc1[1:3,1:3]


Penicillin %>% arrange(sample)
Penicillin %>% arrange(plate)

arrange(Penicillin, sample)
library(dplyr)
group_by(mtcars, cyl) %>%                ## group by cylinder
  mutate(rank = row_number(mpg)) %>%   ## rank by mpg
  filter(rank <= 3) %>%                ## top three for each cyl
  arrange(rank) %>%                    ## arrange each group by rank
  ungroup() %>%                        ## remove grouping
  arrange(desc(cyl))                   ## arrange all by cylinder (descending)




mod2 <- lmer(diameter ~ 1 + (1 | sample) + (1 | plate), data = Penicillin)
rc2 <- rescov(mod2, Penicillin)
image(rc2)
image(rc2[1:12, 1:12])
rc2[1:12, 1:12]

data(Oxide, package = "nlme")
head(Oxide, 12)
Oxide %>% arrange(Wafer)


mod3 <- lmer(Thickness ~ 1 + (1|Lot/Wafer), data = Oxide)
rc3 <- rescov(mod3, Oxide)
image(rc3[1:9, 1:9])

rc3[1:9, 1:9]


mod3b <- lmer(Thickness ~ 1 + (1|Lot) + (1|Wafer), data = Oxide)
rc3b <- rescov(mod3b, Oxide)
image(rc3b)
rc3b[1:]

Oxide