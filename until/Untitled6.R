library(tidyverse) # for data wrangling, pipes, and good dataviz
library(lmerTest)  # for mixed effect models
library(GGally)    # makes it easy to plot relationships between variables
# devtools::install_github("debruine/faux")
library(faux)      # for simulating correlated variables

options("scipen"=10, "digits"=4) # control scientific notation
set.seed(8675309) # Jenny, I've got your number


sub_n  <- 200 # number of subjects in this simulation
sub_sd <- 100 # SD for the subjects' random intercept

sub <- tibble(
  sub_id = 1:sub_n,
  sub_i  = rnorm(sub_n, 0, sub_sd), # random intercept
  sub_cond = rep(c("easy","hard"), each = sub_n/2) # between-subjects factor
)

stim_n  <- 50 # number of stimuli in this simulation
stim_sd <- 50 # SD for the stimuli's random intercept

stim <- tibble(
  stim_id = 1:stim_n,
  stim_i = rnorm(stim_n, 0, stim_sd) # random intercept
)


trials <- crossing(
  sub_id = sub$sub_id, # get subject IDs from the sub data table
  stim_id = stim$stim_id, # get stimulus IDs from the stim data table
  stim_version = c("congruent", "incongruent") # all subjects see both congruent and incongruent versions of all stimuli
) %>%
  left_join(sub, by = "sub_id") %>% # includes the intercept and conditin for each subject
  left_join(stim, by = "stim_id")   # includes the intercept for each stimulus


dat <- trials %>%
  mutate(
    # effect-code subject condition and stimulus version
    sub_cond.e = recode(sub_cond, "hard" = -0.5, "easy" = +0.5),
    stim_version.e = recode(stim_version, "congruent" = -0.5, "incongruent" = +0.5),
    # calculate error term (normally distributed residual with SD set above)
    err = rnorm(nrow(.), 0, error_sd),
    # calculate DV from intercepts, effects, and error
    dv = grand_i + sub_i + stim_i + err +
      (sub_cond.e * sub_cond_eff) + 
      (stim_version.e * stim_version_eff) + 
      (sub_cond.e * stim_version.e * cond_version_ixn) # in this example, this is always 0 and could be omitted
  )
  