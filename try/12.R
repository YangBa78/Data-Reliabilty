library(lme4)
library(tidyverse)
library(knitr)
library(here)
library(broom.mixed)
library(tidybayes)
library(DiagrammeR)


n_part <- 10 # number of parts
n_oper <- 3 # number of opers
n_measurements <- 2 # number of replications


# assign names to each part, operator, trial
part <- str_glue("part_{1:n_part}") %>% as_factor()
operator <- str_glue("oper_{1:n_oper}") %>% as_factor()
measurement <- str_glue("measurment_{1:n_measurements}") %>% as_factor()

n_matrix <- n_part * n_oper * n_measurements # number of observations in the study

n_matrix
## [1] 60


# generate experimental design and outcomes for dummy study
grr_dummy_tbl <- crossing(part, operator, measurement) %>%
  mutate(measurement = 10 + rnorm(n_matrix))

grr_dummy_tbl %>%
  head(7) %>%
  kable(align = "c")


grr_dummy_tbl %>%
  tail(7) %>%
  kable(align = "c")

# fit model for dummy study
m1 <- lmer(measurement ~ (1 | part) + (1 | operator) + (1 | part:operator), data = grr_dummy_tbl)
m1


# extract design matrix Z from dummy model
design_matrix_Z <- getME(m1, "Z") %>% as.matrix()

design_matrix_Z %>% head(1)
##   part_1:oper_1 part_1:oper_2 part_1:oper_3 part_2:oper_1 part_2:oper_2
## 1             1             0             0             0             0
##   part_2:oper_3 part_3:oper_1 part_3:oper_2 part_3:oper_3 part_4:oper_1
## 1             0             0             0             0             0
##   part_4:oper_2 part_4:oper_3 part_5:oper_1 part_5:oper_2 part_5:oper_3
## 1             0             0             0             0             0
##   part_6:oper_1 part_6:oper_2 part_6:oper_3 part_7:oper_1 part_7:oper_2
## 1             0             0             0             0             0
##   part_7:oper_3 part_8:oper_1 part_8:oper_2 part_8:oper_3 part_9:oper_1
## 1             0             0             0             0             0
##   part_9:oper_2 part_9:oper_3 part_10:oper_1 part_10:oper_2 part_10:oper_3
## 1             0             0              0              0              0
##   part_1 part_2 part_3 part_4 part_5 part_6 part_7 part_8 part_9 part_10
## 1      1      0      0      0      0      0      0      0      0       0
##   oper_1 oper_2 oper_3
## 1      1      0      0
# alternate method:
# mylF <- lFormula(m1, data = grr_dummy_tbl) # Process the formula against the data
# design_matrix_Z <- mylF$reTrms$Zt %>% as.matrix() %>% t()  # Extract the Z matrix

set.seed(0118)
int_intercepts_sd <- 1 # standard dev of interaction random effects
oper_intercepts_sd <- 2 # standard dev of operator random effects
part_intercepts_sd <- 9 # standard dev of operator random effects
random_error_repeatability <- 4 # standard dev of random error (repeatability)

# simulate random effects using input params for sd
int_intercepts <- rnorm(n = n_part * n_oper, mean = 0, sd = int_intercepts_sd)
oper_intercepts <- rnorm(n = n_oper, mean = 0, sd = oper_intercepts_sd)
part_intercepts <- rnorm(n = n_part, mean = 0, sd = part_intercepts_sd)

# vector of all random effect intercepts (order matters here: ineraction, part, oper if n_oper < n_part, else swith part and oper)

random_effects_intercepts <- c(int_intercepts, part_intercepts, oper_intercepts)
random_effects_intercepts
##  [1]  -1.676079782   0.167651720  -0.008545182   0.296888139  -1.706489201
##  [6]  -1.049094451  -0.102206749   0.682492401  -0.511117703   0.487346673
## [11]  -1.345812057   0.527128496   0.686104071  -0.221484626   0.532399538
## [16]   0.597393622   0.831437918   0.735023009   0.830043214   0.769163682
## [21]   1.830416344   0.182049999   1.018859437   0.844012288   0.575312043
## [26]   0.006855854  -0.231251230   0.205834471   0.250942908  -1.575663200
## [31]  19.280822421  10.499576059  -3.997253469  -7.111499870   6.986996777
## [36]  -4.861684848 -13.031232590  14.723410605  16.967969396  22.293922487
## [41]  -1.345816596  -3.905496830  -4.061788248


# create observations (add in repeatability random error to each term). %*% is matrix multiplication
grr_sim_tbl <- grr_dummy_tbl %>%
  mutate(measurement = 10 + design_matrix_Z %*% random_effects_intercepts + rnorm(
    n = nrow(grr_dummy_tbl),
    mean = 0,
    sd = random_error_repeatability
  ))

grr_sim_tbl %>%
  head(10) %>%
  kable(align = "c")


# fit a model to the simulated dataset
sim_m_fit <- lmer(measurement ~ (1 | part) + (1 | operator) + (1 | part:operator), data = grr_sim_tbl)
sim_m_fit1 <- lmer(measurement ~ (1 | part) + (1 | operator) + (1 | time), data = grr_sim_tbl)

summary(sim_m_fit1)
sim_m_fit2 <- lmer(measurement ~ (1 | part) + (1 | operator) + (1 | part:operator) + (1| time), data = grr_sim_tbl)

grr_sim_tbl$time <- rep(c(1, 2), 30)

summary(sim_m_fit2)
# tibble of results for a single simulation
one_grr_result_tbl <-
  broom.mixed::tidy(sim_m_fit, effects = "ran_pars") %>%
  rename(
    st_dev_estimate = estimate,
    variable = group
  ) %>%
  mutate(
    variance_estimate = st_dev_estimate^2,
    sim_number = 1
  ) %>%
  select(sim_number, variable, st_dev_estimate, variance_estimate)

one_grr_result_tbl %>% kable(align = "c")

one_grr_tol_outcome_tbl <- one_grr_result_tbl %>%
  filter(variable != "part") %>%
  group_by(sim_number) %>%
  summarize(grr_variance_est = sum(variance_estimate)) %>%
  mutate(true_tol_pct = scales::percent((int_intercepts_sd^2 + oper_intercepts_sd^2 + random_error_repeatability^2) / part_intercepts_sd)) %>%
  rowwise() %>%
  mutate(est_tol_pct = scales::percent(grr_variance_est / part_intercepts_sd))

one_grr_tol_outcome_tbl %>% kable(align = "c")


grr_fct <- function(n, np, no, nm, iisd, oisd, pisd, rer) {
  all_grr_results_tbl <- NULL # tibble to hold results
  n_sims <- n # number of simulations
  n_part <- np # number of parts
  n_oper <- no # number of opers
  n_measurements <- nm # number of replications
  
  int_intercepts_sd <- iisd # standard dev of interaction random effects
  oper_intercepts_sd <- oisd # standard dev of operator random effects
  part_intercepts_sd <- pisd # standard dev of operator random effects
  random_error_repeatability <- rer # standard dev of random error (repeatability)
  
  # assign names to each per, operator, trial
  part <- str_glue("part_{1:n_part}") %>% as_factor()
  operator <- str_glue("oper_{1:n_oper}") %>% as_factor()
  measurement <- str_glue("measurment_{1:n_measurements}") %>% as_factor()
  
  n_matrix <- n_part * n_oper * n_measurements # number of observations in the study
  
  for (i in 1:n) {
    
    # generate experimental designa and outcomes for dummy study
    grr_dummy_tbl <- crossing(part, operator, measurement) %>%
      mutate(measurement = 10 + rnorm(n_matrix))
    
    # fit model for dummy study
    m1 <- lmer(measurement ~ (1 | part) + (1 | operator) + (1 | part:operator), data = grr_dummy_tbl)
    
    # extract design matrix Z from dummy model
    design_matrix_Z <- getME(m1, "Z") %>% as.matrix()
    
    # simulate random effects using input params for sd
    int_intercepts <- rnorm(n = n_part * n_oper, mean = 0, sd = int_intercepts_sd)
    oper_intercepts <- rnorm(n = n_oper, mean = 0, sd = oper_intercepts_sd)
    part_intercepts <- rnorm(n = n_part, mean = 0, sd = part_intercepts_sd)
    
    # vector of all random effect intercepts (order matters here: ineraction, oper, part)
    random_effects_intercepts <- c(int_intercepts, part_intercepts, oper_intercepts)
    
    # create observations (add in repeatability random error to each term). %*% is matrix multiplication
    grr_sim_tbl <- grr_dummy_tbl %>%
      mutate(measurement = 10 + design_matrix_Z %*% random_effects_intercepts + rnorm(
        n = nrow(grr_dummy_tbl),
        mean = 0,
        sd = random_error_repeatability
      ))
    
    # fit a model to the simulated dataset
    sim_m_fit <- lmer(measurement ~ (1 | part) + (1 | operator) + (1 | part:operator), data = grr_sim_tbl)
    
    # tibble of results for a single simulation
    one_grr_result_tbl <-
      broom.mixed::tidy(sim_m_fit, effects = "ran_pars") %>%
      rename(
        st_dev_estimate = estimate,
        variable = group
      ) %>%
      mutate(
        variance_estimate = st_dev_estimate^2,
        sim_number = i
      ) %>%
      select(sim_number, variable, st_dev_estimate, variance_estimate)
    
    # append this recent simulation to the others
    all_grr_results_tbl <- bind_rows(all_grr_results_tbl, one_grr_result_tbl)
  }
  return(all_grr_results_tbl)
}


fcn_test_tbl <- grr_fct(n = 3, np = 10, no = 3, nm = 2, iisd = 4, oisd = 3, pisd = 2, rer = 1)

fcn_test_tbl %>% kable(align = "c")


sim_setup_tbl <- tibble(
  n_sims = 200,
  n_part = 10,
  n_oper = 3,
  n_meas = 2,
  int_sd = 1,
  oper_sd = 1,
  #  part_var = 1,
  part_var = c(2^(0:10)),
  repeatab_sd = 1
) %>%
  mutate(
    row_id = row_number()
  ) %>%
  rowwise() %>%
  mutate(part_sd = part_var^.5) %>%
  mutate(tol_pct_true = 6 * (int_sd^2 + oper_sd^2 + repeatab_sd^2) / (6 * part_sd))

sim_setup_tbl %>% kable(align = "c")


set.seed(0118)

#commented out because this takes a while to run

# sim_outcomes_tbl <- sim_setup_tbl %>%
#   rowwise() %>% # may not be needed
#   mutate(sim_outcomes = list(grr_fct(n = n_sims, np = n_part, no = n_oper, nm = n_meas, iisd = int_sd, oisd = oper_sd, pisd = part_sd, rer = repeatab_sd))) %>%
#   select(sim_outcomes, everything()) %>%
#   unnest(cols = c(sim_outcomes)) %>%
#   mutate_if(is.character, as_factor)
sim_outcomes_tbl %>%
  select(sim_number, variable, st_dev_estimate, n_sims, n_part, n_meas, int_sd, oper_sd, repeatab_sd, part_sd) %>%
  head(12) %>%
  kable(align = "c")

a <- sim_outcomes_tbl %>%
  filter(row_id == 4) %>%
  filter(st_dev_estimate > .001) %>%
  ggplot(aes(x = variable, y = st_dev_estimate)) +
  geom_jitter(width = .05, alpha = .5, size = .6) +
  geom_hline(yintercept = 1, lty = 2, color = "#2c3e50") +
  geom_hline(yintercept = 2.8428472, lty = 2, color = "#2c3e50") +
  #    stat_summary(fun.y= mean, fun.ymin=mean, fun.ymax=mean, geom="crossbar", width=0.2, color="red") +
  stat_halfeye(aes(fill = variable), point_interval = mean_qi, alpha = .7, position = position_nudge(x = .15)) +
  labs(
    title = "Gage R&R - Estimates for Component Standard Deviations",
    subtitle = str_glue("Settings: {sim_outcomes_tbl$n_part[1]} Parts, {sim_outcomes_tbl$n_oper[1]} Operators, {sim_outcomes_tbl$n_meas[1]} Measurements"),
    x = "",
    y = "Standard Deviation Estimate",
    caption = "dotted line marks true population standard dev\n Interval marks median, .66 quantile, .95 quantile"
  ) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "c", end = .7)

a
