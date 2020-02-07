

## devtools::install_github("mrc-ide/ZikaModel")

library(ZikaModel)
library(ggplot2)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "wrapper_multi_factors_ZikaModel.R"))
source(file.path("R", "calculate_MC_numbers.R"))
source(file.path("R", "reshape.R"))
source(file.path("R", "post_process.R"))


# define parameters -----------------------------------------------------------


experiment_name <- "vaccine_strategies"

out_fig_dir <- file.path("figures", experiment_name)

out_tab_dir <- file.path("output", experiment_name)

age_init <- c(1, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10)

deathrt <- c(1e-10,
             1e-10,
             1e-10,
             0.00277068683332695,
             0.0210680857689784,
             0.026724997685722,
             0.0525354529367476,
             0.0668013582441452,
             0.119271483740379,
             0.279105747097929,
             0.390197266957464)

time_years <- 50 # years

my_dt <- 1

integer_time_steps <- (364 * time_years) / my_dt

its <- seq(0, integer_time_steps, 1)

mr_baseline <- 0.0002

N_human_brazil <- 200000000
season <- FALSE

plot_interval <- 5 # years

vacc_child_coverage_values <- c(0.5, 0.8)

population_coverage_values <- c(0.5, 1)
  
vacc_starttime <- 1.5 * 364  

vacc_stoptime <- vacc_starttime + 60

# from 9 to 49
vacc_ages <- c(0,0,1,1,1,1,0,0,0,0,0) 


# load data -------------------------------------------------------------------


# microcephaly risk during pregnancy
mr_pregn_data <- readRDS(file.path("output", "microcephaly_risk_probs.rds"))
#mr_pregn_data[, "prob"] <- 0

br_brazil_age <- readRDS(file.path("output", "age_specific_birth_rates.rds"))

  
# create factor combinations --------------------------------------------------


fact_combs <- expand.grid(vacc_cov = vacc_child_coverage_values, 
                          pop_cov = population_coverage_values,
                          start = vacc_starttime,
                          stop = vacc_stoptime)

fact_combs$tot_cov <- fact_combs$vacc_cov * fact_combs$pop_cov

fact_combs <- cbind(id = seq_len(nrow(fact_combs)), fact_combs)

fact_combs_ls <- df_to_list(fact_combs, use_names = TRUE)

write_out_csv(fact_combs, out_tab_dir, "experiments")


# run -------------------------------------------------------------------------


vacc_experiment <- loop(fact_combs_ls,
                        wrapper_multi_factors_ZikaModel_2,
                        agec = age_init,
                        death = deathrt,
                        vaccine_age = vacc_ages,
                        my_dt = my_dt,
                        integer_time_steps = its,
                        season = season,
                        parallel = FALSE)

## write_out_rds(vacc_experiment, out_tab_dir, "vaccination_experiment")


# plot number of new infections -----------------------------------------------


tt <- its * my_dt

time <- max(tt)

brks <- seq(from = 0, to = time, by = 364 * plot_interval)

sum_over_ages <- lapply(vacc_experiment, sum_across_array_dims, c(1,3,4))

sum_over_ages_patches <- lapply(vacc_experiment, sum_across_array_dims, c(1,3))

melt_2 <- lapply(sum_over_ages, melt_sim_output_array_2, tt)
  
melt_3 <- lapply(sum_over_ages_patches, melt_sim_output_array_3, tt)  

melt_3_id <- lapply(seq_along(melt_3), cbind_id, melt_3)
  
summary_all <- do.call("rbind", melt_3_id)

p <- ggplot(data = summary_all) +
  geom_line(aes(x = time, y = value, colour = vaccine_status)) +
  facet_wrap(~ id)
