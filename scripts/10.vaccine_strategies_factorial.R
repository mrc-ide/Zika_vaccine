

## devtools::install_github("mrc-ide/ZikaModel")

library(ZikaModel)
library(ggplot2)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "wrapper_multi_factors_ZikaModel.R"))
source(file.path("R", "calculate_MC_numbers.R"))
source(file.path("R", "reshape.R"))
source(file.path("R", "post_process.R"))
source(file.path("R", "plot_diagnostics.R"))


# define parameters -----------------------------------------------------------


experiment_name <- "vaccine_strategies_factorial"

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

tt <- its * my_dt

time <- max(tt)

mr_baseline <- 0.0002

N_human_brazil <- 200000000

vacc_child_coverage_values <- c(0.5, 0.8)

population_coverage_values <- c(0.5, 1)
  
vacc_starttime <- 1.5

vacc_stoptime <- vacc_starttime + 0.2

vacc_ages <- c(0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0) 

tot_cov_values <- c(0, as.vector(outer(vacc_child_coverage_values, population_coverage_values)))

fixed_params <- list(DT = my_dt,
                     N_human = N_human_brazil)


# load data -------------------------------------------------------------------


mr_pregn_data <- readRDS(file.path("output", "microcephaly_risk_probs.rds"))

br_brazil_age <- readRDS(file.path("output", "age_specific_birth_rates.rds"))

  
# create factor combinations --------------------------------------------------


fact_combs <- expand.grid(tot_cov = tot_cov_values, 
                          start = vacc_starttime,
                          stop = vacc_stoptime)

fact_combs <- cbind(id = seq_len(nrow(fact_combs)), fact_combs)

fact_combs_ls <- df_to_list(fact_combs, use_names = TRUE)

write_out_csv(fact_combs, out_tab_dir, "experiments")


# run -------------------------------------------------------------------------


vacc_experiment <- loop(fact_combs_ls,
                        wrapper_multi_factors_ZikaModel_2,
                        agec = age_init,
                        death = deathrt,
                        vaccine_age = vacc_ages,
                        params = fixed_params,
                        integer_time_steps = its,
                        parallel = FALSE)


# plot number of new infections -----------------------------------------------


sum_all_fact <- lapply(vacc_experiment, sum_across_array_dims, 1)

sum_all_fact_id <- lapply(seq_along(sum_all_fact), cbind_id_time, sum_all_fact, tt)

sum_all <- do.call("rbind", sum_all_fact_id)

sum_all_df <- as.data.frame(sum_all)

sum_all_df$id <- factor(sum_all_df$id, 
                        levels = unique(sum_all_df$id),
                        labels = unique(sum_all_df$id))
  
p <- plot_diagnostics_by_vaccine(sum_all_df, "id", "new infections")


# save plots ------------------------------------------------------------------


save_plot(p, out_fig_dir, "inf_1_by_run_id", wdt = 12, hgt = 8)
