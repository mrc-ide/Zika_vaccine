

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

  
# preprocessing ---------------------------------------------------------------


tot_cov_values <- vacc_child_coverage_values * population_coverage_values

vacc_child_cov <- tot_cov_values[1]


# run -------------------------------------------------------------------------


params <- list(DT = my_dt,
               N_human = N_human_brazil,
               vacc_child_coverage = vacc_child_cov,
               vacc_child_starttime = vacc_starttime,
               vacc_child_stoptime = vacc_stoptime)

odin_model_path <- system.file("extdata/odin_model_determ.R", package = "ZikaModel")

create_generator <- create_r_model(odin_model_path = odin_model_path,
                                   agec = age_init,
                                   death = deathrt,
                                   nn_links = nn_links,
                                   amplitudes_phases = amplitudes_phases,
                                   vaccine_age = vacc_ages,
                                   params = params)

gen <- create_generator$generator(user = create_generator$state)

integer_time_steps <- (364 * time_years) / my_dt

its <- seq(0, integer_time_steps, 1)

mod_run <- gen$run(its)


# post processing -------------------------------------------------------------


out <- gen$transform_variables(mod_run)

n_infections <- out$inf_1

time_steps <- out$TIME

tt <- out$TIME

time <- max(tt)

brks <- seq(from = 0, to = time, by = 364 * plot_interval)


# plot number of new infections -----------------------------------------------


sum_over_ages_patches <- lsum_across_array_dims(n_infections, c(1,3))

melted_sum <- melt_sim_output_array_3(sum_over_ages_patches, tt)  

p <- ggplot(data = summary_all) +
  geom_line(aes(x = time, y = value, colour = vaccine_status)) +
  facet_wrap(~ id)
