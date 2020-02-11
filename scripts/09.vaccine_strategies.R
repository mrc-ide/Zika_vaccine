

## devtools::install_github("mrc-ide/ZikaModel")

library(ZikaModel)
library(ggplot2)
library(viridis)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "wrapper_multi_factors_ZikaModel.R"))
source(file.path("R", "calculate_MC_numbers.R"))
source(file.path("R", "reshape.R"))
source(file.path("R", "post_process.R"))
source(file.path("R", "plot_diagnostics.R"))
source(file.path("R", "calculate_MC_numbers.R"))


# define parameters -----------------------------------------------------------


experiment_name <- "vaccine_strategies_high_cov"

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

plot_interval <- 5 # years

vacc_child_coverage_values <- c(0.5, 0.8)

population_coverage_values <- c(0.5, 1)
  
vacc_starttime <- 1.5  

# 2 months campaign
vacc_stoptime <- vacc_starttime + 0.2 

# from 9 to 49
vacc_ages <- c(0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0) 

tot_cov_values <- as.vector(outer(vacc_child_coverage_values, population_coverage_values))

vacc_child_cov <- tot_cov_values[4]

params <- list(DT = my_dt,
               N_human = N_human_brazil,
               vacc_child_coverage = vacc_child_cov,
               vacc_child_starttime = vacc_starttime,
               vacc_child_stoptime = vacc_stoptime)


# load data -------------------------------------------------------------------


mr_pregn_data <- readRDS(file.path("output", "microcephaly_risk_probs.rds"))

br_brazil_age <- readRDS(file.path("output", "age_specific_birth_rates.rds"))


# run -------------------------------------------------------------------------


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

Ntotal <- out$Ntotal

time_steps <- out$TIME

tt <- out$TIME

time <- max(tt)

brks <- seq(from = 0, to = time, by = 364 * plot_interval)

MC <- calculate_microcases(N_inf = n_infections, 
                           pregnancy_risk_curve = mr_pregn_data, 
                           birth_rates = br_brazil_age, 
                           N_tot = Ntotal, 
                           baseline_probM = mr_baseline)


# plot number of new infections -----------------------------------------------


## by age, patches and vaccine status
melted_array_full <- melt_sim_output_array(n_infections, tt)

melted_array_nv <- subset(melted_array_full, vaccine == 1 & patch == 1)

p1 <- plot_diagnostics_by_age(melted_array_nv, "new infections", "Non vaccinated")

melted_array_v <- subset(melted_array_full, vaccine == 2 & patch == 1)

p2 <- plot_diagnostics_by_age(melted_array_v, "new infections", "Vaccinated")
 
 
## by patches and vaccine status
sum_over_ages <- sum_across_array_dims(n_infections, c(1, 3, 4))

Nt_sum_over_ages <- sum_across_array_dims(Ntotal, c(1, 3, 4))

prop_sum_over_ages <- ifelse(Nt_sum_over_ages == 0, 0, sum_over_ages / Nt_sum_over_ages)

melted_sum_a <- melt_sim_output_array_2(prop_sum_over_ages, tt)

p3 <- plot_diagnostics_by_p_v(melted_sum_a, "vaccine", "new infections (prop)")

melted_sum_a_b <- melt_sim_output_array_2(sum_over_ages, tt)
  
p3_b <- plot_diagnostics_by_p_v(melted_sum_a_b, "vaccine", "new infections")


## by vaccine status
sum_over_ages_patches <- sum_across_array_dims(n_infections, c(1, 3))

Nt_sum_over_ages_patches <- sum_across_array_dims(Ntotal, c(1, 3))

prop_sum_over_ages_patches <- ifelse(Nt_sum_over_ages_patches == 0, 0, 
                                     sum_over_ages_patches / Nt_sum_over_ages_patches)

melted_sum_a_p <- melt_sim_output_array_3(prop_sum_over_ages_patches, tt)

p4 <- plot_diagnostics_by_vaccine(melted_sum_a_p, "vaccine", "new infections") 


# save plots ------------------------------------------------------------------


save_plot(p1, out_fig_dir, "inf_1_by_age_nv", wdt = 12, hgt = 8)
save_plot(p2, out_fig_dir, "inf_1_by_age_v", wdt = 12, hgt = 8)
save_plot(p3, out_fig_dir, "inf_1_by_patch_vaccine_prop", wdt = 17, hgt = 17)
save_plot(p3_b, out_fig_dir, "inf_1_by_patch_vaccine_n", wdt = 17, hgt = 17)
save_plot(p4, out_fig_dir, "inf_1_by_vaccine", wdt = 12, hgt = 8)
