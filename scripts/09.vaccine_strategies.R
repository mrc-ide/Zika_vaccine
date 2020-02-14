

## devtools::install_github("mrc-ide/ZikaModel")

library(ZikaModel)
library(ggplot2)
library(viridis)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "calculate_MC_numbers.R"))
source(file.path("R", "reshape.R"))
source(file.path("R", "aggregate.R"))
source(file.path("R", "post_process.R"))
source(file.path("R", "plot_diagnostics.R"))


# define parameters -----------------------------------------------------------


my_cov_value <- 0.25
  
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

mr_baseline <- 0.0002

plot_interval <- 5 # years

vacc_child_coverage_values <- c(0.5, 0.8)

population_coverage_values <- c(0.5, 1)
  
vacc_starttime <- 1.5  

# 2 months campaign
vacc_stoptime <- vacc_starttime + 0.2 

# from 9 to 49
vacc_ages <- c(0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0) 

odin_model_path <- system.file("extdata/odin_model_determ.R", package = "ZikaModel")

trial_name <- paste0("coverage_", my_cov_value * 100)

pl_ttl <- gsub("_+", " ", trial_name)


# pre processing --------------------------------------------------------------


experiment_name <- file.path("vaccine_strategies", trial_name)

out_fig_dir <- file.path("figures", experiment_name)

out_tab_dir <- file.path("output", experiment_name)

tags <- c("infections", "weekly_infections", "microcephaly_cases", "weekly_microcephaly_cases")

plot_type <- c("by_age", "by_patch", "by_vaccine", "total") 

all_combs <- expand.grid(tags, plot_type)

out_fl_nms <- paste(all_combs$Var1, all_combs$Var2, sep = "_")

vacc_pop_cov_combs <- as.vector(outer(vacc_child_coverage_values, population_coverage_values))

tot_cov_values <- c(0, vacc_pop_cov_combs, 1)

vacc_child_cov <- tot_cov_values[tot_cov_values == my_cov_value]

integer_time_steps <- (364 * time_years) / my_dt

its <- seq(0, integer_time_steps, 1)

params <- list(DT = my_dt,
               vacc_child_coverage = vacc_child_cov,
               vacc_child_starttime = vacc_starttime,
               vacc_child_stoptime = vacc_stoptime)


# load data -------------------------------------------------------------------


mr_pregn_data <- readRDS(file.path("output", "microcephaly_risk_probs.rds"))

br_brazil_age <- readRDS(file.path("output", "age_specific_birth_rates.rds"))


# run -------------------------------------------------------------------------


create_generator <- create_r_model(odin_model_path = odin_model_path,
                                   agec = age_init,
                                   death = deathrt,
                                   nn_links = nn_links,
                                   amplitudes_phases = amplitudes_phases,
                                   vaccine_age = vacc_ages,
                                   params = params)

gen <- create_generator$generator(user = create_generator$state)

mod_run <- gen$run(its)


# post processing -------------------------------------------------------------


out <- gen$transform_variables(mod_run)

infections <- out$inf_1

Ntotal <- out$Ntotal

time_steps <- out$TIME

tt <- out$TIME

time <- max(tt)

MC <- calculate_microcases(N_inf = infections, 
                           pregnancy_risk_curve = mr_pregn_data, 
                           birth_rates = br_brazil_age, 
                           N_tot = Ntotal, 
                           baseline_probM = mr_baseline)

## aggregate (age, vaccine status, patch)
sum_av_infections <- sum_across_array_dims(infections, c(1, 4))
sum_ap_infections <- sum_across_array_dims(infections, c(1, 3))
sum_apv_infections <- sum_across_array_dims(infections, 1)

sum_av_MC <- sum_across_array_dims(MC, c(1, 4))
sum_ap_MC <- sum_across_array_dims(MC, c(1, 3))
sum_apv_MC <- sum_across_array_dims(MC, 1)

sum_av_Ntotal <- sum_across_array_dims(Ntotal, c(1, 4))
sum_ap_Ntotal <- sum_across_array_dims(Ntotal, c(1, 3))
sum_apv_Ntotal <- sum_across_array_dims(Ntotal, 1)

## cumulative sums (time)
cumsum_infections <- cumsum_across_array_dims(infections, c(2, 3, 4))
cumsum_sum_av_infections <- cumsum_across_array_dims(sum_av_infections, 2)
cumsum_sum_ap_infections <- cumsum_across_array_dims(sum_ap_infections, 2)
cumsum_sum_apv_infections <- cumsum(sum_apv_infections)

cumsum_MC <- cumsum_across_array_dims(MC, c(2, 3, 4))
cumsum_sum_av_MC <- cumsum_across_array_dims(sum_av_MC, 2)
cumsum_sum_ap_MC <- cumsum_across_array_dims(sum_ap_MC, 2)
cumsum_sum_apv_MC <- cumsum(sum_apv_MC)

## calculate incidence 
weekly_infections <- calculate_incidence(cumsum_infections, Ntotal, 7)
weekly_infections_patch <- calculate_incidence(cumsum_sum_av_infections, sum_av_Ntotal, 7)
weekly_infections_vaccine <- calculate_incidence(cumsum_sum_ap_infections, sum_ap_Ntotal, 7)
weekly_infections_total <- calculate_incidence(cumsum_sum_apv_infections, sum_apv_Ntotal, 7)

weekly_MC <- calculate_incidence(cumsum_MC, Ntotal, 7)
weekly_MC_patch <- calculate_incidence(cumsum_sum_av_MC, sum_av_Ntotal, 7)
weekly_MC_vaccine <- calculate_incidence(cumsum_sum_ap_MC, sum_ap_Ntotal, 7)
weekly_MC_total <- calculate_incidence(cumsum_sum_apv_MC, sum_apv_Ntotal, 7)

## reshape (for plotting)
melt_infections <- melt_sim_output_array(infections, tt)
melt_sum_av_infections <- melt_sim_output_array_3(sum_av_infections, tt, "patch")
melt_sum_ap_infections <- melt_sim_output_array_3(sum_ap_infections, tt, "vaccine")
melt_sum_apv_infections <- cbind_time(sum_apv_infections, tt)

melt_w_infections <- melt_sim_output_array(weekly_infections, tt)
melt_sum_av_w_infections <- melt_sim_output_array_3(weekly_infections_patch, tt, "patch")
melt_sum_ap_w_infections <- melt_sim_output_array_3(weekly_infections_vaccine, tt, "vaccine")
melt_sum_apv_w_infections <- cbind_time(weekly_infections_total, tt)

melt_MC <- melt_sim_output_array(MC, tt)
melt_sum_av_MC <- melt_sim_output_array_3(sum_av_MC, tt, "patch")
melt_sum_ap_MC <- melt_sim_output_array_3(sum_ap_MC, tt, "vaccine")
melt_sum_apv_MC <- cbind_time(sum_apv_MC, tt)

melt_w_MC <- melt_sim_output_array(weekly_MC, tt)
melt_sum_av_w_MC <- melt_sim_output_array_3(weekly_MC_patch, tt, "patch")
melt_sum_ap_w_MC <- melt_sim_output_array_3(weekly_MC_vaccine, tt, "vaccine")
melt_sum_apv_w_MC <- cbind_time(weekly_MC_total, tt)

## subset patch 1
melt_infections_p1 <- subset(melt_infections, patch == 1)
melt_w_infections_p1 <- subset(melt_w_infections, patch == 1)
melt_MC_p1 <- subset(melt_MC, patch == 1)
melt_w_MC_p1 <- subset(melt_w_MC, patch == 1)


# plotting --------------------------------------------------------------------


## by age, patch and vaccine status
p_01 <- plot_by_line_facet(melt_infections_p1, "age", "vaccine", "infections", pl_ttl)
p_02 <- plot_by_line_facet(melt_w_infections_p1, "age", "vaccine", "weekly infections/1000", pl_ttl)
p_03 <- plot_by_line_facet(melt_MC_p1, "age", "vaccine", "micro cases", pl_ttl)
p_04 <- plot_by_line_facet(melt_w_MC_p1, "age", "vaccine", "weekly micro cases/1000", pl_ttl)

## by patch
p_05 <- plot_by_facet(melt_sum_av_infections, "patch", "infections")
p_06 <- plot_by_facet(melt_sum_av_w_infections, "patch", "weekly infections/1000")
p_07 <- plot_by_facet(melt_sum_av_MC, "patch", "micro cases")
p_08 <- plot_by_facet(melt_sum_av_w_MC, "patch", "weekly micro cases/1000")

## by vaccine status
p_09 <- plot_by_line(melt_sum_ap_infections, "vaccine", "infections", pl_ttl) 
p_10 <- plot_by_line(melt_sum_ap_w_infections, "vaccine", "weekly infections/1000", pl_ttl)
p_11 <- plot_by_line(melt_sum_ap_MC, "vaccine", "micro cases", pl_ttl)
p_12 <- plot_by_line(melt_sum_ap_w_MC, "vaccine", "weekly micro cases/1000", pl_ttl)

## total
p_13 <- simple_plot(melt_sum_apv_infections, "infections", pl_ttl)
p_14 <- simple_plot(melt_sum_apv_w_infections, "weekly infections/1000", pl_ttl)
p_15 <- simple_plot(melt_sum_apv_MC, "micro cases", pl_ttl)
p_16 <- simple_plot(melt_sum_apv_w_MC, "weekly micro cases/1000", pl_ttl)


# saving plots ----------------------------------------------------------------


save_plot(p_01, out_fig_dir, out_fl_nms[1], wdt = 12, hgt = 14)
save_plot(p_02, out_fig_dir, out_fl_nms[2], wdt = 12, hgt = 14)
save_plot(p_03, out_fig_dir, out_fl_nms[3], wdt = 12, hgt = 14)
save_plot(p_04, out_fig_dir, out_fl_nms[4], wdt = 12, hgt = 14)

save_plot(p_05, out_fig_dir, out_fl_nms[5], wdt = 17, hgt = 17)
save_plot(p_06, out_fig_dir, out_fl_nms[6], wdt = 17, hgt = 17)
save_plot(p_07, out_fig_dir, out_fl_nms[7], wdt = 17, hgt = 17)
save_plot(p_08, out_fig_dir, out_fl_nms[8], wdt = 17, hgt = 17)

save_plot(p_09, out_fig_dir, out_fl_nms[9], wdt = 12, hgt = 8)
save_plot(p_10, out_fig_dir, out_fl_nms[10], wdt = 12, hgt = 8)
save_plot(p_11, out_fig_dir, out_fl_nms[11], wdt = 12, hgt = 8)
save_plot(p_12, out_fig_dir, out_fl_nms[12], wdt = 12, hgt = 8)

save_plot(p_13, out_fig_dir, out_fl_nms[13], wdt = 12, hgt = 8)
save_plot(p_14, out_fig_dir, out_fl_nms[14], wdt = 12, hgt = 8)
save_plot(p_15, out_fig_dir, out_fl_nms[15], wdt = 12, hgt = 8)
save_plot(p_16, out_fig_dir, out_fl_nms[16], wdt = 12, hgt = 8)
