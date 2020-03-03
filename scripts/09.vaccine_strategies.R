
## devtools::install_github("mrc-ide/ZikaModel")

library(ZikaModel)
library(ggplot2)
library(purrr)
library(gridExtra)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "calculate_MC_numbers.R"))
source(file.path("R", "reshape.R"))
source(file.path("R", "aggregate.R"))
source(file.path("R", "post_process.R"))
source(file.path("R", "plot_diagnostics.R"))
source(file.path("R", "plot_layout.R"))


# define parameters -----------------------------------------------------------


my_id <- 5

exp_des_nm <- "experimental_design_2"

vacc_coverage_values <- c(0, 0.5)

prop_immune_values <- c(0, 0.25)

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

vacc_starttime <- 1.7  

vacc_duration <- 3.5 # 0.16

# from 9 to 49
vacc_ages <- c(0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0) 

odin_model_path <- system.file("extdata/odin_model_determ.R", package = "ZikaModel")

diagnostics_to_save <- c("inf_1", "Ntotal", "MC")


# pre processing --------------------------------------------------------------


experiment_name <- paste0("experiment_", my_id)

experiment_path <- file.path("vaccine_strategies", experiment_name)

out_fig_dir <- file.path("figures", experiment_path)

out_tab_dir <- file.path("output", experiment_path)

pl_ttl <- gsub("_+", " ", experiment_name)

plot_type <- c("total", "by_patch", "by_vaccine", "by_age") 

exp_des <- expand.grid(vacc_cov = vacc_coverage_values,
                       prop_immune = prop_immune_values)

exp_des$id <- seq_len(nrow(exp_des)) + 4

exp_des_one <- exp_des[exp_des$id == my_id,]

vacc_child_cov <- exp_des_one[, "vacc_cov"]

prop_immune <- exp_des_one[, "prop_immune"]

integer_time_steps <- (364 * time_years) / my_dt

its <- seq(0, integer_time_steps, 1)

vacc_stoptime <- vacc_starttime + vacc_duration  

params <- list(DT = my_dt,
               vacc_child_coverage = vacc_child_cov,
               vacc_child_starttime = vacc_starttime,
               vacc_child_stoptime = vacc_stoptime,
               other_prop_immune = prop_immune,
               Mwt_mean = 1)


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

MC <- calculate_microcases(N_inf = infections, 
                           pregnancy_risk_curve = mr_pregn_data, 
                           birth_rates = br_brazil_age, 
                           N_tot = Ntotal, 
                           baseline_probM = mr_baseline)

out$MC <- MC

tt <- out$TIME

time <- length(tt)


# plotting --------------------------------------------------------------------


## totals 

plot_total <- c("S", "I1", "R1", "Ntotal", "births", "inf_1", "MC")

plot_total_inc <- c("inf_1", "MC")

plot_total_inc_lab <- paste0("w_", plot_total_inc)

plot_mean <- c("Kcav", "eipav", "Deltaav", "R0t_1av", "FOI1av")
  
plot_total_p <- c("S", "I1", "R1", "Ntotal", "pS", "pI1", "pR1", "births", "inf_1", "MC")

y_labels <- c("number", "number", "number", "number", "proportion", "proportion", 
              "proportion", "number", "number", "number",
              "weekly number/1000", "weekly number/1000", 
              rep("mean patch", 5))

titles <- c("Susceptibles", "Infected", "Recovered", "Total",
            "Susceptibles", "Infected", "Recovered",
            "Births", "New infections", "Microcephaly", 
            "Weekly new infections", "Weekly new microcephaly",
            "Carrying capacity", "Extrinsic Incubation Period", 
            "Adult mosquito daily mortality rate", expression("R"["0"] * ""), 
            "FOI (within patches)")

ls_total_1 <- out[plot_total]

ls_total_2 <- out[plot_total_inc]

ls_mean <- out[plot_mean]

to_plot_mean <- lapply(ls_mean, cbind_time, tt)

names(ls_total_2) <- plot_total_inc_lab

to_plot_total_1 <- lapply(ls_total_1, aggregate_and_reshape_total, tt) 

pS <- calculate_SIR_proportions(to_plot_total_1$S$value, to_plot_total_1$Ntotal$value, tt)
pI1 <- calculate_SIR_proportions(to_plot_total_1$I1$value, to_plot_total_1$Ntotal$value, tt)
pR1 <- calculate_SIR_proportions(to_plot_total_1$R1$value, to_plot_total_1$Ntotal$value, tt)

to_plot_total_1$pS <- pS
to_plot_total_1$pI1 <- pI1
to_plot_total_1$pR1 <- pR1

to_plot_total_1a <- to_plot_total_1[plot_total]

to_plot_total_2 <- lapply(ls_total_2, cumsum_and_incidence_total, Ntotal)

to_plot_total <- c(to_plot_total_1a, to_plot_total_2, to_plot_mean)

y_labels <- setNames(y_labels, c(plot_total_p, plot_total_inc_lab, plot_mean)) 

titles <- setNames(titles, c(plot_total_p, plot_total_inc_lab, plot_mean))

all_plots_total <- imap(to_plot_total, 
                        ~ simple_plot(df = .x, 
                                      y_lab_title = y_labels[.y],
                                      ttl = titles[.y]))

plots_total_combined <- arrangeGrob_multi_files(all_plots_total, n_plots_per_file = 4)
  

# -----------------------------------------------------------------------------


## by patch  

plot_patch <- c("S", "I1", "R1", "Ntotal", "inf_1", "MC")

plot_patch_vector <- "births"

plot_patch_inc <- c("inf_1", "MC")

ls_patch_1 <- out[plot_patch]

ls_patch_2 <- out[plot_patch_vector]

ls_patch_3 <- out[plot_patch_inc]

plot_patch_inc_lab <- paste0("w_", plot_patch_inc)

names(ls_patch_3) <- plot_patch_inc_lab

to_plot_patch_1 <- lapply(ls_patch_1, aggregate_and_reshape_patch, tt) 

to_plot_patch_2 <- lapply(ls_patch_2, melt_sim_output_array_3, tt, "patch")

to_plot_patch_3 <- lapply(ls_patch_3, cumsum_and_incidence_patch, Ntotal)

to_plot_patch <- c(to_plot_patch_1, to_plot_patch_2, to_plot_patch_3)

y_labels <- c("number", "number", "number", "number", "number", "number", "number",
              "weekly number/1000", "weekly number/1000")

y_labels <- setNames(y_labels, c(plot_patch, plot_patch_vector, plot_patch_inc_lab)) 

titles <- c("Susceptibles", "Infected", "Recovered", "Total",
            "New infections", "Microcephaly", "Births", 
            "Weekly new infections", "Weekly new microcephaly")

titles <- setNames(titles, c(plot_patch, plot_patch_vector, plot_patch_inc_lab))

all_plots_patch <- imap(to_plot_patch, 
                        ~ plot_by_facet(df = .x,
                                        facet_var = "patch", 
                                        y_lab_title = y_labels[.y],
                                        ttl = titles[.y]))


# -----------------------------------------------------------------------------


## by vaccine status

plot_vaccine <- c("S", "I1", "R1", "Ntotal", "inf_1", "MC")

plot_vaccine_inc <- c("inf_1", "MC")

ls_vaccine_1 <- out[plot_vaccine]

ls_vaccine_2 <- out[plot_vaccine_inc]

plot_vaccine_inc_lab <- paste0("w_", plot_vaccine_inc)

names(ls_vaccine_2) <- plot_vaccine_inc_lab

to_plot_vaccine_1 <- lapply(ls_vaccine_1, aggregate_and_reshape_vaccine, tt) 

to_plot_vaccine_2 <- lapply(ls_vaccine_2, cumsum_and_incidence_vaccine, Ntotal)

to_plot_vaccine <- c(to_plot_vaccine_1, to_plot_vaccine_2)

y_labels <- c("number", "number", "number", "number", "number", "number",
              "weekly number/1000", "weekly number/1000")

y_labels <- setNames(y_labels, c(plot_vaccine, plot_vaccine_inc_lab)) 

titles <- c("Susceptibles", "Infected", "Recovered", "Total",
            "New infections", "Microcephaly",
            "Weekly new infections", "Weekly new microcephaly")

titles <- setNames(titles, c(plot_vaccine, plot_vaccine_inc_lab))

all_plots_vaccine <- imap(to_plot_vaccine, 
                          ~ plot_by_line(df = .x, 
                                         line_var = "vaccine",
                                         y_lab_title = y_labels[.y],
                                         ttl = titles[.y]))

plots_vaccine_combined <- arrangeGrob_multi_files(all_plots_vaccine, 
                                                  n_plots_per_file = 4, 
                                                  legend = TRUE)


# -----------------------------------------------------------------------------


## by patch and vaccine status (for one patch)

plot_age <- c("S", "I1", "R1", "Ntotal", "inf_1", "MC")

plot_age_inc <- c("inf_1", "MC")

ls_age_1 <- out[plot_age]

ls_age_2 <- out[plot_age_inc]

plot_age_inc_lab <- paste0("w_", plot_age_inc)

names(ls_age_2) <- plot_age_inc_lab

to_plot_age_1 <- lapply(ls_age_1, aggregate_and_reshape_age, tt) 

to_plot_age_2 <- lapply(ls_age_2, cumsum_and_incidence_age, Ntotal)

to_plot_age <- c(to_plot_age_1, to_plot_age_2)

y_labels <- c("number", "number", "number", "number", "number", "number",
              "weekly number/1000", "weekly number/1000")

y_labels <- setNames(y_labels, c(plot_age, plot_age_inc_lab)) 

titles <- c("Susceptibles", "Infected", "Recovered", "Total",
            "New infections", "Microcephaly", 
            "Weekly new infections", "Weekly new microcephaly")

titles <- setNames(titles, c(plot_age, plot_age_inc_lab))

all_plots_age <- imap(to_plot_age, 
                      ~ plot_by_line_facet(df = .x, 
                                           line_var = "age", 
                                           facet_var = "vaccine",
                                           y_lab_title = y_labels[.y],
                                           ttl = titles[.y]))

plots_age_combined <- arrangeGrob_multi_files(all_plots_age, 
                                              n_plots_per_file = 2,
                                              legend = TRUE)


# save plots ------------------------------------------------------------------


save_01 <- imap(plots_total_combined,
                ~ save_plot(plot_obj = .x,
                            out_pth = out_fig_dir, 
                            out_fl_nm = paste(plot_type[1], .y, sep = "_"), 
                            wdt = 17, 
                            hgt = 12))

save_02 <- imap(all_plots_patch,
                ~ save_plot(plot_obj = .x,
                            out_pth = out_fig_dir, 
                            out_fl_nm = paste(plot_type[2], .y, sep = "_"), 
                            wdt = 17, 
                            hgt = 17))

save_03 <- imap(plots_vaccine_combined,
                ~ save_plot(plot_obj = .x,
                            out_pth = out_fig_dir, 
                            out_fl_nm = paste(plot_type[3], .y, sep = "_"), 
                            wdt = 17, 
                            hgt = 12))

save_04 <- imap(plots_age_combined,
                ~ save_plot(plot_obj = .x,
                            out_pth = out_fig_dir, 
                            out_fl_nm = paste(plot_type[4], .y, sep = "_"), 
                            wdt = 17, 
                            hgt = 12))


# save rds --------------------------------------------------------------------


rds_out <- out[diagnostics_to_save]

write_out_rds(rds_out, out_tab_dir, paste0("diagnostics_", my_id))                

                           
# save tables -----------------------------------------------------------------


write_out_csv(exp_des, file.path("output", "vaccine_strategies"), exp_des_nm) 
