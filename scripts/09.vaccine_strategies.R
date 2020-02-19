
## devtools::install_github("mrc-ide/ZikaModel")

library(ZikaModel)
library(ggplot2)
library(viridis)
library(purrr)
library(gridExtra)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "calculate_MC_numbers.R"))
source(file.path("R", "reshape.R"))
source(file.path("R", "aggregate.R"))
source(file.path("R", "post_process.R"))
source(file.path("R", "plot_diagnostics.R"))


# define parameters -----------------------------------------------------------


my_id <- 4

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

vacc_coverage_values <- c(0, 0.5, 0.8, 1)

vacc_starttime <- 0  

# 2 months campaign
vacc_stoptime <- vacc_starttime + 50 

# from 9 to 49
vacc_ages <- c(0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0) 

odin_model_path <- system.file("extdata/odin_model_determ.R", package = "ZikaModel")

trial_name <- paste0("experiment_", my_id)

pl_ttl <- gsub("_+", " ", trial_name)

vacc_ages_labels <- c("1", "2_10", "11_20", "21_30", "31_40", "41_50", "51_60", "61_70", "71_80", "81_90", "91_100") 


# pre processing --------------------------------------------------------------


experiment_name <- file.path("vaccine_strategies", trial_name)

out_fig_dir <- file.path("figures", experiment_name)

out_tab_dir <- file.path("output", experiment_name)

plot_type <- c("total", "by_patch", "by_vaccine", "by_age") 

exp_des <- data.frame(id = seq_len(length(vacc_coverage_values)), 
                      vacc_cov = vacc_coverage_values)

write_out_csv(exp_des, file.path("output", "vaccine_strategies"), "experimental_design") 

exp_des_one <- exp_des[exp_des$id == my_id,]

vacc_child_cov <- exp_des_one[, "vacc_cov"]

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

plot_total <- c("S", "I1", "R1", "inf_1", "births", "Ntotal", "MC")

plot_total_inc <- c("inf_1", "MC")

ls_total_1 <- out[plot_total]

ls_total_2 <- out[plot_total_inc]

plot_total_inc_lab <- paste0("w_", plot_total_inc)

names(ls_total_2) <- plot_total_inc_lab

to_plot_total_1 <- lapply(ls_total_1, aggregate_and_reshape_total, tt) 

to_plot_total_2 <- lapply(ls_total_2, cumsum_and_incidence_total, Ntotal)

to_plot_total <- c(to_plot_total_1, to_plot_total_2)

y_labels <- c("number", "number", "number", "number", "number", "number", "number",
              "weekly number/1000", "weekly number/1000")

y_labels <- setNames(y_labels, c(plot_total, plot_total_inc_lab)) 

titles <- c("Susceptibles", "Infected", "Recovered", "New infections", 
            "Births", "Total", "Microcephaly", 
            "Weekly new infections", "Weekly new microcephaly")

titles <- setNames(titles, c(plot_total, plot_total_inc_lab))

all_plots_total <- imap(to_plot_total, 
                        ~ simple_plot(df = .x, 
                                      y_lab_title = y_labels[.y],
                                      ttl = titles[.y]))

all_plots_total_01 <- arrangeGrob(grobs = all_plots_total[1:4], nrow = 2, ncol = 2)
all_plots_total_02 <- arrangeGrob(grobs = all_plots_total[5:8], nrow = 2, ncol = 2)
all_plots_total_03 <- arrangeGrob(grobs = all_plots_total[9], nrow = 2, ncol = 2)

save_plot(all_plots_total_01, out_fig_dir, paste0(plot_type[1], "_1"), wdt = 17, hgt = 12)
save_plot(all_plots_total_02, out_fig_dir, paste0(plot_type[1], "_2"), wdt = 17, hgt = 12)
save_plot(all_plots_total_03, out_fig_dir, paste0(plot_type[1], "_3"), wdt = 17, hgt = 12)


# -----------------------------------------------------------------------------


## by patch  

plot_patch <- c("S", "I1", "R1", "inf_1", "Ntotal", "MC")

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

titles <- c("Susceptibles", "Infected", "Recovered", "New infections", 
            "Total", "Microcephaly", "Births", 
            "Weekly new infections", "Weekly new microcephaly")

titles <- setNames(titles, c(plot_patch, plot_patch_vector, plot_patch_inc_lab))

all_plots_patch <- imap(to_plot_patch, 
                        ~ plot_by_facet(df = .x,
                                        facet_var = "patch", 
                                        y_lab_title = y_labels[.y],
                                        ttl = titles[.y]))

imap(all_plots_patch,
     ~ save_plot(plot_obj = .x,
                 out_pth = out_fig_dir, 
                 out_fl_nm = paste(plot_type[2], .y, sep = "_"), 
                 wdt = 17, 
                 hgt = 17))


# -----------------------------------------------------------------------------


## by vaccine status

plot_vaccine <- c("S", "I1", "R1", "inf_1", "Ntotal", "MC")

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

titles <- c("Susceptibles", "Infected", "Recovered", "New infections", 
            "Total", "Microcephaly",
            "Weekly new infections", "Weekly new microcephaly")

titles <- setNames(titles, c(plot_vaccine, plot_vaccine_inc_lab))

all_plots_vaccine_leg <- imap(to_plot_vaccine, 
                          ~ plot_by_line(df = .x, 
                                         line_var = "vaccine",
                                         y_lab_title = y_labels[.y],
                                         ttl = titles[.y]))

mylegend <- g_legend(all_plots_vaccine_leg[[1]])

all_plots_vaccine <- imap(to_plot_vaccine, 
                          ~ plot_by_line(df = .x, 
                                         line_var = "vaccine",
                                         y_lab_title = y_labels[.y],
                                         ttl = titles[.y],
                                         leg_pos = "none"))

all_plots_vaccine_01 <- arrangeGrob(grobs = all_plots_vaccine[1:4], nrow = 2, ncol = 2)
all_plots_vaccine_02 <- arrangeGrob(grobs = all_plots_vaccine[5:8], nrow = 2, ncol = 2)

all_plots_vaccine_01_leg <- arrangeGrob(all_plots_vaccine_01, mylegend, 
                                        nrow = 2, 
                                        widths = 17, 
                                        heights = c(11, 1))

all_plots_vaccine_02_leg <- arrangeGrob(all_plots_vaccine_02, mylegend, 
                                        nrow = 2, 
                                        widths = 17, 
                                        heights = c(11, 1))

save_plot(all_plots_vaccine_01_leg, out_fig_dir, paste0(plot_type[3], "_1"), wdt = 17, hgt = 12)
save_plot(all_plots_vaccine_02_leg, out_fig_dir, paste0(plot_type[3], "_2"), wdt = 17, hgt = 12)


# -----------------------------------------------------------------------------


## by patch and vaccine status (for one patch)

plot_age <- c("S", "I1", "R1", "inf_1", "Ntotal", "MC")

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

titles <- c("Susceptibles", "Infected", "Recovered", "New infections", 
            "Total", "Microcephaly", 
            "Weekly new infections", "Weekly new microcephaly")

titles <- setNames(titles, c(plot_age, plot_age_inc_lab))

all_plots_age_leg <- imap(to_plot_age, 
                          ~ plot_by_line_facet(df = .x, 
                                               line_var = "age", 
                                               facet_var = "vaccine",
                                               y_lab_title = y_labels[.y],
                                               ttl = titles[.y]))

mylegend <- g_legend(all_plots_age_leg[[1]])

all_plots_age <- imap(to_plot_age, 
                      ~ plot_by_line_facet(df = .x, 
                                           line_var = "age", 
                                           facet_var = "vaccine",
                                           y_lab_title = y_labels[.y],
                                           ttl = titles[.y],
                                           leg_pos = "none"))

all_plots_age_01 <- arrangeGrob(grobs = all_plots_age[1:2], nrow = 1, ncol = 2)
all_plots_age_02 <- arrangeGrob(grobs = all_plots_age[3:4], nrow = 1, ncol = 2)
all_plots_age_03 <- arrangeGrob(grobs = all_plots_age[5:6], nrow = 1, ncol = 2)
all_plots_age_04 <- arrangeGrob(grobs = all_plots_age[7:8], nrow = 1, ncol = 2)

all_plots_age_01_leg <- arrangeGrob(all_plots_age_01, mylegend, 
                                    nrow = 2, 
                                    widths = 17, 
                                    heights = c(11, 1))

all_plots_age_02_leg <- arrangeGrob(all_plots_age_02, mylegend, 
                                    nrow = 2, 
                                    widths = 17, 
                                    heights = c(11, 1))

all_plots_age_03_leg <- arrangeGrob(all_plots_age_03, mylegend, 
                                    nrow = 2, 
                                    widths = 17, 
                                    heights = c(11, 1))

all_plots_age_04_leg <- arrangeGrob(all_plots_age_04, mylegend, 
                                    nrow = 2, 
                                    widths = 17, 
                                    heights = c(11, 1))

save_plot(all_plots_age_01_leg, out_fig_dir, paste0(plot_type[4], "_1"), wdt = 17, hgt = 12)
save_plot(all_plots_age_02_leg, out_fig_dir, paste0(plot_type[4], "_2"), wdt = 17, hgt = 12)
save_plot(all_plots_age_03_leg, out_fig_dir, paste0(plot_type[4], "_3"), wdt = 17, hgt = 12)
save_plot(all_plots_age_04_leg, out_fig_dir, paste0(plot_type[4], "_4"), wdt = 17, hgt = 12)
