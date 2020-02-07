

## devtools::install_github("mrc-ide/ZikaModel")

library(ZikaModel)
library(reshape2)
library(ggplot2)
library(viridis)
library(gridExtra)

source(file.path("R", "wrapper_to_save_plot.R"))
source(file.path("R", "utility_functions.R"))
source(file.path("R", "reshape.R"))
source(file.path("R", "plot_diagnostics.R"))
source(file.path("R", "calculate_MC_numbers.R"))
source(file.path("R", "average.R"))


# define parameters -----------------------------------------------------------


out_dir <- file.path("figures", "microcephaly_risk")

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

time_years <- 5 # years

my_dt <- 1

time_frame <- (364 * time_years) / my_dt

mr_baseline <- 0.0002

N_human_brazil <- 200000000
season <- FALSE

plot_interval <- 0.5 # years


# load data -------------------------------------------------------------------


# microcephaly risk during pregnancy
mr_pregn_data <- readRDS(file.path("output", "microcephaly_risk_probs.rds"))
#mr_pregn_data[, "prob"] <- 0

br_brazil_age <- readRDS(file.path("output", "age_specific_birth_rates.rds"))

  
# run -------------------------------------------------------------------------


odin_model_path <- system.file("extdata/odin_model_determ.R", package = "ZikaModel")

create_generator <- create_r_model(odin_model_path = odin_model_path,
                                   agec = age_init,
                                   death = deathrt,
                                   nn_links = nn_links,
                                   amplitudes_phases = amplitudes_phases,
                                   DT = my_dt,
                                   season = season,
                                   N_human = N_human_brazil)

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


# plot number of new infections -----------------------------------------------


brks <- seq(from = 0, to = time, by = 364 * plot_interval)

inf_1_full_melt <- melt_sim_output_array(n_infections, tt)

inf_1_melt <- subset(inf_1_full_melt, vaccine == 1 & patch == 1)

p <- plot_diagnostics_by_age(inf_1_melt, "inf_1")

save_plot(p,
          out_dir,
          out_fl_nm = sprintf("inf_1_vaccine_%s_patch_%s", 1, 1),
          wdt = 12,
          hgt = 8)

inf_1_melt_p <- melt_by_patch(n_infections, tt)

inf_1_melt_p_plot <- plot_diagnostics_by_patch(inf_1_melt_p, "inf_1")

save_plot(inf_1_melt_p_plot,
          out_dir,
          out_fl_nm = "inf_1_by_patch",
          wdt = 17,
          hgt = 15)


# get MC number ---------------------------------------------------------------


MC_ZIKV <- calculate_microcases_ZIKV(n_infections, mr_pregn_data, br_brazil_age)
  
N_tot <- out$Ntotal

MC_baseline <- calculate_microcases_baseline(N_tot, mr_baseline, br_brazil_age)
  
MC_tot <- MC_baseline + MC_ZIKV 


# plot by patch and age -------------------------------------------------------
  
  
all_probs_2_full_melt <- melt_sim_output_array(MC_tot, tt)

all_probs_2_melt <- subset(all_probs_2_full_melt, vaccine == 1 & patch == 1)

p3 <- plot_diagnostics_by_age(all_probs_2_melt, "microcephaly cases")

save_plot(p3,
          out_dir,
          out_fl_nm = sprintf("microcephaly_cases_vaccine_%s_patch_%s", 1, 1),
          wdt = 12,
          hgt = 8)


# plot by patch ---------------------------------------------------------------


MC_tot_melt <- melt_by_patch(MC_tot, tt)

MC_tot_plot_p <- plot_diagnostics_by_patch(MC_tot_melt, "microcephaly cases")

# time window of the first epidemic, years
from_t <- 1
to_t <- 3

MC_tot_w <- summarize_in_window(MC_tot_melt, from_t, to_t)
  
MC_tot_plot_p_2 <- MC_tot_plot_p + 
  geom_text(data = MC_tot_w, 
            aes(x = 1*364, y = max_y*0.8, label = sprintf("y%s-%s = %s", from_t, to_t, n)), 
            hjust = 0, 
            col = "red",
            size = 3)

save_plot(MC_tot_plot_p_2,
          out_dir,
          out_fl_nm = "microcephaly_cases_by_patch",
          wdt = 17,
          hgt = 15)


# plot total ------------------------------------------------------------------


all_probs_2_sum <- apply(MC_tot, 1, sum)

df_2 <- data.frame(time = tt, dis = all_probs_2_sum)

p4 <- ggplot(df_2, aes(x = time, y = dis)) + 
  geom_line(size = 0.5, colour = "#63B8FF") + 
  scale_x_continuous(name = "Years", breaks = brks, 
                     labels = brks/364) + 
  scale_y_continuous(name = "microcephaly cases") + 
  theme_bw() + 
  theme(axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8), 
        strip.text.x = element_text(size = 10), 
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

save_plot(p4,
          out_dir,
          out_fl_nm = "microcephaly_cases_tot",
          wdt = 12,
          hgt = 8)


# calculate and plot number of births from Brazil birth rates 


n_tot <- out$Ntotal
  
n_births <- sweep(n_tot/2, MARGIN = 2, br_brazil_age, "*")

n_births_melt <- melt_by_patch(n_births, tt)

n_births_plot_p <- plot_diagnostics_by_patch(n_births_melt, "births")

n_births_w <- summarize_in_window(n_births_melt, from_t, to_t)

n_births_plot_p_2 <- n_births_plot_p + 
  geom_text(data = n_births_w, 
            aes(x = 1*364, y = max_y*0.8, label = sprintf("y%s-%s = %s", from_t, to_t, n)), 
            hjust = 0, 
            col = "red",
            size = 3)

save_plot(n_births_plot_p_2,
          out_dir,
          out_fl_nm = "births_by_patch",
          wdt = 17,
          hgt = 15)


# calculate and plot number of births from model  


n_births_model <- as.data.frame(out$births)
names(n_births_model) <- seq_len(21)
n_births_model$time <- tt
n_births_model_melt <- melt(n_births_model,
                            id.vars = "time",
                            variable.name = "patch")

n_births_model_plot_p <- plot_diagnostics_by_patch(n_births_model_melt, "births")

save_plot(n_births_model_plot_p,
          out_dir,
          out_fl_nm = "births_from_model_by_patch",
          wdt = 17,
          hgt = 15)
