
devtools::install_github("mrc-ide/ZikaModel")

library(ZikaModel)
library(reshape2)
library(ggplot2)
library(gridExtra)

source(file.path("R", "save_plot.R"))
source(file.path("R", "wrapper_to_save_plot.R"))
source(file.path("R", "reshape.R"))


# define parameters -----------------------------------------------------------


out_dir <- file.path("figures", "deterministic_wol_2")

agec <- c(1, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10)

death <- c(1e-10,
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

my_dt <- 0.5

odin_model_path <- system.file("extdata/odin_model_determ.R", package = "ZikaModel")

Wb_starttime <- 1 # years
Wb_introduration_days <- 30 # days
Wb_introlevel <- 0.1 # proportion of the initial number of wt mosquitoes 

Wb_introduration <- Wb_introduration_days / my_dt


# run -------------------------------------------------------------------------


create_generator <- create_r_model(odin_model_path = odin_model_path,
                                   agec = agec,
                                   death = death,
                                   nn_links = nn_links,
                                   amplitudes_phases = amplitudes_phases,
                                   DT = my_dt,
                                   season = TRUE,
                                   Wb_starttime = Wb_starttime,
                                   Wb_introduration = Wb_introduration,
                                   Wb_introlevel = Wb_introlevel)

gen <- create_generator$generator(user = create_generator$state)

integer_time_steps <- (364 * time_years) / my_dt

its <- seq(0, integer_time_steps, 1)

mod_run <- gen$run(its)


# post processing -------------------------------------------------------------


out <- gen$transform_variables(mod_run)

out_2 <- post_processing(out)

p1 <- plot_compartments(out_2$compartments)

save_plot(plot_obj = p1,
          out_pth = out_dir,
          out_fl_nm = "human_compartments.png",
          wdt = 14,
          hgt = 9)

p2 <- plot_demographics(out_2$demographics)

lapply(seq_along(p2),
       wrapper_to_save_plot,
       p2,
       out_fl_nm = "human_demographics",
       out_pth = out_dir,
       wdt = 18,
       hgt = 10)


# extract mosquitoes diagnostics ----------------------------------------------


diagno_mos_wt <- c("Lwt", "Mwt_S", "Mwt_E1", "Mwt_E2", "Mwt_I1", "Mwt_tot", "Lwt_birth",
                   "Lwt_mature", "Mwt_inf1")

diagno_mos_wb <- c("Lwb", "Mwb_S", "Mwb_E1", "Mwb_E2", "Mwb_I1", "Mwb_tot", "Lwb_birth",
                   "Lwb_mature", "Mwb_inf1", "Mwb_intro")

diagno_mos <- c(diagno_mos_wt, diagno_mos_wb)

dia_mos <- setNames(out[diagno_mos], diagno_mos)

mossum <- lapply(dia_mos, function(x){apply(x, 1, sum)})

mat_M <- do.call("cbind", mossum)

df_M <- as.data.frame(mat_M)

tt <- out$TIME
time <- max(tt)

df_M$time <- tt
df_M_melt <- melt(df_M,
                  id.vars = "time",
                  variable.name = "diagnostic")

diagno_levs <- c(diagno_mos_wt, diagno_mos_wb)

df_M_melt$diagnostic <- factor(df_M_melt$diagnostic, levels = diagno_levs, labels = diagno_levs)

p3 <- plot_demographics(df_M_melt)

lapply(seq_along(p3),
       wrapper_to_save_plot,
       p3,
       out_fl_nm = "mosquitoes_demographics",
       out_pth = out_dir,
       wdt = 18,
       hgt = 10)



# -----------------------------------------------------------------------------
#
# Plot mosquitoes diagnostics by patch
#
# -----------------------------------------------------------------------------



out_dir_1 <- file.path(out_dir, "patch")

my_breaks_2 <- seq(from = 0, to = time, by = 364 * 10)

diagno_mos_reshaped <- lapply(diagno_mos, reshape_by_patch, out)

for (i in seq_along(diagno_mos_reshaped)) {
  
  one_dat <- diagno_mos_reshaped[[i]]
  
  one_ggttl <- diagno_mos[i]
  
  one_plot <- ggplot(data = one_dat, aes(x = x, y = y)) +
    geom_line(color = 'royalblue', size = 0.5) +
    facet_wrap(~ patch) +
    scale_x_continuous("Years", breaks = my_breaks_2, labels = my_breaks_2/364) +
    scale_y_continuous("") +
    ggtitle(one_ggttl) +
    theme_bw(base_size = 10)
  
  out_fl_nm <- paste0(one_ggttl, ".png")
  
  save_plot(plot_obj = one_plot, out_pth = out_dir_1, 
            out_fl_nm = out_fl_nm, wdt = 18, hgt = 15)
  
}


# plot proportions ------------------------------------------------------------

# it does not make sense to take the mean across patches of these 
# if seasonality is ON


diagno_mos_prop <- c("Mwt_propinf", "Mwb_propinf", "M_propinf", "prop_wb")

diagno_mos_reshaped_2 <- lapply(diagno_mos_prop, reshape_by_patch, out)

y_brks <- seq(0, 1, 0.2)

for (i in seq_along(diagno_mos_reshaped_2)) {
  
  one_dat <- diagno_mos_reshaped_2[[i]]
  
  one_ggttl <- diagno_mos_prop[i]
    
  one_plot <- ggplot(data = one_dat, aes(x = x, y = y)) +
    geom_line(color = 'royalblue', size = 0.5) +
    facet_wrap(~ patch) +
    scale_x_continuous("Years", breaks = my_breaks_2, labels = my_breaks_2/364) +
    scale_y_continuous("Proportion", 
                       breaks = y_brks, 
                       labels = y_brks, 
                       limits = c(min(y_brks), max(y_brks))) +
    ggtitle(one_ggttl) +
    theme_bw(base_size = 10)
  
  out_fl_nm <- paste0(one_ggttl, ".png")
  
  save_plot(plot_obj = one_plot, out_pth = out_dir_1, 
            out_fl_nm = out_fl_nm, wdt = 18, hgt = 15)
  
}
