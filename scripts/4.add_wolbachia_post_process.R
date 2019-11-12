
library(dplyr) 
library(ggplot2)

source(file.path("R", "save_plot.R"))
source(file.path("R", "reshape.R"))
source(file.path("R", "post_process.R"))

in_dir <- file.path("output", "wolbachia_factorial_exp")

out_dir <- file.path("figures", "wolbachia_factorial_exp")


# load data -------------------------------------------------------------------


dat <- readRDS(file.path(in_dir, "wolbachia_experiment.rds"))

exp_des <- read.csv(file.path(in_dir, "wolbachia_parameters_experiment.csv")) 


# run -------------------------------------------------------------------------


dat_m <- lapply(dat, apply_mean_across_patches)

time_years <- 50

my_dt <- 0.5

integer_time_steps <- (364 * time_years) / my_dt

its <- seq(0, integer_time_steps, 1)

tt <- its * my_dt

dat_m_pp <- lapply(seq_along(dat_m), post_process, dat_m, tt, exp_des)

dat_all <- do.call("rbind", dat_m_pp)

dat_all_j <- left_join(as.data.frame(dat_all), exp_des) 

dat_all_j$Wb_introlevel <- factor(dat_all_j$Wb_introlevel)

Wb_starttime_vals <- unique(dat_all_j$Wb_starttime)

Wb_introduration_vals <- unique(dat_all_j$Wb_introduration)

dat_all_j_2 <- dat_all_j

dat_all_j_2$Wb_starttime <- factor(dat_all_j_2$Wb_starttime, 
                                   levels = Wb_starttime_vals)

levels(dat_all_j_2$Wb_starttime) <- paste0("Year ", Wb_starttime_vals)

dat_all_j_2$Wb_introduration <- factor(dat_all_j_2$Wb_introduration, 
                                       levels = Wb_introduration_vals)

levels(dat_all_j_2$Wb_introduration) <- paste0(Wb_introduration_vals*my_dt, " days")

my_breaks <- seq(from = 0, to = max(tt), by = 364 * 5)

p <- ggplot(dat_all_j_2, aes(x = time, y = x, colour = Wb_introlevel)) +
  geom_line(size = 0.5) +
  facet_grid(Wb_starttime ~ Wb_introduration) +
  scale_x_continuous("Years", breaks = my_breaks, labels = my_breaks/364) +
  scale_y_continuous("Mean patch proportion") +
  ggtitle("Wolbachia mosquitoes") +
  theme_bw(base_size = 10)

save_plot(plot_obj = p, out_pth = out_dir, 
          out_fl_nm = "wolbachia_experiment.png", wdt = 20, hgt = 15)


# zoom in 1 year --------------------------------------------------------------


dat_all_j_sub <- subset(dat_all_j, Wb_starttime == 1)

dat_all_j_sub_2 <- dat_all_j_sub

dat_all_j_sub_2$Wb_introduration <- factor(dat_all_j_sub_2$Wb_introduration, 
                                           levels = Wb_introduration_vals)

levels(dat_all_j_sub_2$Wb_introduration) <- paste0(Wb_introduration_vals*my_dt, " days")

my_breaks_2 <- seq(from = 0, to = max(tt), by = 364)

p_year1 <- ggplot(dat_all_j_sub_2, aes(x = time, y = x, colour = Wb_introlevel)) +
  geom_line(size = 0.5) +
  facet_wrap(~ Wb_introduration, nrow = 2) +
  scale_x_continuous("Year", breaks = my_breaks_2, labels = my_breaks_2/364) +
  scale_y_continuous("Mean patch proportion") +
  coord_cartesian(xlim = c(364, 364*3)) +
  ggtitle("Wolbachia mosquitoes (Year 1 start)") +
  theme_bw(base_size = 9)

save_plot(plot_obj = p_year1, out_pth = out_dir, 
          out_fl_nm = "wolbachia_experiment_zoom.png", wdt = 11, hgt = 8)
