
library(dplyr) 
library(ggplot2)
library(purrr)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "post_process.R"))

in_dir <- file.path("output", "wolbachia_factorial_exp")

out_dir <- file.path("figures", "wolbachia_factorial_exp")


# load data -------------------------------------------------------------------


dat <- readRDS(file.path(in_dir, "wolbachia_experiment.rds"))

exp_des <- read.csv(file.path(in_dir, "wolbachia_parameters_experiment.csv")) 


# run -------------------------------------------------------------------------


dat_id <- imap(seq_len(nrow(exp_des)), ~ cbind_id(x = dat[[.y]], id = .x))

dat_id_df <- do.call("rbind", dat_id)
  
wb_startime_lev <- unique(exp_des$Wb_starttime)
wb_startime_lev_new <- setNames(paste0("Year ", wb_startime_lev), wb_startime_lev)

wb_introdur_lev <- unique(exp_des$Wb_introduration)
wb_introdur_lev_new <- setNames(paste(wb_introdur_lev, " days"), wb_introdur_lev)

df_to_plot <- dat_id_df %>%
  left_join(exp_des) %>%
  mutate(Wb_introlevel = factor(Wb_introlevel)) %>%
  mutate(Wb_starttime = factor(Wb_starttime)) %>%
  mutate(Wb_introduration = factor(Wb_introduration)) %>% 
  mutate(Wb_starttime = recode(Wb_starttime, !!!wb_startime_lev_new)) %>%
  mutate(Wb_introduration = recode(Wb_introduration, !!!wb_introdur_lev_new))

p <- ggplot(df_to_plot, aes(x = t, y = y, colour = Wb_introlevel)) +
  geom_line(size = 0.5) +
  facet_grid(Wb_starttime ~ Wb_introduration) +
  scale_x_continuous("Time") +
  scale_y_continuous("Mean patch proportion") +
  ggtitle("Wolbachia mosquitoes") +
  theme_bw(base_size = 10)

save_plot(p, out_dir, "wolbachia_experiment.png", wdt = 20, hgt = 15)


# zoom in 1 year --------------------------------------------------------------


df_to_plot_sub <- df_to_plot %>%
  filter(Wb_starttime == "Year 1")

p_year1 <- ggplot(df_to_plot_sub, aes(x = t, y = y, colour = Wb_introlevel)) +
  geom_line(size = 0.5) +
  facet_wrap(~ Wb_introduration, nrow = 2) +
  scale_x_continuous("Time") +
  scale_y_continuous("Mean patch proportion") +
  coord_cartesian(xlim = c(365, 365*3)) +
  ggtitle("Wolbachia mosquitoes (Year 1 start)") +
  theme_bw(base_size = 9)

save_plot(p_year1, out_dir, "wolbachia_experiment_zoom.png", wdt = 11, hgt = 8)
