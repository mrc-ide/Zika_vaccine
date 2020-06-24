
# devtools::install_github("mrc-ide/ZikaModel")

library(ZikaModel)
library(ggplot2)
library(patchwork)
library(dplyr)

source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


out_dir <- file.path("figures", "wolbachia")
out_dir_2 <- file.path(out_dir, "patch")

hgt <- 8
wdt <- 13
hgt_p <- 15
wdt_p <- 18

Wb_introlevel <- 0.1 # proportion of the initial number of wt mosquitoes 


# run -------------------------------------------------------------------------


r1 <- run_deterministic_model(time_period = 365 * 5,
                              Wb_introlevel = Wb_introlevel)


# plot totals -----------------------------------------------------------------


## format
Ntotal <- format_output_H(r1, var_select = "Ntotal")
inf_1 <- format_output_H(r1, var_select = "inf_1")
MC <- format_output_H(r1, var_select = "MC")
inf_1_w <- format_output_H(r1, var_select = "inf_1_w")
MC_w <- format_output_H(r1, var_select = "MC_w")

Lwt <- format_output_M(r1, var_select = "Lwt")
Mwt_S <- format_output_M(r1, var_select = "Mwt_S")
Mwt_E1 <- format_output_M(r1, var_select = "Mwt_E1")
Mwt_E2 <- format_output_M(r1, var_select = "Mwt_E2")
Mwt_I1 <- format_output_M(r1, var_select = "Mwt_I1")

Lwb <- format_output_M(r1, var_select = "Lwb")
Mwb_S <- format_output_M(r1, var_select = "Mwb_S")
Mwb_E1 <- format_output_M(r1, var_select = "Mwb_E1")
Mwb_E2 <- format_output_M(r1, var_select = "Mwb_E2")
Mwb_I1 <- format_output_M(r1, var_select = "Mwb_I1")
prop_wb <- format_output_Mprop(r1)

# plot
humans_plot <- plot(r1, type = "H")

Ntotal_plot <- ggplot(Ntotal, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Total number of humans") +
  theme_bw()

inf_1_plot <- ggplot(inf_1, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Daily infections") +
  theme_bw()

MC_plot <- ggplot(MC, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Daily microcepahly cases") +
  theme_bw()

inf_1_w_plot <- ggplot(inf_1_w, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Weekly infections/1000") +
  theme_bw()

MC_w_plot <- ggplot(MC_w, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Weekly microcepahly cases/1000") +
  theme_bw()

extra_H_plot <- Ntotal_plot + inf_1_plot + MC_plot + inf_1_w_plot + MC_w_plot +
  plot_layout(ncol = 2)

Lwt_plot <- ggplot(Lwt, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Larvae wild type") +
  theme_bw()

Mwt_S_plot <- ggplot(Mwt_S, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Susceptibles wild type") +
  theme_bw()

Mwt_E1_plot <- ggplot(Mwt_E1, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Exposed stage 1 wild type") +
  theme_bw()

Mwt_E2_plot <- ggplot(Mwt_E2, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Exposed stage 2 wild type") +
  theme_bw()

Mwt_I1_plot <- ggplot(Mwt_I1, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Infectious wild type") +
  theme_bw()

extra_Mwt_plot <- Lwt_plot + Mwt_S_plot + Mwt_E1_plot +  Mwt_E2_plot +
  Mwt_I1_plot + plot_layout(ncol = 2)

Lwb_plot <- ggplot(Lwb, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Larvae wb type") +
  theme_bw()

Mwb_S_plot <- ggplot(Mwb_S, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Susceptibles wb type") +
  theme_bw()

Mwb_E1_plot <- ggplot(Mwb_E1, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Exposed stage 1 wb type") +
  theme_bw()

Mwb_E2_plot <- ggplot(Mwb_E2, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Exposed stage 2 wb type") +
  theme_bw()

Mwb_I1_plot <- ggplot(Mwb_I1, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Infectious wb type") +
  theme_bw()

prop_wb_plot <- ggplot(prop_wb, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("Proportion") +
  ggtitle("Proportion of wb type") +
  theme_bw()

extra_Mwb_plot <- Lwb_plot + Mwb_S_plot + Mwb_E1_plot +  Mwb_E2_plot +
  Mwb_I1_plot + prop_wb_plot + plot_layout(ncol = 2)


# save
save_plot(humans_plot, out_dir, "human_compartments", wdt = wdt, hgt = hgt)
save_plot(extra_H_plot, out_dir, "extra_H_diagnostics", wdt = 18, hgt = 15)
save_plot(extra_Mwt_plot, out_dir, "extra_Mwt_diagnostics", wdt = 18, hgt = 15)
save_plot(extra_Mwb_plot, out_dir, "extra_Mwb_diagnostics", wdt = 18, hgt = 15)


# plot by patch ---------------------------------------------------------------


## format
Ntotal_p <- format_output_H(r1, var_select = "Ntotal", keep = "patch")
inf_1_p <- format_output_H(r1, var_select = "inf_1", keep = "patch")
MC_p <- format_output_H(r1, var_select = "MC", keep = "patch")
inf_1_w_p <- format_output_H(r1, var_select = "inf_1_w", keep = "patch")
MC_w_p <- format_output_H(r1, var_select = "MC_w", keep = "patch")

Lwt_p <- format_output_M(r1, var_select = "Lwt", keep = "patch")
Mwt_S_p <- format_output_M(r1, var_select = "Mwt_S", keep = "patch")
Mwt_E1_p <- format_output_M(r1, var_select = "Mwt_E1", keep = "patch")
Mwt_E2_p <- format_output_M(r1, var_select = "Mwt_E2", keep = "patch")
Mwt_I1_p <- format_output_M(r1, var_select = "Mwt_I1", keep = "patch")

Lwb_p <- format_output_M(r1, var_select = "Lwb", keep = "patch")
Mwb_S_p <- format_output_M(r1, var_select = "Mwb_S", keep = "patch")
Mwb_E1_p <- format_output_M(r1, var_select = "Mwb_E1", keep = "patch")
Mwb_E2_p <- format_output_M(r1, var_select = "Mwb_E2", keep = "patch")
Mwb_I1_p <- format_output_M(r1, var_select = "Mwb_I1", keep = "patch")
prop_wb_p <- format_output_Mprop(r1, keep = "patch")

# plot
humans_p_plot <- plot(r1, type = "H", keep = "patch")

Ntotal_p_plot <- ggplot(Ntotal_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Total number of humans") +
  theme_bw()

inf_1_p_plot <- ggplot(inf_1_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Daily infections") +
  theme_bw()

MC_p_plot <- ggplot(MC_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Daily microcepahly cases") +
  theme_bw()

inf_1_w_p_plot <- ggplot(inf_1_w_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Weekly infections/1000") +
  theme_bw()

MC_w_p_plot <- ggplot(MC_w_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Weekly microcepahly cases/1000") +
  theme_bw()

Lwt_p_plot <- ggplot(Lwt_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Larvae wild type") +
  theme_bw()

Mwt_S_p_plot <- ggplot(Mwt_S_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Susceptibles wild type") +
  theme_bw()

Mwt_E1_p_plot <- ggplot(Mwt_E1_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Exposed stage 1 wild type") +
  theme_bw()

Mwt_E2_p_plot <- ggplot(Mwt_E2_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Exposed stage 2 wild type") +
  theme_bw()

Mwt_I1_p_plot <- ggplot(Mwt_I1_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Infectious wild type") +
  theme_bw()

Lwb_p_plot <- ggplot(Lwb_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Larvae wb type") +
  theme_bw()

Mwb_S_p_plot <- ggplot(Mwb_S_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Susceptibles wb type") +
  theme_bw()

Mwb_E1_p_plot <- ggplot(Mwb_E1_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Exposed stage 1 wb type") +
  theme_bw()

Mwb_E2_p_plot <- ggplot(Mwb_E2_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Exposed stage 2 wb type") +
  theme_bw()

Mwb_I1_p_plot <- ggplot(Mwb_I1_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Infectious wb type") +
  theme_bw()

prop_wb_p_plot <- ggplot(prop_wb_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("Proportion") +
  ggtitle("Proportion of wb type mosquitoes") +
  theme_bw()

# save
save_plot(humans_p_plot, out_dir_2, "human_compartments", wdt = wdt_p, hgt = hgt_p)
save_plot(Ntotal_p_plot, out_dir_2, "Ntotal", wdt = wdt_p, hgt = hgt_p)
save_plot(inf_1_p_plot, out_dir_2, "inf_1", wdt = wdt_p, hgt = hgt_p)
save_plot(MC_p_plot, out_dir_2, "MC", wdt = wdt_p, hgt = hgt_p)
save_plot(inf_1_w_p_plot, out_dir_2, "inf_1_w", wdt = wdt_p, hgt = hgt_p)
save_plot(MC_w_p_plot, out_dir_2, "MC_w", wdt = wdt_p, hgt = hgt_p)
save_plot(Lwt_p_plot, out_dir_2, "Lwt", wdt = wdt_p, hgt = hgt_p)
save_plot(Mwt_S_p_plot, out_dir_2, "Mwt_S", wdt = wdt_p, hgt = hgt_p)
save_plot(Mwt_E1_p_plot, out_dir_2, "Mwt_E1", wdt = wdt_p, hgt = hgt_p)
save_plot(Mwt_E2_p_plot, out_dir_2, "Mwt_E2", wdt = wdt_p, hgt = hgt_p)
save_plot(Mwt_I1_p_plot, out_dir_2, "Mwt_I1", wdt = wdt_p, hgt = hgt_p)
save_plot(Lwb_p_plot, out_dir_2, "Lwb", wdt = wdt_p, hgt = hgt_p)
save_plot(Mwb_S_p_plot, out_dir_2, "Mwb_S", wdt = wdt_p, hgt = hgt_p)
save_plot(Mwb_E1_p_plot, out_dir_2, "Mwb_E1", wdt = wdt_p, hgt = hgt_p)
save_plot(Mwb_E2_p_plot, out_dir_2, "Mwb_E2", wdt = wdt_p, hgt = hgt_p)
save_plot(Mwb_I1_p_plot, out_dir_2, "Mwb_I1", wdt = wdt_p, hgt = hgt_p)
save_plot(prop_wb_p_plot, out_dir_2, "prop_wb", wdt = wdt_p, hgt = hgt_p)
