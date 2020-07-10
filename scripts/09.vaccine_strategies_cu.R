
## devtools::install_github("mrc-ide/ZikaModel")

library(ZikaModel)
library(ggplot2)
library(patchwork)
library(dplyr)

source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


experiment_path <- file.path("vaccine_strategies", "campaign", "experiment_1")

vacc_cu_cov <- 0

prop_immune <- 0

vacc_starttime <- 1.3  

hgt <- 8
wdt <- 13
hgt_p <- 15
wdt_p <- 18
hgt_v <- 7
wdt_v <- 15
hgt_avp <- 12
wdt_avp <- 17


# pre processing --------------------------------------------------------------


out_dir <- file.path("figures", experiment_path)
out_dir_2 <- file.path(out_dir, "patch")
out_dir_3 <- file.path(out_dir, "vaccine")
out_dir_4 <- file.path(out_dir, "all")


# run -------------------------------------------------------------------------


r1 <- run_deterministic_model(time_period = 365 * 5,
                              vacc_cu_coverage = vacc_cu_cov,
                              vacc_cu_time = vacc_starttime,
                              other_prop_immune = prop_immune)


# plot totals -----------------------------------------------------------------


## format
Ntotal <- format_output_H(r1, var_select = "Ntotal")
inf_1 <- format_output_H(r1, var_select = "inf_1")
MC <- format_output_H(r1, var_select = "MC")
inf_1_w <- format_output_H(r1, var_select = "inf_1_w")
MC_w <- format_output_H(r1, var_select = "MC_w")

Lwt <- format_output_M(r1, var_select = "Lwt")
Kc <- format_output_M(r1, var_select = "Kc")
eip <- format_output_M(r1, var_select = "eip")
Delta <- format_output_M(r1, var_select = "Delta")
Rt1 <- format_output_M(r1, var_select = "R0t_1")
FOI1 <- format_output_M(r1, var_select = "FOI1")

# plot
humans_plot <- plot(r1, type = "H")
mosquitoes_plot <- plot(r1, type = "M")

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
  ggtitle("Daily microcephaly cases") +
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
  ggtitle("Weekly microcephaly cases/1000") +
  theme_bw()

extra_H_plot <- Ntotal_plot + inf_1_plot + MC_plot + inf_1_w_plot + MC_w_plot +
  plot_layout(ncol = 2)

Lwt_plot <- ggplot(Lwt, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Larvae wild type") +
  theme_bw()

Kc_plot <- ggplot(Kc, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("Mean across patches") +
  ggtitle("Mosquito larvae carrying capacity") +
  theme_bw()

eip_plot <- ggplot(eip, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("Mean across patches") +
  ggtitle("Extrinsic Incubation Period") +
  theme_bw()

Delta_plot <- ggplot(Delta, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("Mean across patches") +
  ggtitle("Adult mosquito daily mortality rate") +
  theme_bw()

Rt1_plot <- ggplot(Rt1, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("Mean across patches") +
  ggtitle("Time-varying ZIKV reprodution number") +
  theme_bw()

FOI1_plot <- ggplot(FOI1, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("Mean across patches") +
  ggtitle("Force of Infection") +
  theme_bw()

extra_M_plot <- Lwt_plot + Kc_plot + eip_plot + 
  Delta_plot + Rt1_plot + FOI1_plot + plot_layout(ncol = 2)

# save
save_plot(humans_plot, out_dir, "human_compartments", wdt = wdt, hgt = hgt)
save_plot(mosquitoes_plot, out_dir, "mosquitoes_compartments", wdt = wdt, hgt = hgt)
save_plot(extra_H_plot, out_dir, "extra_H_diagnostics", wdt = 18, hgt = 15)
save_plot(extra_M_plot, out_dir, "extra_M_diagnostics", wdt = 18, hgt = 15)


# plot by patch ---------------------------------------------------------------


# format 
Ntotal_p <- format_output_H(r1, var_select = "Ntotal", keep = "patch")
inf_1_p <- format_output_H(r1, var_select = "inf_1", keep = "patch")
MC_p <- format_output_H(r1, var_select = "MC", keep = "patch")
inf_1_w_p <- format_output_H(r1, var_select = "inf_1_w", keep = "patch")
MC_w_p <- format_output_H(r1, var_select = "MC_w", keep = "patch")

Lwt_p <- format_output_M(r1, var_select = "Lwt", keep = "patch")
Kc_p <- format_output_M(r1, var_select = "Kc", keep = "patch")
eip_p <- format_output_M(r1, var_select = "eip", keep = "patch")
Delta_p <- format_output_M(r1, var_select = "Delta", keep = "patch")
Rt1_p <- format_output_M(r1, var_select = "R0t_1", keep = "patch")
FOI1_p <- format_output_M(r1, var_select = "FOI1", keep = "patch")

# plot
humans_p_plot <- plot(r1, type = "H", keep = "patch")
mosquitoes_p_plot <- plot(r1, type = "M", keep = "patch")

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
  ggtitle("Daily microcephaly cases") +
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
  ggtitle("Weekly microcephaly cases/1000") +
  theme_bw()

Lwt_p_plot <- ggplot(Lwt_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Larvae wild type") +
  theme_bw()

Kc_p_plot <- ggplot(Kc_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("Kc") +
  ggtitle("Mosquito larvae carrying capacity") +
  theme_bw()

eip_p_plot <- ggplot(eip_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("EIP") +
  ggtitle("Extrinsic Incubation Period") +
  theme_bw()

Delta_p_plot <- ggplot(Delta_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("Delta") +
  ggtitle("Adult mosquito daily mortality rate") +
  theme_bw()

Rt1_p_plot <- ggplot(Rt1_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("Rt") +
  ggtitle("Time-varying ZIKV reprodution number") +
  theme_bw()

FOI1_p_plot <- ggplot(FOI1_p, aes(x = t, y = y)) +
  geom_line(color = 'royalblue', size = 0.5) +
  facet_wrap(~ patch) +
  scale_x_continuous("Time") +
  scale_y_continuous("FOI") +
  ggtitle("Force of Infection") +
  theme_bw()

# save
save_plot(humans_p_plot, out_dir_2, "human_compartments", wdt = wdt_p, hgt = hgt_p)
save_plot(mosquitoes_p_plot, out_dir_2, "mosquitoes_compartments", wdt = wdt_p, hgt = hgt_p)
save_plot(Ntotal_p_plot, out_dir_2, "Ntotal", wdt = wdt_p, hgt = hgt_p)
save_plot(inf_1_p_plot, out_dir_2, "inf_1", wdt = wdt_p, hgt = hgt_p)
save_plot(MC_p_plot, out_dir_2, "MC", wdt = wdt_p, hgt = hgt_p)
save_plot(inf_1_w_p_plot, out_dir_2, "inf_1_w", wdt = wdt_p, hgt = hgt_p)
save_plot(MC_w_p_plot, out_dir_2, "MC_w", wdt = wdt_p, hgt = hgt_p)
save_plot(Lwt_p_plot, out_dir_2, "Lwt", wdt = wdt_p, hgt = hgt_p)
save_plot(Kc_p_plot, out_dir_2, "Kc", wdt = wdt_p, hgt = hgt_p)
save_plot(eip_p_plot, out_dir_2, "eip", wdt = wdt_p, hgt = hgt_p)
save_plot(Delta_p_plot, out_dir_2, "Delta", wdt = wdt_p, hgt = hgt_p)
save_plot(Rt1_p_plot, out_dir_2, "Rt", wdt = wdt_p, hgt = hgt_p)
save_plot(FOI1_p_plot, out_dir_2, "FOI", wdt = wdt_p, hgt = hgt_p)


# plot by vaccine status ------------------------------------------------------


# format 
Ntotal_v <- format_output_H(r1, var_select = "Ntotal", keep = "vaccine") %>%
  mutate(vaccine = factor(vaccine))
inf_1_v <- format_output_H(r1, var_select = "inf_1", keep = "vaccine") %>%
  mutate(vaccine = factor(vaccine))
MC_v <- format_output_H(r1, var_select = "MC", keep = "vaccine") %>%
  mutate(vaccine = factor(vaccine))
inf_1_w_v <- format_output_H(r1, var_select = "inf_1_w", keep = "vaccine") %>%
  mutate(vaccine = factor(vaccine))
MC_w_v <- format_output_H(r1, var_select = "MC_w", keep = "vaccine") %>%
  mutate(vaccine = factor(vaccine))

# plot
humans_v_plot <- plot(r1, type = "H", keep = "vaccine")

Ntotal_v_plot <- ggplot(Ntotal_v, aes(x = t, y = y, col = vaccine)) +
  geom_line(size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Total number of humans") +
  theme_bw()

inf_1_v_plot <- ggplot(inf_1_v, aes(x = t, y = y, col = vaccine)) +
  geom_line(size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Daily infections") +
  theme_bw()

MC_v_plot <- ggplot(MC_v, aes(x = t, y = y, col = vaccine)) +
  geom_line(size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Daily microcephaly cases") +
  theme_bw()

inf_1_w_v_plot <- ggplot(inf_1_w_v, aes(x = t, y = y, col = vaccine)) +
  geom_line(size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Weekly infections/1000") +
  theme_bw()

MC_w_v_plot <- ggplot(MC_w_v, aes(x = t, y = y, col = vaccine)) +
  geom_line(size = 0.5) +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Weekly microcephaly cases/1000") +
  theme_bw()

extra_H_plot <- Ntotal_v_plot + inf_1_v_plot + MC_v_plot + inf_1_w_v_plot + 
  MC_w_v_plot + plot_layout(ncol = 2, guides = "collect")

# save
save_plot(humans_v_plot, out_dir_3, "human_compartments", wdt = wdt_v, hgt = hgt_v)
save_plot(extra_H_plot, out_dir_3, "extra_H_diagnostics", wdt = 18, hgt = 15)


# plot by age, vaccine status and patch ---------------------------------------


# format 
patch_id <- 1
S_avp <- format_output_H(r1, var_select = "S", keep = "all", patch_id = patch_id)
I_avp <- format_output_H(r1, var_select = "I1", keep = "all", patch_id = patch_id)
R_avp <- format_output_H(r1, var_select = "R1", keep = "all", patch_id = patch_id)
Ntotal_avp <- format_output_H(r1, var_select = "Ntotal", keep = "all", patch_id = patch_id)
inf_1_avp <- format_output_H(r1, var_select = "inf_1", keep = "all", patch_id = patch_id)
MC_avp <- format_output_H(r1, var_select = "MC", keep = "all", patch_id = patch_id)
inf_1_w_avp <- format_output_H(r1, var_select = "inf_1_w", keep = "all", patch_id = patch_id)
MC_w_avp <- format_output_H(r1, var_select = "MC_w", keep = "all", patch_id = patch_id)

# plot
S_avp_plot <- ggplot(S_avp, aes(x = t, y = y, col = age)) +
  geom_line(size = 0.5) +
  facet_wrap(~ vaccine, ncol = 1, scales = "free_y") +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Susceptibles") +
  theme_bw()

I_avp_plot <- ggplot(I_avp, aes(x = t, y = y, col = age)) +
  geom_line(size = 0.5) +
  facet_wrap(~ vaccine, ncol = 1, scales = "free_y") +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Infectious") +
  theme_bw()

avp_plot_1 <- S_avp_plot + I_avp_plot + 
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(theme = theme(plot.margin = unit(c(0,0,0,0), "cm"))) & 
  theme(legend.position = 'bottom') &
  guides(colour = guide_legend(nrow = 1))

R_avp_plot <- ggplot(R_avp, aes(x = t, y = y, col = age)) +
  geom_line(size = 0.5) +
  facet_wrap(~ vaccine, ncol = 1, scales = "free_y") +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Recovered") +
  theme_bw()

Ntotal_avp_plot <- ggplot(Ntotal_avp, aes(x = t, y = y, col = age)) +
  geom_line(size = 0.5) +
  facet_wrap(~ vaccine, ncol = 1, scales = "free_y") +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Total number of humans") +
  theme_bw()

avp_plot_2 <- R_avp_plot + Ntotal_avp_plot + 
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(theme = theme(plot.margin = unit(c(0,0,0,0), "cm"))) & 
  theme(legend.position = 'bottom') &
  guides(colour = guide_legend(nrow = 1))

inf_1_avp_plot <- ggplot(inf_1_avp, aes(x = t, y = y, col = age)) +
  geom_line(size = 0.5) +
  facet_wrap(~ vaccine, ncol = 1, scales = "free_y") +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Daily infections") +
  theme_bw()

MC_avp_plot <- ggplot(MC_avp, aes(x = t, y = y, col = age)) +
  geom_line(size = 0.5) +
  facet_wrap(~ vaccine, ncol = 1, scales = "free_y") +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Daily microcephaly cases") +
  theme_bw()

avp_plot_3 <- inf_1_avp_plot + MC_avp_plot + 
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(theme = theme(plot.margin = unit(c(0,0,0,0), "cm"))) & 
  theme(legend.position = 'bottom') &
  guides(colour = guide_legend(nrow = 1))

inf_1_w_avp_plot <- ggplot(inf_1_w_avp, aes(x = t, y = y, col = age)) +
  geom_line(size = 0.5) +
  facet_wrap(~ vaccine, ncol = 1, scales = "free_y") +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Weekly infections") +
  theme_bw()

MC_w_avp_plot <- ggplot(MC_w_avp, aes(x = t, y = y, col = age)) +
  geom_line(size = 0.5) +
  facet_wrap(~ vaccine, ncol = 1, scales = "free_y") +
  scale_x_continuous("Time") +
  scale_y_continuous("N") +
  ggtitle("Weekly microcephaly cases") +
  theme_bw()

avp_plot_4 <- inf_1_w_avp_plot + MC_w_avp_plot + 
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(theme = theme(plot.margin = unit(c(0,0,0,0), "cm"))) & 
  theme(legend.position = 'bottom') &
  guides(colour = guide_legend(nrow = 1))

# save
save_plot(avp_plot_1, out_dir_4, "avp_plot_1", wdt = wdt_avp, hgt = hgt_avp)
save_plot(avp_plot_2, out_dir_4, "avp_plot_2", wdt = wdt_avp, hgt = hgt_avp)
save_plot(avp_plot_3, out_dir_4, "avp_plot_3", wdt = wdt_avp, hgt = hgt_avp)
save_plot(avp_plot_4, out_dir_4, "avp_plot_4", wdt = wdt_avp, hgt = hgt_avp)
