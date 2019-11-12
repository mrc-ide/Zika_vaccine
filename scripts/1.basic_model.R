
devtools::install_github("mrc-ide/ZikaModel")

library(ZikaModel)
library(reshape2)
library(ggplot2)
library(viridis)

source(file.path("R", "wrapper_to_save_plot.R"))
source(file.path("R", "save_plot.R"))


# define parameters -----------------------------------------------------------


out_dir <- file.path("figures", "deterministic_1")

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

my_dt <- 1

odin_model_path <- system.file("extdata/odin_model_determ.R", package = "ZikaModel")


# run -------------------------------------------------------------------------


create_generator <- create_r_model(odin_model_path = odin_model_path,
                                   agec = agec,
                                   death = death,
                                   nn_links = nn_links,
                                   amplitudes_phases = amplitudes_phases,
                                   DT = my_dt,
                                   season = TRUE)

gen <- create_generator$generator(user = create_generator$state)

integer_time_steps <- (364 * time_years) / my_dt

its <- seq(0, integer_time_steps, 1)

mod_run <- gen$run(its)


# post processing -------------------------------------------------------------


out <- gen$transform_variables(mod_run)

out_2 <- post_processing(out, my_dt)

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


# -----------------------------------------------------------------------------
#
# Plot diagnostics by patch
#
# -----------------------------------------------------------------------------


tt <- out$TIME
time <- max(tt)

out_dir_1 <- file.path(out_dir, "patch")
brks <- seq(from = 0, to = time, by = 364 * 5)

births_patch_df <- as.data.frame(out$births)
names(births_patch_df) <- seq_len(21)
births_patch_df$time <- tt
births_patch_df_melt <- melt(births_patch_df,
                             id.vars = "time",
                             variable.name = "patch")

p <- ggplot(births_patch_df_melt) +
  geom_line(aes(x = time, y = value), colour = "#63B8FF") +
  ggplot2::facet_wrap(~ patch, ncol = 4) +
  scale_y_continuous(name = "births") +
  scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8))
save_plot(p,
          out_dir_1,
          out_fl_nm = "births_by_patch.png",
          wdt = 15,
          hgt = 15)

# sum across ages and vaccine status (dims 2 and 3)
inf_1_patch <- apply(out$inf_1, c(1, 4), sum)
inf_1_patch_df <- as.data.frame(inf_1_patch)
names(inf_1_patch_df) <- seq_len(21)
inf_1_patch_df$time <- tt
inf_1_patch_df_melt <- melt(inf_1_patch_df,
                            id.vars = "time",
                            variable.name = "patch")

p <- ggplot(inf_1_patch_df_melt) +
  geom_line(aes(x = time, y = value), colour = "#63B8FF") +
  ggplot2::facet_wrap(~ patch, ncol = 4) +
  scale_y_continuous(name = "inf_1") +
  scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8))
save_plot(p,
          out_dir_1,
          out_fl_nm = "inf_1_by_patch.png",
          wdt = 15,
          hgt = 15)

S_patch <- apply(out$S, c(1, 4), sum)
S_patch_df <- as.data.frame(S_patch)
names(S_patch_df) <- seq_len(21)
S_patch_df$time <- tt
S_patch_df_melt <- melt(S_patch_df,
                        id.vars = "time",
                        variable.name = "patch")

p <- ggplot(S_patch_df_melt) +
  geom_line(aes(x = time, y = value), colour = "#63B8FF") +
  ggplot2::facet_wrap(~ patch, ncol = 4) +
  scale_y_continuous(name = "S") +
  scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8))
save_plot(p,
          out_dir_1,
          out_fl_nm = "S_by_patch.png",
          wdt = 15,
          hgt = 15)

I1_patch <- apply(out$I1, c(1, 4), sum)
I1_patch_df <- as.data.frame(I1_patch)
names(I1_patch_df) <- seq_len(21)
I1_patch_df$time <- tt
I1_patch_df_melt <- melt(I1_patch_df,
                         id.vars = "time",
                         variable.name = "patch")

p <- ggplot(I1_patch_df_melt) +
  geom_line(aes(x = time, y = value), colour = "#63B8FF") +
  ggplot2::facet_wrap(~ patch, ncol = 4) +
  scale_y_continuous(name = "I1") +
  scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8))
save_plot(p,
          out_dir_1,
          out_fl_nm = "I1_by_patch.png",
          wdt = 15,
          hgt = 15)

R1_patch <- apply(out$R1, c(1, 4), sum)
R1_patch_df <- as.data.frame(R1_patch)
names(R1_patch_df) <- seq_len(21)
R1_patch_df$time <- tt
R1_patch_df_melt <- melt(R1_patch_df,
                         id.vars = "time",
                         variable.name = "patch")

p <- ggplot(R1_patch_df_melt) +
  geom_line(aes(x = time, y = value), colour = "#63B8FF") +
  ggplot2::facet_wrap(~ patch, ncol = 4) +
  scale_y_continuous(name = "R1") +
  scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8))
save_plot(p,
          out_dir_1,
          out_fl_nm = "R1_by_patch.png",
          wdt = 15,
          hgt = 15)

FOI1p_patch_df <- as.data.frame(out$FOI1p)
names(FOI1p_patch_df) <- seq_len(21)
FOI1p_patch_df$time <- tt
FOI1p_patch_df_melt <- melt(FOI1p_patch_df,
                            id.vars = "time",
                            variable.name = "patch")

p <- ggplot(FOI1p_patch_df_melt) +
  geom_line(aes(x = time, y = value), colour = "#63B8FF") +
  ggplot2::facet_wrap(~ patch, ncol = 4) +
  scale_y_continuous(name = "FOI1p") +
  scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8))
save_plot(p,
          out_dir_1,
          out_fl_nm = "FOI1p_by_patch.png",
          wdt = 15,
          hgt = 15)


# -----------------------------------------------------------------------------
#
# Plot diagnostics by age group
#
# -----------------------------------------------------------------------------


out_dir_2 <- file.path(out_dir, "age")

deathrt_age_df <- as.data.frame(out$deathrt)
names(deathrt_age_df) <- seq_len(11)
deathrt_age_df$time <- tt
deathrt_age_df_melt <- melt(deathrt_age_df,
                            id.vars = "time",
                            variable.name = "age")

p <- ggplot(deathrt_age_df_melt) +
  geom_line(aes(x = time, y = value), colour = "#63B8FF") +
  ggplot2::facet_wrap(~ age, ncol = 4#,
                      # scales = "free_y"
  ) +
  scale_y_continuous(name = "deathrt") +
  scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8)) +
  ggplot2::coord_cartesian(ylim=c(0,.004))
save_plot(p,
          out_dir_2,
          out_fl_nm = "deathrt_by_age.png",
          wdt = 15,
          hgt = 15)

agert_age_df <- as.data.frame(out$agert)
names(agert_age_df) <- seq_len(11)
agert_age_df$time <- tt
agert_age_df_melt <- melt(agert_age_df,
                          id.vars = "time",
                          variable.name = "age")

p <- ggplot(agert_age_df_melt) +
  geom_line(aes(x = time, y = value), colour = "#63B8FF") +
  ggplot2::facet_wrap(~ age, ncol = 4, scales = "free_y") +
  scale_y_continuous(name = "agert") +
  scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8))
save_plot(p,
          out_dir_2,
          out_fl_nm = "agert_by_age.png",
          wdt = 15,
          hgt = 15)



# -----------------------------------------------------------------------------
#
# Plot diagnostics by patch, age group and vaccine status
#
# -----------------------------------------------------------------------------



O_S_prob_full_melt <- melt(out$O_S_prob)
names(O_S_prob_full_melt) <- c("time", "age", "vaccine", "patch", "value")
no_age <- length(unique(O_S_prob_full_melt$age))
no_vaccine <- length(unique(O_S_prob_full_melt$vaccine))
no_patch <- length(unique(O_S_prob_full_melt$patch))
combs <- no_age * no_vaccine * no_patch
tt_long <- rep(tt, combs)
O_S_prob_full_melt$time <- tt_long
O_S_prob_full_melt$age <- factor(O_S_prob_full_melt$age,
                                 levels = unique(O_S_prob_full_melt$age),
                                 labels = unique(O_S_prob_full_melt$age))

O_S_prob_melt <- subset(O_S_prob_full_melt, vaccine == 1 & patch == 1)

p <- ggplot(O_S_prob_melt) +
  geom_line(aes(x = time, y = value, colour = age)) +
  scale_fill_viridis() +
  scale_y_continuous(name = "O_S_prob") +
  scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8))
save_plot(p,
          out_dir,
          out_fl_nm = sprintf("O_S_prob_vaccine_%s_patch_%s.png", 1, 1),
          wdt = 10,
          hgt = 8)


S_full_melt <- melt(out$S)
names(S_full_melt) <- c("time", "age", "vaccine", "patch", "value")
S_full_melt$time <- tt_long
S_full_melt$age <- factor(S_full_melt$age,
                          levels = unique(S_full_melt$age),
                          labels = unique(S_full_melt$age))

S_melt <- subset(S_full_melt, vaccine == 1 & patch == 1)

p <- ggplot(S_melt) +
  geom_line(aes(x = time, y = value, colour = age)) +
  scale_fill_viridis() +
  scale_y_continuous(name = "S", breaks = seq(0,6e+6,1e+6), labels = seq(0,6e+6,1e+6), limits = c(0,6e+6)) +
  scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8))
save_plot(p,
          out_dir,
          out_fl_nm = sprintf("S_vaccine_%s_patch_%s.png", 1, 1),
          wdt = 10,
          hgt = 8)

I1_full_melt <- melt(out$I1)
names(I1_full_melt) <- c("time", "age", "vaccine", "patch", "value")
I1_full_melt$time <- tt_long
I1_full_melt$age <- factor(I1_full_melt$age,
                           levels = unique(I1_full_melt$age),
                           labels = unique(I1_full_melt$age))

I1_melt <- subset(I1_full_melt, vaccine == 1 & patch == 1)

p <- ggplot(I1_melt) +
  geom_line(aes(x = time, y = value, colour = age)) +
  scale_fill_viridis() +
  scale_y_continuous(name = "I1", breaks = seq(0,1.4e+6,2e+5), labels = seq(0,1.4e+6,2e+5), limits = c(0,1.4e+6)) +
  scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8))
save_plot(p,
          out_dir,
          out_fl_nm = sprintf("I1_vaccine_%s_patch_%s.png", 1, 1),
          wdt = 10,
          hgt = 8)

O_S_full_melt <- melt(out$O_S)
names(O_S_full_melt) <- c("time", "age", "vaccine", "patch", "value")
O_S_full_melt$time <- tt_long
O_S_full_melt$age <- factor(O_S_full_melt$age,
                            levels = unique(O_S_full_melt$age),
                            labels = unique(O_S_full_melt$age))

O_S_melt <- subset(O_S_full_melt, vaccine == 1 & patch == 1)

p <- ggplot(O_S_melt) +
  geom_line(aes(x = time, y = value, colour = age)) +
  scale_fill_viridis() +
  scale_y_continuous(name = "O_S") +
  scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8))
save_plot(p,
          out_dir,
          out_fl_nm = sprintf("O_S_vaccine_%s_patch_%s.png", 1, 1),
          wdt = 10,
          hgt = 8)

inf_1_full_melt <- melt(out$inf_1)
names(inf_1_full_melt) <- c("time", "age", "vaccine", "patch", "value")
inf_1_full_melt$time <- tt_long
inf_1_full_melt$age <- factor(inf_1_full_melt$age,
                              levels = unique(inf_1_full_melt$age),
                              labels = unique(inf_1_full_melt$age))

inf_1_melt <- subset(inf_1_full_melt, vaccine == 1 & patch == 3)

p <- ggplot(inf_1_melt) +
  geom_line(aes(x = time, y = value, colour = age)) +
  scale_fill_viridis() +
  scale_y_continuous(name = "inf_1", breaks = seq(0,7.5e+4,1.5e+4), labels = seq(0,7.5e+4,1.5e+4), limits = c(0,7.5e+4)) +
  scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8))
save_plot(p,
          out_dir,
          out_fl_nm = sprintf("inf_1_vaccine_%s_patch_%s.png", 1, 3),
          wdt = 10,
          hgt = 8)
