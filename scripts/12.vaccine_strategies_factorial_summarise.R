
library(purrr)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(ZikaModel)
library(dplyr)

source(file.path("R", "aggregate.R"))
source(file.path("R", "post_process.R"))
source(file.path("R", "plot_diagnostics.R"))
source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


experiment_name <- file.path("vaccine_strategies", "factorial_2")

vacc_starttime <- 1.7  

tt <- seq.int(1, 5*364, 1)
  
ages_labels <- c("0_1", "2_10", "11_20", "21_30", "31_40", "41_50", "51_60", "61_70", "71_80", "81_90", "91_100") 

data_path <- file.path("output", experiment_name)
  
out_fig_path <- file.path("figures", experiment_name)


# load data -------------------------------------------------------------------


exp_des <- read.csv(file.path(data_path, "experimental_design.csv"),
                    colClasses = c("numeric", "numeric", "numeric", "numeric", "factor"))


# plot pre processing ---------------------------------------------------------


fl_nms <- list.files(data_path, pattern = "^diagnostics", full.names = TRUE)

sim_data <- lapply(fl_nms, readRDS)

sim_data_ls_1 <- lapply(sim_data, lapply, subset_array, 1, 364*5)

sim_data_ls_2 <- lapply(sim_data_ls_1, lapply, sum_across_array_dims, 1)

sim_data_ls_3 <- map(sim_data_ls_2, ~ t(do.call("rbind", .x)))

out <- imap(sim_data_ls_3, 
            ~ vacc_strategies_post_processing(x = .x, id = .y, time = tt))

out_df <- do.call("rbind", out)

out_df_2 <- out_df[, setdiff(names(out_df), c("inf_1", "Ntotal", "MC"))]

out_df_3 <- left_join(out_df_2, exp_des, by = "id")

out_melt <- melt(out_df_3,
                 id.vars = c("id", "time", "vacc_cov", "target_pop", "duration", "efficacy"),
                 variable.name = "measure")

out_melt$target_pop <- factor(out_melt$target_pop, 
                              levels = unique(out_melt$target_pop),
                              labels = c("9-49", "1-99"))

out_melt$duration <- factor(out_melt$duration, 
                            levels = unique(out_melt$duration),
                            labels = c("2 months", ">3.5 years"))

out_melt$vacc_cov <- factor(out_melt$vacc_cov, 
                            levels = unique(out_melt$vacc_cov),
                            labels = c("0%", "50%", "80%", "100%"))

measure_values <- levels(out_melt$measure)

efficacy_values <- unique(out_melt$efficacy)
  
# qual_brew_palette <- "Set1"

seq_brew_palette <- "YlOrRd"

plot_ttls <- c("ZIKV infections", "Microcephaly cases")

brks_values <- list(seq(0,25,5), seq(0,0.003,0.001))

lmts_values <- list(c(0,30), c(0,0.004))

labs_values <- list(seq(0,25,5), seq(0,0.003,0.001))

duration_values <- c(0.16, 3.3)

rects <- unique(out_melt[, c("target_pop", "duration")])
rects$xstart <- vacc_starttime * 364
rects$xend <- rep((vacc_starttime + duration_values) * 364, each = 2)


# plotting --------------------------------------------------------------------


# the first colours of the seq palettes are difficult to see on white bckg
my_palette <- brewer.pal(n = 9, name = seq_brew_palette)[c(3, 5, 7, 9)]

break_interval <- 0.5
max_time <- length(tt)
brks <- seq(from = 0, to = max_time, by = 364 * break_interval)

for (i in seq_along(measure_values)) {
  
  mes <- measure_values[i]
  
  for (j in seq_along(efficacy_values)) {
    
    eff <- efficacy_values[j]
    
    df <- subset(out_melt, measure == mes & efficacy == eff) 
    
    p <- ggplot(df) +
      geom_rect(data = rects,
                aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), 
                fill = "lightskyblue1",
                alpha = 0.3) +
      geom_line(aes_string(x = "time", y = "value", colour = "vacc_cov")) +
      geom_vline(xintercept = vacc_starttime * 364, linetype = "dashed") +
      facet_grid(facets = target_pop ~ duration) +    
      scale_colour_manual(name = "Vaccine coverage",
                          values = my_palette) +
      # scale_color_brewer(name = "Vaccine coverage",
      #                    palette = qual_brew_palette) +
      scale_y_continuous(name = "Weekly numbers/1000", 
                         breaks = brks_values[[i]],
                         limits = lmts_values[[i]],
                         labels = labs_values[[i]],
                         expand = expand_scale(mult = c(0, 0))) +
      scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
      coord_cartesian(xlim = c(0, max_time)) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            strip.text.x = element_text(size = 8),
            legend.position = "bottom") + 
      guides(colour = guide_legend(nrow = 1)) + 
      ggtitle(sprintf("%s by vaccine target ages and duration (%s%% efficacy)", plot_ttls[i], eff*100))
    
    save_plot(plot_obj = p,
              out_pth = out_fig_path, 
              out_fl_nm = sprintf("summary_factorial_%s_%s", mes, j), 
              wdt = 17, 
              hgt = 15)
    
  }
  
}


# calculate infections and MC averted by the vaccine --------------------------


sim_data_ls_sums <- lapply(sim_data_ls_1, vapply, sum, numeric(1))

out_sums <- do.call("rbind", sim_data_ls_sums)

out_sums_df <- out_sums %>%
  as.data.frame() %>%
  mutate(id = as.factor(sprintf("%02d", seq_len(nrow(out_sums))))) %>%
  left_join(exp_des, by = "id") %>%
  mutate(red_inf_1 = (inf_1[1] - inf_1) / inf_1) %>%
  mutate(red_MC = (MC[1] - MC) / MC) %>%
  select(- Ntotal)


# save table ------------------------------------------------------------------


write_out_csv(out_sums_df, data_path, "results") 

write_out_rds(out_melt, data_path, "summary_factorial_data")
