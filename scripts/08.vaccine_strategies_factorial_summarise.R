
library(purrr)
library(ggplot2)
library(RColorBrewer)
library(dplyr)

source(file.path("R", "post_process.R"))
source(file.path("R", "vaccine.R"))
source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


experiment_no <- 1

copy_files <- FALSE 

duration_values <- c(0.17, 1.5, 2)


# load data -------------------------------------------------------------------


exp_des <- read.csv(file.path("output", 
                              "vaccine_strategies", 
                              "factorial_1", 
                              "experimental_design_1.csv"),
                    colClasses = c("numeric", "numeric", "numeric", "numeric", "character"))


# plot pre processing ---------------------------------------------------------


experiment_name <- paste0("factorial_", experiment_no)

experiment_path <- file.path("vaccine_strategies", "routine", experiment_name)

data_path <- file.path("output", experiment_path)

out_fig_path <- file.path("figures", experiment_path)

if(copy_files) {
  
  root <- file.path("Q:", "Zika_vaccine")
  
  share_path <- file.path(root, data_path)
  
  fl_path_all <- list.files(share_path, pattern = "^diagnostics", full.names = TRUE)
  
  copy_from_share(root, fl_path_all)
    
}

fl_nms <- list.files(data_path, pattern = "^diagnostics", full.names = TRUE)

sim_data <- lapply(fl_nms, readRDS)

out <- imap(sim_data, ~ cbind_id(x = .x, id = .y))

out_df <- do.call("rbind", out)

out_df_2 <- left_join(out_df, exp_des, by = "id")

out_df_2$target_pop <- factor(out_df_2$target_pop, 
                              levels = unique(out_df_2$target_pop),
                              labels = c("9-49", "1-99"))

out_df_2$duration <- factor(out_df_2$duration, 
                            levels = unique(out_df_2$duration),
                            labels = c("2 months", "1.5 years", "2 years"))

out_df_2$vacc_cov <- factor(out_df_2$vacc_cov, 
                            levels = unique(out_df_2$vacc_cov),
                            labels = c("0%", "50%", "80%", "100%"))

measure_values <- levels(out_df_2$compartment)
  
imm_values <- unique(out_df_2$prop_imm)
  
# qual_brew_palette <- "Set1"

seq_brew_palette <- "YlOrRd"

plot_ttls <- c("ZIKV infections", "Microcephaly cases")

brks_values <- list(seq(0,25,5), seq(0,0.003,0.001))

lmts_values <- list(c(0,30), c(0,0.004))

labs_values <- list(seq(0,25,5), seq(0,0.003,0.001))

vacc_starttime <- get_vacc_start_time(imm_values)

vacc_starttime_all <- rep(vacc_starttime, each = 6)

duration_values_all <- rep(rep(duration_values, each = 2), 3)

rects <- unique(out_df_2[, c("target_pop", "duration", "prop_imm")])
rects$xstart <- vacc_starttime_all * 364
rects$xend <- (vacc_starttime_all + duration_values_all) * 364


# plotting --------------------------------------------------------------------


# the first colours of the seq palettes are difficult to see on white bckg
my_palette <- brewer.pal(n = 9, name = seq_brew_palette)[c(3, 5, 7, 9)]

for (i in seq_along(measure_values)) {
  
  mes <- measure_values[i]
  
  for (k in seq_along(imm_values)) {
    
    imm <- imm_values[k]
    
    v_s <- vacc_starttime[k]
    
    rects_2 <- subset(rects, prop_imm == imm)
    
    df <- subset(out_df_2, compartment == mes & prop_imm == imm)
    
    p <- ggplot(df) +
      geom_rect(data = rects_2,
                aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf),
                fill = "lightskyblue1",
                alpha = 0.3) +
      geom_line(aes(x = t, y = y, colour = vacc_cov)) +
      geom_vline(xintercept = v_s * 364, linetype = "dashed") +
      facet_grid(facets = duration ~ target_pop) +
      scale_colour_manual(name = "Vaccine coverage",
                          values = my_palette) +
      scale_y_continuous(name = "Weekly numbers/1000",
                         breaks = brks_values[[i]],
                         limits = lmts_values[[i]],
                         labels = labs_values[[i]],
                         expand = expansion(mult = c(0, 0))) +
      scale_x_continuous(name = "Time") +
      theme_bw() +
      theme(axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            strip.text.x = element_text(size = 8),
            legend.position = "bottom") +
      guides(colour = guide_legend(nrow = 1)) +
      ggtitle(sprintf("%s by vaccine target ages and duration \n(%s%% preexisting immunity)", plot_ttls[i], imm*100))
    
    wdt <- length(imm_values)
    
    # my_index <- (j * wdt + k) - wdt 
    
    # message(my_index)
    
    my_hgt <- 15
    
    if(length(duration_values) == 3) {
      
      my_hgt <- 20
      
    }
    
    save_plot(plot_obj = p,
              out_pth = out_fig_path,
              out_fl_nm = sprintf("summary_factorial_%s_%s", mes, k),
              wdt = 18,
              hgt = my_hgt)
    
  }
  
  
}


# # calculate proportions of infections and MC averted by the vaccine -----------
# 
# 
# sim_data_ls_sums <- lapply(sim_data_ls_1, vapply, sum, numeric(1))
# 
# out_sums <- do.call("rbind", sim_data_ls_sums)
# 
# out_sums_df <- out_sums %>%
#   as.data.frame() %>%
#   mutate(id = as.factor(sprintf("%02d", seq_len(nrow(out_sums))))) %>%
#   left_join(exp_des, by = "id") %>%
#   group_by(target_pop, duration, efficacy, prop_imm) %>%
#   mutate(red_inf_1 = (inf_1[1] - inf_1) / inf_1[1]) %>%
#   mutate(red_MC = (MC[1] - MC) / MC[1]) %>%
#   ungroup() %>%
#   select(- Ntotal)
# 
# 
# # save table ------------------------------------------------------------------
# 
# 
# write_out_csv(out_sums_df, data_path, paste0("results_", experiment_no)) 
# 
# write_out_rds(out_melt, data_path, "summary_factorial_data")
