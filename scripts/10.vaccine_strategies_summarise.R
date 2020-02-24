
library(purrr)
library(ggplot2)
library(reshape2)
library(ZikaModel)

source(file.path("R", "aggregate.R"))
source(file.path("R", "post_process.R"))
source(file.path("R", "plot_diagnostics.R"))
source(file.path("R", "utility_functions.R"))


# -----------------------------------------------------------------------------


tt <- seq.int(1, 5*364, 1)
  
ages_labels <- c("0_1", "2_10", "11_20", "21_30", "31_40", "41_50", "51_60", "61_70", "71_80", "81_90", "91_100") 

data_path <- file.path("output", "vaccine_strategies")
  
out_fig_dir <- file.path("figures", "vaccine_strategies")


# create summary table by age -------------------------------------------------


data_path_labels <- paste0(file.path(data_path, "experiment_"), 1:4)

fl_nms <- list.files(data_path_labels, full.names = TRUE)

sim_data <- lapply(fl_nms, readRDS)

sim_data_ls_1 <- lapply(sim_data, lapply, subset_array, 1, 364*5)

sim_data_ls_2 <- lapply(sim_data_ls_1, lapply, sum_across_array_dims, 1)

sim_data_ls_3 <- map(sim_data_ls_2, ~ t(do.call("rbind", .x)))

out <- imap(sim_data_ls_3, 
            ~ vacc_strategies_post_processing(x = .x, id = .y, time = tt))

out_df <- do.call("rbind", out)

out_df_2 <- out_df[, setdiff(names(out_df), c("inf_1", "Ntotal", "MC"))]

out_melt <- melt(out_df_2,
                 id.vars = c("id", "time"),
                 variable.name = "measure")


# plotting --------------------------------------------------------------------


p <- plot_by_line_facet(out_melt, 
                        line_var = "id", 
                        facet_var = "measure", 
                        y_lab_title = "Weekly numbers/1000", 
                        break_interval = 0.5)

measure_labs <- c(inf_1_IR = "Infections", MC_IR = "Microcephaly cases")

p2 <- p + 
  facet_wrap(~ measure, 
             ncol = 1, 
             scales = "free_y",
             labeller = labeller(measure = measure_labs))  +
  scale_colour_viridis_d(name = "Vaccine coverage",
                         option = "C",
                         labels = c("0", "50%", "80%", "100%"))


# saving ----------------------------------------------------------------------


save_plot(plot_obj = p2,
          out_pth = out_fig_dir, 
          out_fl_nm = paste("summary_coverage"), 
          wdt = 17, 
          hgt = 15)

# summary_vacc_ages <- data.frame(age_groups = ages_labels,
#                                 infections = sum_apv_infections,
#                                 microcephaly = sum_apv_MC,
#                                 population = sum_apv_Ntotal, 
#                                 stringsAsFactors = FALSE)
# 
# write_out_csv(summary_vacc_ages, out_tab_dir, paste0("burden_summary_age_", my_id))
