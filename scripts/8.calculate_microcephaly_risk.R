

## devtools::install_github("mrc-ide/ZikaModel")

library(ZikaModel)
library(reshape2)
library(ggplot2)
library(viridis)
library(gridExtra)

source(file.path("R", "wrapper_to_save_plot.R"))
source(file.path("R", "utility_functions.R"))
source(file.path("R", "melt_sim_output_array.R"))
source(file.path("R", "plot_diagnostics_by_age.R"))


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

# number of years to run the simulation for
time_years <- 50 # years

# time step
my_dt <- 1

# number of model time steps
time_frame <- (364 * time_years) / my_dt

bp <- 0.0002


# load data -------------------------------------------------------------------


# microcephaly risk during pregnancy - processed from James' curves
mr_pregn_data <- readRDS(file.path("output", "microcephaly_risk_probs.rds"))

br_brazil_age <- readRDS()


# run -------------------------------------------------------------------------


# run the model
out <- run_model(agec = age_init,
                 death = deathrt,
                 nn_links,
                 amplitudes_phases,
                 time = time_frame,
                 season = TRUE)

n_infections <- out$inf_1

time_steps <- out$TIME

tt <- out$TIME

time <- max(tt)


# plot number of new infections -------------------------------------------


brks <- seq(from = 0, to = time, by = 364 * 5)

inf_1_full_melt <- melt_sim_output_array(n_infections, tt)

inf_1_melt <- subset(inf_1_full_melt, vaccine == 1 & patch == 1)

p <- plot_diagnostics_by_age(inf_1_melt, "inf_1")

save_plot(p,
          out_dir,
          out_fl_nm = sprintf("inf_1_vaccine_%s_patch_%s", 1, 1),
          wdt = 10,
          hgt = 8)


# ----------------------------------------------------------------------------
# calculate the number of infected foeti for each simulation time step


array_dim <- dim(n_infections)

probM <- mr_pregn_data$prob

n_time_steps <- length(time_steps)

probM_size <- length(probM)

all_probs <- array(0, array_dim) 

for (i in seq(1, n_time_steps, 1)) {
  
  tmp <- 0
  
  testDay <- i
  
  minDay <- testDay - probM_size + 1
  
  if(minDay < 1) minDay <- 1
  
  #message("testDay = ", testDay)
  #message("minDay = ", minDay)

  for(j in seq(minDay, testDay, by = 1)) {
    
    #message("j = ", j)
    
    index <- j - (testDay - probM_size)
    #message("index = ", index)
    
    if(index == 0) stop("j = ", j)
    
    tmp <- tmp + n_infections[j,,,] * (probM[index] + bp - probM[index]*bp)
  
  }
  
  #message("tmp = ", tmp)
  
  all_probs[i,,,] <- tmp #1 - (1 - tmp) * (1 - bp)
  
}


# plot ------------------------------------------------------------------------


all_probs_full_melt <- melt_sim_output_array(all_probs, tt)

all_probs_melt <- subset(all_probs_full_melt, vaccine == 1 & patch == 1)

p2 <- plot_diagnostics_by_age(all_probs_melt, "infected foeti")

save_plot(p2,
          out_dir,
          out_fl_nm = sprintf("n_infected_foeti_bp_vaccine_%s_patch_%s", 1, 1),
          wdt = 12,
          hgt = 8)


# calculate number of births per age group ------------------------------------


all_probs_2 <- sweep(all_probs, MARGIN = 2, br_brazil_age, `*`)


# plot ------------------------------------------------------------------------
  
  
all_probs_2_full_melt <- melt_sim_output_array(all_probs_2, tt)

all_probs_2_melt <- subset(all_probs_2_full_melt, vaccine == 1 & patch == 8)

p3 <- plot_diagnostics_by_age(all_probs_2_melt, "infected births")

save_plot(p3,
          out_dir,
          out_fl_nm = sprintf("n_infected_births_vaccine_%s_patch_%s", 1, 8),
          wdt = 12,
          hgt = 8)
