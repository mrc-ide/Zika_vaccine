

## devtools::install_github("mrc-ide/ZikaModel")

library(ZikaModel)
library(reshape2)
library(ggplot2)
library(viridis)
library(gridExtra)

source(file.path("R", "wrapper_to_save_plot.R"))
source(file.path("R", "utility_functions.R"))


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

# age specific fertility rates (live births per women) 
br_brazil <- read.csv(file.path("data", "age_specific_birth_rates_Brazil.csv"))

# sex ratio of the total population (males per 100 females)
sex_ratio_brazil <- read.csv(file.path("data", "sex_ratio_tot_pop_Brazil.csv"))


# run -------------------------------------------------------------------------


# run the model
out <- run_model(agec = age_init,
                 death = deathrt,
                 nn_links,
                 amplitudes_phases,
                 time = time_frame,
                 season = TRUE)

infection_probs <- out$inf_1_prob

time_steps <- out$TIME

tt <- out$TIME

time <- max(tt)


# plot probabilty of new infections -------------------------------------------


infection_probs <- out$inf_1_prob
brks <- seq(from = 0, to = time, by = 364 * 5)

inf_1_prob_full_melt <- melt(infection_probs)
names(inf_1_prob_full_melt) <- c("time", "age", "vaccine", "patch", "value")
no_age <- length(unique(inf_1_prob_full_melt$age))
no_vaccine <- length(unique(inf_1_prob_full_melt$vaccine))
no_patch <- length(unique(inf_1_prob_full_melt$patch))
combs <- no_age * no_vaccine * no_patch
tt_long <- rep(tt, combs)
inf_1_prob_full_melt$time <- tt_long
inf_1_prob_full_melt$age <- factor(inf_1_prob_full_melt$age,
                                   levels = unique(inf_1_prob_full_melt$age),
                                   labels = unique(inf_1_prob_full_melt$age))

inf_1_prob_melt <- subset(inf_1_prob_full_melt, vaccine == 1 & patch == 1)

p <- ggplot(inf_1_prob_melt) +
  geom_line(aes(x = time, y = value, colour = age)) +
  scale_fill_viridis() +
  scale_y_continuous(name = "inf_1") +
  scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8))
save_plot(p,
          out_dir,
          out_fl_nm = sprintf("inf_1_prob_vaccine_%s_patch_%s", 1, 1),
          wdt = 10,
          hgt = 8)


# ----------------------------------------------------------------------------
# calculate the risk of microcephaly cases for each simulation time step


array_dim <- dim(infection_probs)

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
    
    tmp <- tmp + infection_probs[j,,,] * probM[index]
  
  }
  
  #message("tmp = ", tmp)
  
  all_probs[i,,,] <- 1 - (1 - tmp) * (1 - bp)
  
}


# plot ------------------------------------------------------------------------


all_probs_full_melt <- melt(all_probs)
names(all_probs_full_melt) <- c("time", "age", "vaccine", "patch", "value")
no_age <- length(unique(all_probs_full_melt$age))
no_vaccine <- length(unique(all_probs_full_melt$vaccine))
no_patch <- length(unique(all_probs_full_melt$patch))
combs <- no_age * no_vaccine * no_patch
tt_long <- rep(tt, combs)
all_probs_full_melt$time <- tt_long
all_probs_full_melt$age <- factor(all_probs_full_melt$age,
                                 levels = unique(all_probs_full_melt$age),
                                 labels = unique(all_probs_full_melt$age))

all_probs_melt <- subset(all_probs_full_melt, vaccine == 1 & patch == 1)

p <- ggplot(all_probs_melt) +
  geom_line(aes(x = time, y = value, colour = age)) +
  scale_fill_viridis() +
  scale_y_continuous(name = "P_m_t") +
  scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8))
save_plot(p,
          out_dir,
          out_fl_nm = sprintf("P_m_t_vaccine_%s_patch_%s", 1, 1),
          wdt = 12,
          hgt = 8)


# # calculate number of births per age group ------------------------------------
# 
# br_brazil <- br_brazil[, 15]
# sr_pc <- sex_ratio_brazil[, 17]
# sr <- sr_pc / 100
# 
# na <- 11
# br_age <- rep(0, na)
