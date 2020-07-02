
# calculate age specific birth rates

library(ggplot2)

source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


na <- 11

YL <- 364

DT <- 1


# -----------------------------------------------------------------------------


# age specific fertility rates per 5yr windows (live births per 1000 women) 
br_brazil_all_years <- read.csv(file.path("data", "age_specific_birth_rates_Brazil.csv"))

br_brazil <- br_brazil_all_years[, "X2010...2015"] / 1000

br_brazil_2 <- c(0, br_brazil)

br_brazil_av <- c()

my_seq <- seq(1, length(br_brazil_2), by = 2)

for (i in seq_along(my_seq)) {
  
  index <- my_seq[i]
  
  out <- (br_brazil_2[index] + br_brazil_2[index + 1]) / 2
  
  br_brazil_av[i] <- out 
  
}

br_brazil_age <- rep(0, na)

br_brazil_age[1] <- 0
br_brazil_age[2] <- 0
br_brazil_age[3] <- br_brazil_av[1]
br_brazil_age[4] <- br_brazil_av[2]
br_brazil_age[5] <- br_brazil_av[3]
br_brazil_age[6] <- br_brazil_av[4]
br_brazil_age[7] <- 0
br_brazil_age[8] <- 0
br_brazil_age[9] <- 0
br_brazil_age[10] <- 0
br_brazil_age[11] <- 0

br_brazil_age_2 <- br_brazil_age / YL * DT
  
df_to_plot <- data.frame(age = seq_len(na), 
                         br = br_brazil_age_2)

p <- ggplot() +
  geom_bar(data = df_to_plot, aes(age, br), stat = "identity") +
  scale_x_continuous("age groups", 
                     breaks = seq_len(na), 
                     labels = c("1", "2-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-99")) +
  scale_y_continuous("# births per woman (per day)") +
  ggtitle("Brazil age-specific birth rates") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p, "figures", "age_specific_birth_rates", wdt = 12, hgt = 8)
  
write_out_rds(br_brazil_age_2, "output", "age_specific_birth_rates")
