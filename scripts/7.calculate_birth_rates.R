
# calculate age specific birth rates

source(file.path("R", "utility_functions.R"))

# age specific fertility rates per 5yr windows (live births per 1000 women) 
br_brazil_all_years <- read.csv(file.path("data", "age_specific_birth_rates_Brazil.csv"))

br_brazil <- br_brazil_all_years[, "X2010...2015"] / 1000 / 5

br_brazil_2 <- c(0, br_brazil)

br_brazil_av <- c()

my_seq <- seq(1, length(br_brazil_2), by = 2)

for (i in seq_along(my_seq)) {
  
  index <- my_seq[i]
  
  out <- (br_brazil_2[index] + br_brazil_2[index + 1]) / 2
  
  br_brazil_av[i] <- out 
  
}

na <- 11

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

write_out_rds(br_brazil_age, "output", "age_specific_birth_rates")
