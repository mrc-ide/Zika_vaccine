melt_sim_output_array <- function(array_to_melt, TIME) {
  
  full_melt <- melt(array_to_melt)
  names(full_melt) <- c("time", "age", "vaccine", "patch", "value")
  no_age <- length(unique(full_melt$age))
  no_vaccine <- length(unique(full_melt$vaccine))
  no_patch <- length(unique(full_melt$patch))
  combs <- no_age * no_vaccine * no_patch
  tt_long <- rep(TIME, combs)
  full_melt$time <- tt_long
  full_melt$age <- factor(full_melt$age,
                                levels = unique(full_melt$age),
                                labels = unique(full_melt$age))
  
  full_melt
  
}