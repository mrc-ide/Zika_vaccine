
reshape_by_patch <- function(var, out) {
  
  NP <- 21
  
  prop_wb_df <- as.data.frame(out[[var]])
  names(prop_wb_df) <- seq_len(NP)
  prop_wb_df$x <- out$TIME
  melt(prop_wb_df,
       id.vars = "x",
       variable.name = "patch",
       value.name = "y")
  
}

melt_sim_output_array <- function(array_to_melt, TIME) {
  
  full_melt <- reshape2::melt(array_to_melt)
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

melt_sim_output_array_3 <- function(array_to_melt, TIME, var_name) {
  
  no_levels <- ncol(array_to_melt)
  
  df <- as.data.frame(array_to_melt)
  
  names(df) <- seq_len(no_levels)
  
  df$time <- TIME
  
  reshape2::melt(df,
                 id.vars = "time",
                 variable.name = var_name)
  
}

aggregate_and_reshape_total <- function(my_array, tt) {
  
  sum_apv <- sum_across_array_dims(my_array, 1)  
  
  cbind_time(sum_apv, tt)
  
}

cumsum_and_incidence_total <- function(my_array, Ntotal) {
  
  sum_apv <- sum_across_array_dims(my_array, 1)
  
  sum_apv_Ntotal <- sum_across_array_dims(Ntotal, 1)
  
  cumsum_sum_apv <- cumsum(sum_apv)
  
  window_inc <- calculate_incidence(cumsum_sum_apv, sum_apv_Ntotal, 7)
  
  cbind_time(window_inc, tt)
  
}

aggregate_and_reshape_patch <- function(my_array, tt) {
  
  sum_av <- sum_across_array_dims(my_array, c(1, 4))
  
  melt_sim_output_array_3(sum_av, tt, "patch")
  
}

cumsum_and_incidence_patch <- function(my_array, Ntotal) {
  
  sum_av <- sum_across_array_dims(my_array, c(1, 4))
  
  sum_av_Ntotal <- sum_across_array_dims(Ntotal, c(1, 4))
  
  cumsum_sum_av <- cumsum_across_array_dims(sum_av, 2)
  
  window_inc <- calculate_incidence(cumsum_sum_av, sum_av_Ntotal, 7)
  
  melt_sim_output_array_3(window_inc, tt, "patch")
  
}


aggregate_and_reshape_vaccine <- function(my_array, tt) {
  
  sum_ap <- sum_across_array_dims(my_array, c(1, 3))
  
  melt_sim_output_array_3(sum_ap, tt, "vaccine")
  
}

cumsum_and_incidence_vaccine <- function(my_array, Ntotal) {
  
  sum_ap <- sum_across_array_dims(my_array, c(1, 3))
  
  sum_ap_Ntotal <- sum_across_array_dims(Ntotal, c(1, 3))
  
  cumsum_sum_ap <- cumsum_across_array_dims(sum_ap, 2)
  
  window_inc <- calculate_incidence(cumsum_sum_ap, sum_ap_Ntotal, 7)
  
  melt_sim_output_array_3(window_inc, tt, "vaccine")
  
}

aggregate_and_reshape_age <- function(my_array, tt) {
  
  melt_array <- melt_sim_output_array(my_array, tt)
  
  subset(melt_array, patch == 1)
  
}

cumsum_and_incidence_age <- function(my_array, Ntotal) {
  
  cumsum_array <- cumsum_across_array_dims(my_array, c(2, 3, 4))
  
  window_inc <- calculate_incidence(cumsum_array, Ntotal, 7)
  
  melt_array <- melt_sim_output_array(window_inc, tt)
  
  subset(melt_array, patch == 1)
  
}

calculate_SIR_proportions <- function(prevalence, Ntotal, tt) {
  
  value <- prevalence / Ntotal
  
  cbind_time(value, tt)
  
}
  
  
# melt_sim_output_array_2 <- function(array_to_melt, TIME) {
#   
#   full_melt <- reshape2::melt(array_to_melt)
#   names(full_melt) <- c("time", "vaccine", "patch", "value")
#   no_vaccine <- length(unique(full_melt$vaccine))
#   no_patch <- length(unique(full_melt$patch))
#   combs <- no_vaccine * no_patch
#   tt_long <- rep(TIME, combs)
#   full_melt$time <- tt_long
#   
#   full_melt$vaccine <- factor(full_melt$vaccine,
#                           levels = unique(full_melt$vaccine),
#                           labels = unique(full_melt$vaccine))
#   
#   full_melt
#   
# }
# 
# melt_by_patch <- function(array_to_melt, TIME) {
#   
#   NP <- 21
#   
#   # sum across ages and vaccine status (dims 2 and 3)
#   sum <- apply(array_to_melt, c(1, 4), sum)
#   
#   sum_df <- as.data.frame(sum)
#   
#   names(sum_df) <- seq_len(NP)
# 
#   sum_df$time <- TIME
#   
#   melt(sum_df,
#        id.vars = "time",
#        variable.name = "patch")
#   
#   
# }
