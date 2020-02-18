
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
