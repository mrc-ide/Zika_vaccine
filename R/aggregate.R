
sum_across_array_dims <- function(array_to_sum, dim_to_keep) {
  
  apply(array_to_sum, dim_to_keep, sum)
  
}

cumsum_across_array_dims <- function(array_to_sum, dim_to_keep) {
  
  apply(array_to_sum, dim_to_keep, cumsum)
  
}

apply_mean_across_patches <- function(x){
  
  # browser()
  
  apply(x, 1, mean_across_patches)
  
}

mean_across_patches <- function(x) {
  
  mean(x[1:20])
  
}

# mean_across_patches <- function(out, var) {
#   
#   ret1 <- apply(out[[var]][,1:20], 1, mean)
#   
#   data.frame(x = out$TIME, y = ret1)
#   
# }

summarize_in_window <- function(melted_array, from_t, to_t) {
  
  NP <- 21
  
  from_t_days <- 364 * from_t
  to_t_days <- 364 * to_t
  
  test <- split(melted_array, melted_array$patch)
  test_2 <- lapply(test, function(x) round(sum(x[x$time %in% c(from_t_days:to_t_days),"value"]),0))
  test_2a <- lapply(test, function(x) max(x$value))
  
  data.frame(patch = as.factor(seq_len(NP)), 
             n = do.call("rbind", test_2),
             max_y = do.call("rbind", test_2a))
  
}

lag_diff_array <- my_fun <- function(my_array, lag_time) {
  
  dims <- dim(my_array)
  
  dim_1 <- dims[3]
  dim_2 <- dims[4]
  
  out <- array(0, dim = dims)
  
  for (i in seq_len(dim_1)) {
    
    for (j in seq_len(dim_2)) {
      
      # browser()
      
      out[,,i,j] <- lag_diff(my_array[,,i,j], lag_time)
      
    }
    
  }
  
  out
  
}

calculate_incidence <- function(new_infections, N, time_window) {
  
  if(time_window == 1) {
    
    lag_diff <- new_infections
    
  } else {
    
    cum_sum <- cumsum_across_array_dims(new_infections, c(2, 3, 4))
    
    lag_diff <- lag_diff_array(cum_sum, time_window)
    
  }
  
  lag_diff / N * 1000
  
}
