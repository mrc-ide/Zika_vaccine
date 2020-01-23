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
