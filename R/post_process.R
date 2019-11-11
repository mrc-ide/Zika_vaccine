post_process <- function(i, dat, time, exp_des) {
  
  # browser()
  
  x <- dat[[i]]
    
  cbind(id = i, time = time, x)  
  
}

# mean_of_the_patches <- function(x){
#   
#   # browser()
#   
#   apply(x[,1:20], 1, mean)
#   
# }
