post_process <- function(i, dat, time, exp_des) {
  
  # browser()
  
  x <- dat[[i]]
    
  cbind(id = i, time = time, x)  
  
}

cbind_id <- function(x, ls) {
  
  cbind(ls[[x]], id = x)

}
