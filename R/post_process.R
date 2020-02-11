cbind_id_time <- function(i, dat, time) {
  
  # browser()
  
  value <- dat[[i]]
    
  cbind(id = i, time = time, value)  
  
}

cbind_id <- function(x, ls) {
  
  cbind(ls[[x]], id = x)

}
