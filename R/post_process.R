add_diagno_name_var <- function(x, b){
  b[[x]]$diagnostics <- names(b[x])
  b[[x]]
}

give_col_names <- function(x){
  colnames(x) <- as.character(seq_len(11))
  x
}

add_time_var <- function(x){
  x$time <- tt
  x
}

cbind_time <- function(x, time) {
  
  out <- cbind(time = time, value = x)  
  
  as.data.frame(out)
  
}

cbind_id_time <- function(i, dat, time) {
  
  # browser()
  
  value <- dat[[i]]
    
  out <- cbind(id = i, time = time, value)  
  
  as.data.frame(out)
  
}

cbind_id <- function(x, ls) {
  
  cbind(ls[[x]], id = x)

}
