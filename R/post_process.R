add_diagno_name_var <- function(x, b){
  b[[x]]$diagnostics <- names(b[x])
  b[[x]]
}

add_time_var <- function(x){
  x$time <- tt
  x
}

give_col_names <- function(x){
  colnames(x) <- as.character(seq_len(11))
  x
}

cbind_id_time <- function(i, dat, time) {
  
  # browser()
  
  value <- dat[[i]]
    
  cbind(id = i, time = time, value)  
  
}

cbind_id <- function(x, ls) {
  
  cbind(ls[[x]], id = x)

}
