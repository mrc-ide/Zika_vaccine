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

cbind_id_time <- function(x, id, time) {
  
  # x is a vetor 
  
  data.frame(id = sprintf("%02d", id), 
             time = time, 
             value = x)  
  
}

cbind_id <- function(x, id) {
  
  x$id <- sprintf("%02d", id)
  
  x

}

subset_array <- function(my_array, id_1, id_2) {
  
  my_array[id_1:id_2,,,]  
  
}
