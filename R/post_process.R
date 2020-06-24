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
  
  cbind(x, id = id)

}

subset_array <- function(my_array, id_1, id_2) {
  
  my_array[id_1:id_2,,,]  
  
}

vacc_strategies_post_processing <- function(x, id, time) {
  
  out_1 <- cbind_id_time(x = x[, "inf_1"], id = id, time = time)
  
  names(out_1)[names(out_1) == "value"] <- "inf_1"
  
  out_2 <- cbind(out_1, MC = x[, "MC"], Ntotal = x[, "Ntotal"])
  
  out_3 <- cumsum(out_2[, "inf_1"])
  
  out_4 <- cumsum(out_2[, "MC"])
  
  out_2$inf_1_IR <- calculate_incidence(out_3, out_2$Ntotal, time_window = 7)
  
  out_2$MC_IR <- calculate_incidence(out_4, out_2$Ntotal, time_window = 7) 
  
  out_2
}