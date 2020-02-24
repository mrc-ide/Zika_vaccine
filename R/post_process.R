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
  
  # browser()
    
  out <- cbind(id = id, time = time, value = x)  
  
  out_df <- as.data.frame(out)
  
  out_df$id <- factor(out_df$id, 
                      levels = unique(out_df$id),
                      labels = unique(out_df$id))
  
  out_df
}

cbind_id <- function(x, id) {
  
  cbind(x, id = id)

}

subset_array <- function(my_array, id_1, id_2) {
  
  my_array[id_1:id_2,,,]  
  
}

vacc_strategies_post_processing <- function(x, id, time) {
  
  out_1 <- cbind_id_time(x = x, id = id, time = time)
  
  out_2 <- cumsum(out_1[, "inf_1"])
  
  out_3 <- cumsum(out_1[, "MC"])
  
  out_1$inf_1_IR <- calculate_incidence(out_2, out_1$Ntotal, time_window = 7)
  
  out_1$MC_IR <- calculate_incidence(out_3, out_1$Ntotal, time_window = 7) 
  
  out_1
}