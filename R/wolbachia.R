wrapper_multi_factors_ZikaModel <- function(x, time_period) {

  Wb_starttime <- x$Wb_starttime
  Wb_introduration <- x$Wb_introduration
  Wb_introlevel <- x$Wb_introlevel
  id <- x$id


  message("Combination number = ", id)
  message("Wb_starttime = ", Wb_starttime)
  message("Wb_introduration = " , Wb_introduration)
  message("Wb_introlevel = ", Wb_introlevel)
  message("----------------------------------------------------------")
  
  
  r1 <- run_deterministic_model(time_period = time_period,
                                Wb_starttime = Wb_starttime,
                                Wb_introduration = Wb_introduration,
                                Wb_introlevel = Wb_introlevel)
  
  prop_wb <- format_output_Mprop(r1) 
  
  prop_wb

}
