wrapper_multi_factors_ZikaModel <- function(x,
                                            agec, 
                                            death,
                                            my_dt,
                                            time_years) {
  
  odin_model_path <- system.file("extdata/odin_model_determ.R", package = "ZikaModel")
  
  Wb_starttime <- x$Wb_starttime
  Wb_introduration <- x$Wb_introduration
  Wb_introlevel <- x$Wb_introlevel
  id <- x$id
  
  
  message("Combination number = ", id)
  message("Wb_starttime = ", Wb_starttime)
  message("Wb_introduration = " , Wb_introduration)
  message("Wb_introlevel = ", Wb_introlevel)
  message("----------------------------------------------------------")
  
  
  create_generator <- ZikaModel::create_r_model(odin_model_path = odin_model_path,
                                                agec = agec,
                                                death = death,
                                                nn_links = nn_links,
                                                DT = my_dt,
                                                season = TRUE,
                                                Wb_starttime = Wb_starttime,
                                                Wb_introduration = Wb_introduration,
                                                Wb_introlevel = Wb_introlevel)
  
  gen <- create_generator$generator(user = create_generator$state)
  
  integer_time_steps <- (364 * time_years) / my_dt
  
  its <- seq(0, integer_time_steps, 1)
  
  mod_run <- gen$run(its)
  
  out <- gen$transform_variables(mod_run)
  
  out$prop_wb
  
}
