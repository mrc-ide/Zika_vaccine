wrapper_multi_factors_ZikaModel_2 <- function(x,
                                              agec, 
                                              death,
                                              vaccine_age,
                                              params,
                                              integer_time_steps,
                                              season = FALSE) {
  
  odin_model_path <- system.file("extdata/odin_model_determ.R", package = "ZikaModel")
  
  id <- x$id
  tot_coverage <- x$tot_cov
  vacc_start <- x$start
  vacc_stop <- x$stop
  
  message("ID = ", id)
  message("total coverage = ", tot_coverage)
  message("start time = " , vacc_start)
  message("stop time = ", vacc_stop)

    
  message("----------------------------------------------------------")
  
  
  factorial_params <- list(vacc_child_coverage = tot_coverage,
                           vacc_child_starttime = vacc_start,
                           vacc_child_stoptime = vacc_stop)
  
  params <- c(params, factorial_params)
  
  create_generator <- ZikaModel::create_r_model(odin_model_path = odin_model_path,
                                                agec = agec,
                                                death = death,
                                                nn_links = nn_links,
                                                amplitudes_phases,
                                                vaccine_age = vaccine_age,
                                                params = params)
  
  gen <- create_generator$generator(user = create_generator$state)
  
  mod_run <- gen$run(integer_time_steps)
  
  out <- gen$transform_variables(mod_run)
  
  out$inf_1
  
}

wrapper_multi_factors_ZikaModel <- function(x,
                                            agec,
                                            death,
                                            my_dt,
                                            time_years,
                                            season) {

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
                                                amplitudes_phases,
                                                DT = my_dt,
                                                season = season,
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
