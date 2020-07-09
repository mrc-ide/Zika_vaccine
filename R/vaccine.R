vaccine_target_code_to_age <- function(code) {
  
  if(code == 1) {
    
    # from 9 to 49
    out <- c(0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0) 
    
  }
  
  if(code == 2) {
    
    # everyone
    out <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)  
    
  }
  
  out
  
}

get_vacc_start_time <- function(preexisting_immunity_level) {
  
  core <- function(preexisting_immunity_level) {
    
    if(preexisting_immunity_level == 0) {
      
      out <- 1.3
      
    }  
    
    if(preexisting_immunity_level == 0.25) {
      
      out <- 1.75
      
    }
    
    if(preexisting_immunity_level == 0.5) {
      
      out <- 2.6
      
    }
    
    out
    
  }
  
  if(length(preexisting_immunity_level) > 1) {
    
    ret <- vapply(preexisting_immunity_level, core, numeric(1)) 
    
  } else {
    
    ret <- core(preexisting_immunity_level) 
    
  }
  
  ret
  
}

wrapper_multi_factors_ZikaModel_2 <- function(x,
                                              agec, 
                                              death,
                                              parms = NULL,
                                              integer_time_steps,
                                              var_save = NULL,
                                              out_dir) {
  
  browser()
  
  id <- x$id
  vacc_child_cov <- x$vacc_cov
  target_pop <- x$target_pop
  vacc_duration <- x$duration
  prop_imm <- x$prop_imm
  
  message("ID = ", id)
  message("coverage = ", vacc_child_cov)
  message("target population = " , target_pop)
  message("duration = ", vacc_duration)
  message("existing immunity = ", prop_imm)  
  
  
  message("--------------------------------------------------------------------")
  
  
  if(!is.null(parms)) {
    
    factorial_params <- list(vacc_child_coverage = vacc_child_cov,
                             vacc_child_stoptime = vacc_starttime,
                             vacceff_prim = vacc_eff,
                             other_prop_immune = prop_imm)
    
    params <- c(parms, factorial_params)
    
  } else {
    
    vacc_starttime <- get_vacc_start_time(prop_imm)
    
    vacc_stoptime <- vacc_starttime + vacc_duration
    
    params <- list(vacc_child_starttime = vacc_starttime,
                   vacc_child_coverage = vacc_child_cov,
                   vacc_child_stoptime = vacc_stoptime,
                   vacceff_prim = vacc_eff,
                   other_prop_immune = prop_imm)
    
  }
  
  vaccine_ages <- vaccine_target_code_to_age(target_pop)
  
  create_generator <- ZikaModel::create_r_model(odin_model_path = odin_model_path,
                                                agec = agec,
                                                death = death,
                                                nn_links = nn_links,
                                                amplitudes_phases,
                                                #vaccine_age = vaccine_ages,
                                                params = params)
  
  gen <- create_generator$generator(user = create_generator$state)
  
  mod_run <- gen$run(integer_time_steps)
  
  out <- gen$transform_variables(mod_run)
  
  infections <- out$inf_1
  
  Ntotal <- out$Ntotal
  
  MC <- calculate_microcases(N_inf = infections, 
                             pregnancy_risk_curve = mr_pregn_risk, 
                             birth_rates = birth_rates, 
                             N_tot = Ntotal, 
                             baseline_probM = mr_baseline)
  
  out$MC <- MC
  
  if(!is.null(var_save)) out <- out[var_save]
  
  write_out_rds(out, out_dir, paste0("diagnostics_", id)) 
  
}

