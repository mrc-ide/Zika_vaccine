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

wrapper_vaccine_factorial <- function(x,
                                      parms = NULL,
                                      integer_time_steps,
                                      out_dir) {
  
  #browser()
  
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
  
  
  message("--------------------------------------------------------------------/n")
  
  
  if(!is.null(parms)) {
    
    vacc_starttime <- parms$vacc_starttime
    
  } else {
    
    vacc_starttime <- get_vacc_start_time(prop_imm)
    
  }  
  
  vacc_stoptime <- vacc_starttime + vacc_duration
  
  vaccine_child_age <- vaccine_target_code_to_age(target_pop)
  
  time_period <- parms$time_period
  
  r1 <- run_deterministic_model(time_period = 365 * time_period,
                                vacc_child_coverage = vacc_child_cov,
                                vacc_child_starttime = vacc_starttime,
                                vacc_child_stoptime = vacc_stoptime,
                                vaccine_child_age = vaccine_child_age,
                                other_prop_immune = prop_immune)

  
  out <- format_output_H(r1, var_select = c("inf_1_w", "MC_w"))
    
  write_out_rds(out, out_dir, paste0("diagnostics_", id)) 
  
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