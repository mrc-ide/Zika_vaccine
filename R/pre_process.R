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
