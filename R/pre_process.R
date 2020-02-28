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
  
  if(preexisting_immunity_level == 0) {
    
    out <- 1.7
  }  
  
  if(preexisting_immunity_level == 0.25) {
    
    out <- 2.2
  }
  
  if(preexisting_immunity_level == 0.5) {
    
    out <- 3.5
    
  }
  
  out

}
