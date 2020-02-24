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
