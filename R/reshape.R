mean_across_patches <- function(out, var) {
  
  ret1 <- apply(out[[var]][,1:20], 1, mean)
  
  data.frame(x = out$TIME, y = ret1)
  
}

reshape_by_patch <- function(var, out) {
  
  NP <- 21
  
  prop_wb_df <- as.data.frame(out[[var]])
  names(prop_wb_df) <- seq_len(NP)
  prop_wb_df$x <- out$TIME
  melt(prop_wb_df,
       id.vars = "x",
       variable.name = "patch",
       value.name = "y")
  
}
