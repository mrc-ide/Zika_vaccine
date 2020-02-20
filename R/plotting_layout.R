
g_legend <- function(a.gplot){
  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  
  legend <- tmp$grobs[[leg]]
  
  return(legend)
  
}

arrangeGrob_multi_files <- function(plots_list) {
  
  # arrange 4 plots per page in a 2x2 table format 
  # return a list of grob obects
  
  n_plots_per_file <- 4
    
  n <- length(plots_list)
  
  n_files <- ceiling(n / n_plots_per_file)
  
  out <- vector("list", length = n_files)
  
  start <- 1
  end <- n_plots_per_file
  
  for (i in seq_len(n_files)) {
    
    out[[i]] <- arrangeGrob(grobs = plots_list[start:end], nrow = 2, ncol = 2)
    
    start <- start + n_plots_per_file
    end <- end + n_plots_per_file
    
    if(end > n) end <- n
    
  }
  
  out
  
}