
# functions for manipulating layout of plots 

strip_gglegend <- function(my_plot) {
  
  p <- my_plot + theme(legend.position = "none")
  
  p
  
}
  
  g_legend <- function(a.gplot){
  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  
  legend <- tmp$grobs[[leg]]
  
  return(legend)
  
}

arrangeGrob_multi_files <- function(plots_list, n_plots_per_file, legend = FALSE) {
  
  # arrange n plots per page in a Z x 2 table format (Z = 2 or 1)
  # return a list of grob obects. 
  
  if(legend) {
    
    mylegend <- g_legend(plots_list[[1]])
    
    plots_list <- lapply(plots_list, strip_gglegend)
    
  }

  n <- length(plots_list)
  
  n_files <- ceiling(n / n_plots_per_file)
  
  out <- vector("list", length = n_files)
  
  if(n_plots_per_file == 2) {
    
    n_row <- 1 
    
  }
  
  if(n_plots_per_file == 4) {
    
    n_row <- 2
    
  }
  
  start <- 1
  end <- n_plots_per_file
  
  for (i in seq_len(n_files)) {
    
    ret <- arrangeGrob(grobs = plots_list[start:end], nrow = n_row, ncol = 2)
    
    if(legend) {
      
      ret <- arrangeGrob(ret, mylegend, 
                         nrow = 2, 
                         widths = 17, 
                         heights = c(11, 1))
      
    }
    
    out[[i]] <- ret 
    start <- start + n_plots_per_file
    end <- end + n_plots_per_file
    
    if(end > n) end <- n
    
  }
  
  out
  
}