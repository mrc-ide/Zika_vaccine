wrapper_to_save_plot <- function(i, p, out_fl_nm, ...) {
  
  out_fl_nm2 <- sprintf("%s_%s", out_fl_nm, i)
  
  save_plot(plot_obj = p[[i]],
            out_fl_nm = out_fl_nm2,
            ...)
  
}
