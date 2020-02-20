
simple_plot <- function(df, y_lab_title, ttl = NULL, y_lim = NULL, break_interval = 5) {
  
  max_time <- length(unique(df$time))
  
  brks <- seq(from = 0, to = max_time, by = 364 * break_interval)
  
  if(is.null(y_lim)) {
    
    my_y_lim <- c(y_lim[1], y_lim[2])  
    
  } else {
    
    my_y_lim <- NULL
    
  }
  
  p <- ggplot(df) +
    geom_line(aes(x = time, y = value), colour = "#63B8FF") +
    scale_y_continuous(name = y_lab_title, limits = my_y_lim, labels = scales::comma) +
    scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          strip.text.x = element_text(size = 8))
  
  if(!is.null(ttl)) {
    
    p <- p + ggtitle(ttl)
    
  }
  
  p
  
}

plot_by_line <- function(df, line_var, y_lab_title, ttl = NULL, leg_pos = NULL, break_interval = 5) {
  
  max_time <- length(unique(df$time))
  
  brks <- seq(from = 0, to = max_time, by = 364 * break_interval)
  
  p <- ggplot(df) +
    geom_line(aes_string(x = "time", y = "value", colour = line_var)) +
    scale_y_continuous(name = y_lab_title, labels = scales::comma) +
    scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          strip.text.x = element_text(size = 8),
          legend.position = "bottom")
  
  if(!is.null(ttl)) {
    
    p <- p + ggtitle(ttl)
    
  }
  
  if(!is.null(leg_pos)){
    
    p <- p + theme(legend.position = leg_pos)
    
  }
    
  p
  
}

plot_by_facet <- function(df, facet_var, y_lab_title, ttl = NULL, break_interval = 5) {
  
  max_time <- length(unique(df$time))
  
  brks <- seq(from = 0, to = max_time, by = 364 * break_interval)
  
  p <- ggplot(df) +
    geom_line(aes(x = time, y = value), colour = "#63B8FF") +
    facet_wrap(as.formula(paste("~", facet_var)), ncol = 4) +
    scale_y_continuous(name = y_lab_title, labels = scales::comma) +
    scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          strip.text.x = element_text(size = 8))
  
  if(!is.null(ttl)) {
    
    p <- p + ggtitle(ttl)
    
  }
  
  p
  
}

plot_by_line_facet <- function(df, line_var, facet_var, y_lab_title, ttl = NULL, y_lim = NULL, leg_pos = NULL, break_interval = 5) {
  
  max_time <- length(unique(df$time))
  
  brks <- seq(from = 0, to = max_time, by = 364 * break_interval)
  
  if(is.null(y_lim)) {
    
    my_y_lim <- c(y_lim[1], y_lim[2])  
    
  } else {
    
    my_y_lim <- NULL
    
  }
  
  p <- ggplot(df) +
    geom_line(aes_string(x = "time", y = "value", colour = line_var)) +
    facet_wrap(facets = as.formula(paste("~", facet_var)), 
               ncol = 1, 
               scales = "free_y") +
    scale_fill_viridis() +
    scale_y_continuous(name = y_lab_title, limits = my_y_lim, labels = scales::comma) +
    scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          strip.text.x = element_text(size = 8),
          legend.position = "bottom") + 
    guides(colour = guide_legend(nrow = 1))
  
  if(!is.null(ttl)) {
    
    p <- p + ggtitle(ttl)
    
  }
  
  if(!is.null(leg_pos)){
    
    p <- p + theme(legend.position = leg_pos)
    
  }
  
  p
  
}

plot_diagnostics_by_p_v <- function(df, v_var, y_lab_title, ttl = NULL, break_interval = 5) {
  
  brks <- seq(from = 0, to = time, by = 364 * break_interval)
  
  p <- ggplot(df) +
    geom_line(aes_string(x = "time", y = "value", colour = v_var)) +
    facet_wrap(~ patch, ncol = 4) +
    scale_y_continuous(name = y_lab_title) +
    scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          strip.text.x = element_text(size = 8),
          legend.position = "bottom")
  
  if(!is.null(ttl)) {
    
    p <- p + ggtitle(ttl)
    
  }
  
  p
  
}

# plot_diagnostics_by_age <- function(df, out_pth, out_fl_nm, diagno_nms){
# 
#   # browser()
# 
#   no_pages <- 1
# 
#   brks <- seq(from = 0, to = time, by = 364*10)
# 
#   xstrips_labs <- as_labeller(setNames(diagno_nms, levels(df$diagnostics)))
# 
#   dir.create(out_pth, FALSE, TRUE)
# 
#   for (i in seq_len(no_pages)){
# 
#     png(file.path(out_pth, sprintf("%s_%s%s", out_fl_nm, i, ".png")),
#         width = 20,
#         height = 12,
#         units = "cm",
#         pointsize = 12,
#         res = 300)
# 
#     ret <- ggplot(df, aes(x = .data$time, y = .data$value, colour = .data$age_group)) +
#       geom_line(size = 0.4) +
#       ggforce::facet_wrap_paginate(~ .data$diagnostics,
#                                    ncol = 1,
#                                    nrow = 2,
#                                    scales = "free_y",
#                                    labeller = xstrips_labs,
#                                    page = i) +
#       scale_x_continuous(name = "Years", breaks = brks, labels = brks / 364) +
#       scale_y_continuous(name = "") +
#       theme_bw() +
#       theme(axis.text.x = element_text(size = 8),
#             axis.text.y = element_text(size = 8),
#             strip.text.x = element_text(size = 8),
#             plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
#             plot.title = element_text(margin = margin(0,0,0.5,0,"cm"))) +
#       ggtitle("SEIR Zika model - diagnostics")
# 
#     print(ret)
# 
#     dev.off()
# 
#   }
# 
# }
