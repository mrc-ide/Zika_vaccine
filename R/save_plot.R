
save_plot <- function(plot_obj, out_pth, out_fl_nm, wdt, hgt){

  dir.create(out_pth, FALSE, TRUE)
  png(file.path(out_pth, out_fl_nm),
      width = wdt,
      height = hgt,
      units = "cm",
      pointsize = 12,
      res = 300)
  print(plot_obj)
  on.exit(dev.off())

}
