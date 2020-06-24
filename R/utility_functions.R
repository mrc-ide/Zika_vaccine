loop <- function(..., parallel) {
  if (parallel) {
    parallel::parLapply(NULL, ...)
  } else {
    lapply(...)
  }
}

loop_simplify <- function(..., what) {
  vapply(loop(...), identity, what)
}

write_out_rds <- function(dat, my_path, file_name) {
  
  dir.create(my_path, FALSE, TRUE)
  
  saveRDS(dat, file.path(my_path, file_name))
  
}

write_out_csv <- function(dat, my_path, file_name, ...) {
  
  dir.create(my_path, FALSE, TRUE)
  
  write.csv(dat, 
            file.path(my_path, file_name),
            ...)
  
}

df_to_list <- function (x, use_names) {
  keep <- c("names", "class", "row.names")
  at <- attributes(x)
  attributes(x) <- at[intersect(names(at), keep)]
  ret <- unname(lapply(split(x, seq_len(nrow(x))), as.list))
  if (!use_names) {
    ret <- lapply(ret, unname)
  }
  if (is.character(at$row.names)) {
    names(ret) <- at$row.names
  }
  ret
}

save_plot <- function(plot_obj, out_pth, out_fl_nm, wdt, hgt){
  
  dir.create(out_pth, FALSE, TRUE)
  png(file.path(out_pth, paste0(out_fl_nm, ".png")),
      width = wdt,
      height = hgt,
      units = "cm",
      pointsize = 12,
      res = 300)
  
  if(is(plot_obj, "gtable")) {
    grid::grid.draw(plot_obj)
  } else {
    print(plot_obj)
  }
  
  on.exit(dev.off())
  
}

write_out_rds <- function(dat, my_path, file_name, ...) {
  
  dir.create(my_path, FALSE, TRUE)
  
  file_name_2 <- paste0(file_name, ".rds")
  
  saveRDS(dat, file.path(my_path, file_name_2), ...)
  
}

write_out_csv <- function(dat, my_path, file_name, ...) {
  
  dir.create(my_path, FALSE, TRUE)
  
  file_name_2 <- paste0(file_name, ".csv")
  
  write.csv(
    dat,
    file.path(my_path, file_name_2),
    ...)
  
}

copy_from_share <- function(root, fl_path_all) {
  
  for (i in seq_along(fl_path_all)){
    
    path_i <- fl_path_all[[i]]
    
    message(path_i)
    
    path_from_i <- path_i
    
    # match root
    path_from_i_no_root <- sub(paste0(root,"/"), "", path_from_i)
    
    # match everything before the last occurrence of /
    path_to_i <- sub("/([^/]*)$", "", path_from_i_no_root)
    
    if(!dir.exists(path_to_i)) dir.create(path_to_i, FALSE, TRUE)
    
    file.copy(path_from_i, path_to_i, overwrite = FALSE)
    
    last_three_digits <- sub("^([^.]*).", "", path_i)
    
    if(last_three_digits == "shp"){
      
      evr_but_last_three_digits <- sub(".([^.]*)$", "", path_i)
      
      path_from_i_1 <- file.path(root, paste0(evr_but_last_three_digits, ".dbf"))
      file.copy(path_from_i_1, path_to_i, overwrite = FALSE)
      
      path_from_i_2 <- file.path(root, paste0(evr_but_last_three_digits, ".prj"))
      file.copy(path_from_i_2, path_to_i, overwrite = FALSE)
      
      path_from_i_3 <- file.path(root, paste0(evr_but_last_three_digits, ".shx"))
      file.copy(path_from_i_3, path_to_i, overwrite = FALSE)
      
    }
    
  }
  
}
