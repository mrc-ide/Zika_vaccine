
devtools::install_github("mrc-ide/ZikaModel")

options(didehpc.cluster = "fi--dideclusthn")

CLUSTER <- FALSE

my_resources <- c(file.path("R", "wrapper_multi_factors_ZikaModel.R"),
                  file.path("R", "utility_functions.R"))

my_pkgs <- "ZikaModel"

if (CLUSTER) {
  
  # running out of place
  
  workdir <- "Q:/CHIK_vaccine"
  didehpc::didehpc_config_global(workdir = workdir)
  root <- file.path(workdir, "context")

} else {
  
  root <- "context"  
  
}  
  
context::context_log_start()
ctx <- context::context_save(path = root,
                             packages = my_pkgs,
                             sources = my_resources,
                             package_sources = provisionr::package_sources(github = "mrc-ide/ZikaModel"))


# are you using the cluster? --------------------------------------------------


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(rtools = TRUE)
  
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  #context::parallel_cluster_start(7, ctx)
  context::context_load(ctx)
  
}


# define parameters -----------------------------------------------------------


season <- FALSE

agec <- c(1, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10)

death <- c(1e-10,
           1e-10,
           1e-10,
           0.00277068683332695,
           0.0210680857689784,
           0.026724997685722,
           0.0525354529367476,
           0.0668013582441452,
           0.119271483740379,
           0.279105747097929,
           0.390197266957464)

time_years <- 50 # years

my_dt <- 0.5

Wb_starttime_values <- c(1, 5, 10, 20, 30) # years
Wb_introduration_days_values <- c(15, 30, 60, 90) # days
Wb_introlevel_values <- seq(0, 0.4, 0.1) # proportion of the initial number of wt mosquitoes 

Wb_introduration_values <- Wb_introduration_days_values / my_dt

out_dir <- file.path("output", "wolbachia_factorial_exp")


# generate all combinations of factors ----------------------------------------


fact_combs <- expand.grid(Wb_starttime = Wb_starttime_values,
                          Wb_introduration = Wb_introduration_values,
                          Wb_introlevel = Wb_introlevel_values)
  
fact_combs$id <- seq_len(nrow(fact_combs))
  
write_out_csv(fact_combs, 
              out_dir,
              "wolbachia_parameters_experiment.csv",
              row.names = FALSE)

fact_combs_ls <- df_to_list(fact_combs, TRUE)


# run one job -----------------------------------------------------------------


# t <- obj$enqueue(wrapper_multi_factors_ZikaModel(
#   fact_combs_ls[[1]],
#   agec = agec,
#   death = death,
#   my_dt = my_dt,
#   time_years = time_years,
#   season = season))


# run -------------------------------------------------------------------------


if (CLUSTER) {

  ZikaModel_wolb_experiment <- queuer::qlapply(
    fact_combs_ls,
    wrapper_multi_factors_ZikaModel,
    obj,
    agec = agec,
    death = death,
    my_dt = my_dt,
    time_years = time_years,
    season = season)

} else {

  ZikaModel_wolb_experiment <- loop(
    fact_combs_ls,
    wrapper_multi_factors_ZikaModel,
    agec = agec,
    death = death,
    my_dt = my_dt,
    time_years = time_years,
    season = season,
    parallel = FALSE)
  
  write_out_rds(ZikaModel_wolb_experiment, out_dir, "wolbachia_experiment.rds")
  
}
