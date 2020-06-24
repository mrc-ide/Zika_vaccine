
# devtools::install_github("mrc-ide/ZikaModel")

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


out_dir <- file.path("output", "wolbachia_factorial_exp")

time_period <- 365 * 50

Wb_starttime_values <- c(1, 5, 10, 20, 30) # years
Wb_introduration_days_values <- c(15, 30, 60, 90) # days
Wb_introlevel_values <- seq(0, 0.4, 0.1) # proportion of the initial number of wt mosquitoes 


# generate all combinations of factors ----------------------------------------


fact_combs <- expand.grid(Wb_starttime = Wb_starttime_values,
                          Wb_introduration = Wb_introduration_days_values,
                          Wb_introlevel = Wb_introlevel_values)
  
fact_combs$id <- seq_len(nrow(fact_combs))
  
write_out_csv(fact_combs, 
              out_dir,
              "wolbachia_parameters_experiment",
              row.names = FALSE)

fact_combs_ls <- df_to_list(fact_combs, TRUE)


# run one job -----------------------------------------------------------------


# t <- obj$enqueue(wrapper_multi_factors_ZikaModel(
#   fact_combs_ls[[1]]),
#   time_period = time_period)


# run -------------------------------------------------------------------------


if (CLUSTER) {

  ZikaModel_wolb_experiment <- queuer::qlapply(
    fact_combs_ls,
    wrapper_multi_factors_ZikaModel,
    obj,
    time_period = time_period)

} else {

  ZikaModel_wolb_experiment <- loop(
    fact_combs_ls,
    wrapper_multi_factors_ZikaModel,
    time_period = time_period,
    parallel = FALSE)
  
  write_out_rds(ZikaModel_wolb_experiment, out_dir, "wolbachia_experiment")
  
}
