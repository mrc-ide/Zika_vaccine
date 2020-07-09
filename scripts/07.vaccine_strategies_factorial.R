
## devtools::install_github("mrc-ide/ZikaModel")

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(file.path("R", "utility_functions.R"),
                  file.path("R", "vaccine.R"))

my_pkgs <- "ZikaModel"

if (CLUSTER) {
  
  # running out of place
  
  workdir <- "Q:/Zika_vaccine"
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
  
  config <- didehpc::didehpc_config(template = "20Core", rtools = TRUE)
  
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::parallel_cluster_start(5, ctx)
  context::context_load(ctx)
  
}


# define parameters -----------------------------------------------------------


experiment_no <- 1

vacc_starttime <- 1.3  

fixed_params <- list(vacc_starttime = vacc_starttime,
                     time_period = 6)

vacc_coverage_values <- c(0, 0.5, 0.8, 1)

target_pop_values <- c(1, 2)

duration_values <- c(0.17, 1.5, 2)

prop_immune_values <- c(0, 0.25, 0.5)


# pre processing --------------------------------------------------------------


experiment_name <- paste0("factorial_", experiment_no)
  
experiment_path <- file.path("vaccine_strategies", experiment_name)

out_tab_dir <- file.path("output", experiment_path)

exp_des <- expand.grid(vacc_cov = vacc_coverage_values,
                       target_pop = target_pop_values,
                       duration = duration_values, 
                       prop_imm = prop_immune_values,
                       stringsAsFactors = FALSE)

exp_des$id <- sprintf("%02d", seq_len(nrow(exp_des)))

fact_combs_ls <- df_to_list(exp_des, use_names = TRUE)
  

# run one job -----------------------------------------------------------------


# t <- obj$enqueue(wrapper_vaccine_factorial(
#   fact_combs_ls[[1]],
#   parms = fixed_params,
#   out_dir = out_tab_dir))


# run -------------------------------------------------------------------------


if (CLUSTER) {
  
  vaccine_exps <- queuer::qlapply(
    fact_combs_ls,
    wrapper_vaccine_factorial,
    obj,
    parms = fixed_params,
    out_dir = out_tab_dir,
    name = experiment_name)
  
} else {
  
  vaccine_exps <- loop(fact_combs_ls,
                       wrapper_vaccine_factorial,
                       parms = fixed_params,
                       out_dir = out_tab_dir,
                       parallel = TRUE)
  
}


# save ------------------------------------------------------------------------


write_out_csv(exp_des, out_tab_dir, paste0("experimental_design_", experiment_no), 
              row.names = FALSE) 
