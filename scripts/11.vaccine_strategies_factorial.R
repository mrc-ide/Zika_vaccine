
## devtools::install_github("mrc-ide/ZikaModel")

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(file.path("R", "utility_functions.R"),
                  file.path("R", "calculate_MC_numbers.R"),
                  file.path("R", "pre_process.R"),
                  file.path("R", "wrapper_multi_factors_ZikaModel.R"))

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
  
  config <- didehpc::didehpc_config(template = "24Core", rtools = TRUE)
  
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  #context::parallel_cluster_start(7, ctx)
  context::context_load(ctx)
  
}


# define parameters -----------------------------------------------------------


experiment_name <- file.path("vaccine_strategies", "factorial_3")

age_init <- c(1, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10)

deathrt <- c(1e-10,
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

my_dt <- 1

mr_baseline <- 0.0002

plot_interval <- 5 # years

vacc_starttime <- 1.7  

diagnostics_to_save <- c("inf_1", "Ntotal", "MC")

fixed_params <- list(vacc_child_starttime = vacc_starttime)

vacc_coverage_values <- c(0, 0.5, 0.8, 1)

target_pop_values <- c(1, 2)

duration_values <- c(0.16, time_years + 1)

eff_values <- c(0.75, 1)

prop_immune_values <- c(0, 0.25, 0.5)


# pre processing --------------------------------------------------------------


out_tab_dir <- file.path("output", experiment_name)

exp_des <- expand.grid(vacc_cov = vacc_coverage_values,
                       target_pop = target_pop_values,
                       duration = duration_values, 
                       efficacy = eff_values,
                       prop_imm = prop_immune_values,
                       stringsAsFactors = FALSE)

exp_des$id <- sprintf("%02d", seq_len(nrow(exp_des)))

integer_time_steps <- (364 * time_years) / my_dt

its <- seq(0, integer_time_steps, 1)

fact_combs_ls <- df_to_list(exp_des, use_names = TRUE)
  

# load data -------------------------------------------------------------------


mr_pregn_data <- readRDS(file.path("output", "microcephaly_risk_probs.rds"))

br_brazil_age <- readRDS(file.path("output", "age_specific_birth_rates.rds"))


# run one job -----------------------------------------------------------------


# t <- obj$enqueue(wrapper_multi_factors_ZikaModel_2(
#   fact_combs_ls[[1]],
#   agec = age_init,
#   death = deathrt,
#   parms = fixed_params,
#   integer_time_steps = its,
#   var_save = diagnostics_to_save,
#   out_dir = out_tab_dir,
#   mr_pregn_risk = mr_pregn_data,
#   birth_rates = br_brazil_age,
#   mr_baseline = mr_baseline))


# run -------------------------------------------------------------------------


if (CLUSTER) {
  
  vaccine_exps <- queuer::qlapply(
    fact_combs_ls,
    wrapper_multi_factors_ZikaModel_2,
    obj,
    agec = age_init,
    death = deathrt,
    parms = fixed_params,
    integer_time_steps = its,
    var_save = diagnostics_to_save,
    out_dir = out_tab_dir,
    mr_pregn_risk = mr_pregn_data,
    birth_rates = br_brazil_age,
    mr_baseline = mr_baseline)
  
} else {
  
  vaccine_exps <- loop(fact_combs_ls,
                       wrapper_multi_factors_ZikaModel_2,
                       agec = age_init,
                       death = deathrt,
                       parms = fixed_params,
                       integer_time_steps = its,
                       var_save = diagnostics_to_save,
                       out_dir = out_tab_dir,
                       mr_pregn_risk = mr_pregn_data,
                       birth_rates = br_brazil_age,
                       mr_baseline = mr_baseline,
                       parallel = FALSE)
  
}


# save output -----------------------------------------------------------------


write_out_csv(exp_des, out_tab_dir, "experimental_design") 
