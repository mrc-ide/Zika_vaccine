
gen <- odin::odin("inst/extdata/min_reproducible_example.R", verbose = FALSE)

pars <- list(na = 3,
             NP = 5,
             init_S = array(1:30, c(3, 2, 5)),
             age_S_prob = c(0,1,0.5),
             incub = 2,
             inf_per = 6)

mod <- gen(user = pars)

t <- seq_len(100)

y <- mod$run(t)

out <- mod$transform_variables(y)
