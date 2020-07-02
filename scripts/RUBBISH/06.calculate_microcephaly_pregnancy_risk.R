## Script for Lorenzo to look at microcephaly risk curves

## devtools::install_github("jameshay218/zikaInfer")

library(zikaInfer)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)

source(file.path("R", "utility_functions.R"))

## Reads in all files ending "multivariate_chain" from specified working dir
chains <- load_mcmc_chains(location = "data/hay_et_al_2018", asList = FALSE, unfixed = FALSE)

## Plot 100 random draws from posterior
p <- plot_random_microceph_curves(chains,100)
save_plot(p, "figures", "random_draws_from_JH_posterior", wdt = 14, hgt = 9)


## Gamma function used
dput(microceph_v1)

chain_means <- colMeans(chains)
gamma_mean <- as.numeric(chain_means["mean"])
gamma_var <- as.numeric(chain_means["var"])
gamma_c <- as.numeric(chain_means["c"])

pars <- c(mean = gamma_mean,
          var = gamma_var,
          c = gamma_c)

m_risk_probs <- microceph_v1(pars = pars)

df <- data.frame(day = 0:279, prob = m_risk_probs)

p2 <- ggplot(df) +
  geom_line(aes(x = day, y = prob), col = "blue", lwd = 1) +
  scale_y_continuous("Probability of microcephaly given infection") +
  scale_x_continuous("Week of gestation at infection", 
                   breaks = seq(0, 280, by = 7*10), 
                   labels = seq(0, 280/7, by = 10)) +
  ggtitle("Microcephaly risk curve") +
  theme_bw() 

save_plot(p2, "figures", "microcephaly_pregnancy_risk", wdt = 14, hgt = 9)

write_out_rds(df, "output", "microcephaly_risk_probs")
