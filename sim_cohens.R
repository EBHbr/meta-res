library(metafor)
library(boot)
library(parallel)
source("meta-utils.R")
set.seed(2600)

n_cores = detectCores() - 2

studies_n_sim <- 10 # No. studies
samples_n_sim <- 18 # sample sizes
mu_int_sim <- 0.3 ; sd_int_sim <- 1 # N(mu,sd) Intervention
mu_con_sim <- 0 ; sd_con_sim <- 1 # N(mu,sd) Control

# Parameters for data simulation 
metas_n = 100
boots_n = 60
n_boot_resampling = 100 

results_sim <- matrix(ncol = boots_n,nrow = metas_n) # empty matrix
for (i in 1:boots_n){
  results_sim[,i] <- replicate(metas_n,
                              simulate_metas(studies_n = studies_n_sim,samples_n = samples_n_sim,
                                             mean_interv = mu_int_sim, mean_contr = mu_con_sim,
                                             sd_interv = sd_int_sim, sd_contr = sd_con_sim))} 


res_boots <- boot(data=results_sim,
                    statistic=pos_boot,
                    R=n_boot_resampling,
                    parallel="multicore",
                    ncpus=n_cores)

boot.ci(res_boots)  
