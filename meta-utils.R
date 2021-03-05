pos_boot <- function(results_metas,indices){
  results_metas_extract <- results_metas[indices,]
  pos_rate <- mean(results_metas_extract) # proportion of positive results
  return(pos_rate)
}

simulate_metas <- function(studies_n,samples_n,
                           mean_interv,sd_interv,
                           mean_contr,sd_contr){
  interv <- replicate(studies_n,rnorm(samples_n,mean_interv,sd_interv))
  contr <- replicate(studies_n,rnorm(samples_n,mean_contr,sd_contr))
  
  # Mean and var 
  interv_m <- apply(interv,2,mean) ; interv_sd <- apply(interv,2,sd)
  contr_m <- apply(contr,2,mean) ; contr_sd <- apply(contr,2,sd)
  
  met_dat <- cbind(interv_m,interv_sd,contr_m,contr_sd)
  
  # Meta analysis
  meta_res <- escalc(measure = "SMD",
                     m1i = interv_m,m2i = contr_m, 
                     sd1i = interv_sd, sd2i = contr_sd,
                     n1i = rep(samples_n,studies_n),
                     n2i = rep(samples_n,studies_n),
                     slab = paste("fulano et al",1:length(interv_m)),
                     data = met_dat)
  
  meta_rma <- rma(yi,vi,data=meta_res)
  print(paste("lb:",meta_rma$ci.lb,"ub:",meta_rma$ci.ub))
  # Either lb and ub are both pos or both negative 
  meta_result <- as.numeric( (meta_rma$ci.lb > 0 & meta_rma$ci.ub > 0) | 
                              (meta_rma$ci.lb < 0 & meta_rma$ci.ub < 0) )
  return(meta_result)}

