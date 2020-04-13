# Test robustness
n <- 10
results <- matrix(NA, 160, n)
setwd("U:/Projects/noveltyVR/analysis")

for(i in 1:n){
  fit_alpha            <- fit.mpt(recogData_alpha_agg[, 4:9], "MPT_RKN.model", n.optim = 50)
  fit_alpha_individual <- fit_alpha$parameters$individual
  fit_alpha_individual <- as.data.frame(t(fit_alpha_individual[,1,]))
  
  fit_living            <- fit.mpt(recogData_living_agg[, 4:9], "MPT_RKN.model", n.optim = 50)
  fit_living_individual <- fit_living$parameters$individual
  fit_living_individual <- as.data.frame(t(fit_living_individual[,1,]))
  participants <- unique(recogData$subNum)
  
  row.names(fit_alpha_individual)  <- NULL
  row.names(fit_living_individual) <- NULL
  
  mainData_wide            <- rbind(fit_alpha_individual, fit_living_individual)
  mainData_wide$subNum     <- rep(recogData_living_agg$subNum, 2)
  mainData_wide$group      <- rep(recogData_living_agg$group, 2)
  mainData_wide$encodTask  <- c(rep('alphabetical', length(participants)), rep('living', length(participants)))
  
  
  results[, i] <- melt(mainData_wide, id.vars = c("subNum", "group", "encodTask"))$value
}


# BF
n <- 1000
results1 <- rep(NA, n)

for(i in 1:n){
  test1.2 <- ttestBF(x = hypo1.2$trans_value[hypo1.2$group == 'Novelty'],
                     y = hypo1.2$trans_value[hypo1.2$group == 'Control'],
                     nullInterval = c(-Inf, 0))
  
  results1[i] <- as.numeric(as.vector(test1.2[2]))
}
