# Libraries
library(BayesFactor)
library(parallel)

datedFileNam <- function(fileName, fileEnding){
  return(paste(fileName, 
               '_',
               format(Sys.time(), "%Y%m%d_%H%M%S"), 
               fileEnding,
               sep = ''))
}

# Settings
nIterations = 10000
sampleSizes = seq(10, 70, by = 1)

# Functions
get_bf <- function(sampleSize) {
  # Under H0
  pop1 = rnorm(sampleSize, 0, 1)
  pop2 = rnorm(sampleSize, 0, 1)
  bf0 = 1/as.numeric(as.vector(ttestBF(pop1, pop2)))
  
  # Under H1
  pop1 = rnorm(sampleSize, 0.942809, 1)
  pop2 = rnorm(sampleSize, 0, 1)
  bf1 = as.numeric(as.vector(ttestBF(pop1, pop2)))
  
  return(c(bf1, bf0))
}

# Creating cluster
numCores <- detectCores() - 1
cluster  <- makeCluster(numCores)
clusterExport(cluster, 'ttestBF')
params <- rep(sampleSizes, nIterations)
bfs    <- parSapply(cluster, params, get_bf)

stopCluster(cluster)

save(bfs, params, nIterations,
     file = datedFileNam('noveltyVR_bayesianDesignAnalysis', '.RData'))

# Analysis of simulation
library(dplyr)
simulationResults <- data.frame(BF0 = bfs[2, ],
                                BF1 = bfs[1, ],
                                sampleSize = params)

# Analysing simulation data
targetBF                  <- 10
upperLimit                <- 26

# Find the simulations which reached the upper limit without having a BF 
# over 10 or less than 0.1
# For H0
simulationResults$limitStopH0 <- 0
simulationResults[which(simulationResults$sampleSize == upperLimit & simulationResults$BF0 >= 1/targetBF), 'limitStopH0'] <- 1
simulationResults[which(simulationResults$sampleSize == upperLimit & simulationResults$BF0 <= targetBF), 'limitStopH0']   <- 1

# For H1
simulationResults$limitStopH1 <- 0
simulationResults[which(simulationResults$sampleSize >= upperLimit & simulationResults$BF1 <= 1/targetBF), 'limitStopH1'] <- 1
simulationResults[which(simulationResults$sampleSize >= upperLimit & simulationResults$BF1 >= targetBF), 'limitStopH1']   <- 1

# Find the simulation where the data collection is stopped due to evidence
# For H0
simulationResults$evidenceStopH0 <- 0
simulationResults[which(simulationResults$sampleSize < upperLimit & simulationResults$BF0 <= 1/targetBF), 'evidenceStopH0'] <- 1
simulationResults[which(simulationResults$sampleSize < upperLimit & simulationResults$BF0 >= targetBF), 'evidenceStopH0']   <- 1
# For H1
simulationResults$evidenceStopH1 <- 0
simulationResults[which(simulationResults$sampleSize < upperLimit & simulationResults$BF1 <= 1/targetBF), 'evidenceStopH1'] <- 1
simulationResults[which(simulationResults$sampleSize < upperLimit & simulationResults$BF1 >= targetBF), 'evidenceStopH1']   <- 1

# Total number of studies 
# For H0
mean(simulationResults$limitStopH0)
mean(simulationResults$evidenceStopH0)

# For H1
mean(simulationResults$limitStopH1)
mean(simulationResults$evidenceStopH1)


# 1. How many studies stop because evidence or reaching the upper limit?
#For H0
simulationResults_leq25 <- subset(simulationResults, simulationResults$sampleSize <= 25) 
simulationResults_leq25$evidenceStopH0 <- 0
simulationResults_leq25[which(simulationResults_leq25$BF0 <= 1/targetBF), 'evidenceStopH0'] <- 1
simulationResults_leq25[which(simulationResults_leq25$BF0 >= targetBF), 'evidenceStopH0']   <- 1
mean(simulationResults_leq25$evidenceStopH0)
1 - mean(simulationResults_leq25$evidenceStopH0)
#For H1
simulationResults_leq25$evidenceStopH1 <- 0
simulationResults_leq25[which(simulationResults_leq25$BF1 <= 1/targetBF), 'evidenceStopH1'] <- 1
simulationResults_leq25[which(simulationResults_leq25$BF1 >= targetBF), 'evidenceStopH1']   <- 1
mean(simulationResults_leq25$evidenceStopH1)
1 - mean(simulationResults_leq25$evidenceStopH1)

# 2. What is the probability of misleading evidence?



