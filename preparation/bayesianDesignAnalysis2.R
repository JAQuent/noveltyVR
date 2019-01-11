# This script simulates an open-ended Sequential Bayes Factor analysis
# for the project noveltyVR

# Setting seed
set.seed(39257)
 
# Libraries
library(BayesFactor)
library(parallel)

# Functions
cohensD <- function(m1, sd1, m2, sd2){
  # Calculates the effect size cohen's d based on means and standard deviations
  return((m2 - m1)/sqrt((sd2*sd2 + sd1 * sd1)/2))
}

datedFileNam <- function(fileName, fileEnding){
  return(paste(fileName, 
               '_',
               format(Sys.time(), "%Y%m%d_%H%M%S"), 
               fileEnding,
               sep = ''))
}

# One sided hypothesis
# following http://bayesfactor.blogspot.com/2014/02/bayes-factor-t-tests-part-2-two-sample.html
# H0 = 0, H1 > 0
# Open SBF
openSBF_tTest_between_one <- function(params){
  nStart      <- params[1]
  nEnd        <- params[2]
  targetBF    <- params[3]
  effectSize  <- params[4]
  
  # Creating populations
  pop1 = rnorm(nEnd, effectSize, 1)
  pop2 = rnorm(nEnd, 0, 1)
  
  # Sequential analysis
  for(n in nStart:length(pop1)){
    bf <- as.numeric(as.vector(ttestBF(pop1[1:n], pop2[1:n], nullInterval = c(0, Inf))[1]))
    if(bf <= 1/targetBF | bf >= targetBF){
      break
    }
  }
  
  return(c(bf, n))
}

# Two sided hypothesis (previously used)
# H0 = 0, H1 != 0
openSBF_tTest_between_two <- function(params){
  nStart      <- params[1]
  nEnd        <- params[2]
  targetBF    <- params[3]
  effectSize  <- params[4]
  
  # Creating populations
  pop1 = rnorm(nEnd, effectSize, 1)
  pop2 = rnorm(nEnd, 0, 1)
  
  # Sequential analysis
  for(n in nStart:length(pop1)){
    bf <- as.numeric(as.vector(ttestBF(pop1[1:n], pop2[1:n])))
    if(bf <= 1/targetBF | bf >= targetBF){
      break
    }
  }
  
  return(c(bf, n))
}

# Setting parameters
# From Table 3
effectSize  <- cohensD(45, 9, 55, 12)
nStart      <- 10
targetBF    <- 10
nIterations <- 10000

# Open SBF version
# nEnd        <- 500
# paramsH0 <- data.frame(nStart     = rep(nStart, nIterations),
#                        nEnd       = rep(nEnd, nIterations),
#                        targetBF   = rep(targetBF, nIterations),
#                        effectSize = rep(0, nIterations))
# 
# paramsH1 <- data.frame(nStart     = rep(nStart, nIterations),
#                        nEnd       = rep(nEnd, nIterations),
#                        targetBF   = rep(targetBF, nIterations),
#                        effectSize = rep(effectSize, nIterations))

# Open SBF version + max
nEnd        <- seq(26, 40, 1)
paramsH0 <- data.frame(nStart     = rep(nStart, nIterations * length(nEnd)),
                       nEnd       = rep(nEnd, nIterations),
                       targetBF   = rep(targetBF, nIterations *length(nEnd)),
                       effectSize = rep(0, nIterations * length(nEnd)))

paramsH1 <- data.frame(nStart     = rep(nStart, nIterations * length(nEnd)),
                       nEnd       = rep(nEnd, nIterations),
                       targetBF   = rep(targetBF, nIterations * length(nEnd)),
                       effectSize = rep(effectSize, nIterations * length(nEnd)))

paramsH0 <- as.matrix(paramsH0)
paramsH1 <- as.matrix(paramsH1)

# Creating cluster
numCores <- detectCores() - 1
cluster  <- makeCluster(numCores)
clusterExport(cluster, 'ttestBF')

# Running analysis
startTime <- Sys.time()
bfH0      <- parRapply(cluster, paramsH0, openSBF_tTest_between_one)
time1     <- Sys.time()
bfH1      <- parRapply(cluster, paramsH1, openSBF_tTest_between_one)
time2     <- Sys.time()

print(time2)

# Stopping analysis
stopCluster(cluster)

# Saving results
save(startTime, 
     time1,
     time2, 
     paramsH0,
     paramsH1,
     bfH0,
     bfH1,
     file = datedFileNam('noveltyVR_SBF_DesignAnalysis', '.RData'))
