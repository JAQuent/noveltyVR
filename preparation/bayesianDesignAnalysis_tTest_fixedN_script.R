# This script simulates an fixed N Bayes Factor analysis
# for the project noveltyVR
# T-test version

# Setting seed
set.seed(39257)
 
# Libraries
library(BayesFactor) # Check version
library(parallel)
library(plyr)

# Functions
datedFileNam <- function(fileName, fileEnding){
  return(paste(fileName, 
               '_',
               format(Sys.time(), '%Y%m%d_%H%M%S'), 
               fileEnding,
               sep = ''))
}

# Create global variables for reuse
# Sample size, groups and levels
groups      <- 2
groupSize   <- 36
nTotal      <- groupSize  * groups
levels1     <- 2
levels2     <- 2
totalLevels <- levels1 * levels2
nObvs       <- nTotal * totalLevels

# Design matrix
cons <- matrix(c( 1,  1, -1, -1,
                  1, -1,  1, -1,
                  1, -1, -1,  1),
               nrow = 3,
               byrow = TRUE)
X    <- cbind(kronecker(t(cons), rep(1, groupSize)), 
              kronecker(rep(1, totalLevels), diag(groupSize)))
X    <- bdiag(X, X)
X    <- cbind(1, rep(c(1, -1), each = groupSize * totalLevels), X)

# Variables
N <- c(rep(1, groupSize * totalLevels), rep(-1, groupSize * totalLevels))
E <- c(rep(c(1, 1, -1, -1), each = groupSize), rep(c(1, 1, -1, -1), each = groupSize))
M <- c(rep(c(1, -1, 1, -1), each = groupSize), rep(c(1, -1, 1, -1), each = groupSize))

# Prepared data frame
df <- data.frame(N = as.factor(N),
                 E = as.factor(E),
                 M = as.factor(M),
                 id = as.factor(c(rep(1:groupSize, totalLevels), rep((groupSize + 1):nTotal, totalLevels))))
levels(df$N) <- c('Group 1', 'Group 2')
levels(df$E) <- c('Level 1', 'Level 2')
levels(df$M) <- c('Level 1', 'Level 2')

ANOVA_between_one_within_two <- function(params){
  # Input parsing
  beta0       <- params[1]
  beta1       <- params[2]
  beta2       <- params[3]
  beta3       <- params[4]
  beta4       <- params[5]
  beta5       <- params[6]
  beta6       <- params[7]
  beta7       <- params[8]
  
  # Preparing coefficient vector b
  e_subj <- rnorm(groupSize, mean = 0, sd = 1)
  b      <- c(beta2 + beta4, 
              beta3 + beta5, 
              beta6 + beta7, 
              e_subj)
  b      <- c(beta0, 
              beta1, 
              b, 
              c(beta2 - beta4, 
                beta3 - beta5, 
                beta6 - beta7, 
                e_subj))
  
  # Generating data
  y <- X %*% b + rnorm(dim(X)[1], mean = 0, sd = 1)
  return(y)
}

# Fixed design
fixed_tTest_between_one_within_two <- function(params){
  
  # Generating data and adding to data.frame
  df$y  <- as.numeric(ANOVA_between_one_within_two(as.numeric(params[2:9])))
  # What's the evidence for a main effect of N?
  mainEffect_N <- ddply(df, c('id', 'N'), summarise, y = mean(y))
  test1 <- ttestBF(x = mainEffect_N$y[mainEffect_N$N == 'Group 2'],
                   y = mainEffect_N$y[mainEffect_N$N == 'Group 1'],
                   paired = FALSE,
                   nullInterval = c(-Inf, 0))
  
  # What's the evidence for an interaction between N and E?
  inter_N_E <- ddply(df, c('id', 'N', 'E'), summarise, y = mean(y))
  diff1     <- inter_N_E[inter_N_E$N == 'Group 1' & inter_N_E$E == 'Level 2', 'y'] - inter_N_E[inter_N_E$N == 'Group 1' & inter_N_E$E == 'Level 1', 'y']
  diff2     <- inter_N_E[inter_N_E$N == 'Group 2' & inter_N_E$E == 'Level 2', 'y'] - inter_N_E[inter_N_E$N == 'Group 2' & inter_N_E$E == 'Level 1', 'y']
  test2     <- ttestBF(x = diff2,
                       y = diff1,
                       nullInterval = c(-Inf, 0))
  
  # What's the evidence for an interaction between N and M?
  inter_N_M <- ddply(df, c('id', 'N', 'M'), summarise, y = mean(y))
  diff1     <- inter_N_M[inter_N_M$N == 'Group 1' & inter_N_M$M == 'Level 2', 'y'] - inter_N_M[inter_N_M$N == 'Group 1' & inter_N_M$M == 'Level 1', 'y']
  diff2     <- inter_N_M[inter_N_M$N == 'Group 2' & inter_N_M$M == 'Level 2', 'y'] - inter_N_M[inter_N_M$N == 'Group 2' & inter_N_M$M == 'Level 1', 'y']
  test3     <- ttestBF(x = diff2,
                       y = diff1,
                       nullInterval = c(-Inf, 0))
  
  # What's the evidence for an interaction between N and E and M?
  # Group 1
  # (E1M1 – E2M1)
  diff1 <- df[df$N == 'Group 1' & df$E == 'Level 1' & df$M == 'Level 1', 'y'] - df[df$N == 'Group 1' & df$E == 'Level 2' & df$M == 'Level 1', 'y']
  
  # (E1M2 – E2M2)
  diff2 <- df[df$N == 'Group 1' & df$E == 'Level 1' & df$M == 'Level 2', 'y'] - df[df$N == 'Group 1' & df$E == 'Level 2' & df$M == 'Level 2', 'y']
  
  # Difference of differences (E1M1 – E2M1) – (E1M2 – E2M2)
  diff3     <- diff1 - diff2
  
  # Group 2
  # (E1M1 – E2M1)
  diff4 <- df[df$N == 'Group 2' & df$E == 'Level 1' & df$M == 'Level 1', 'y'] - df[df$N == 'Group 2' & df$E == 'Level 2' & df$M == 'Level 1', 'y']
  
  # (E1M2 – E2M2)
  diff5 <- df[df$N == 'Group 2' & df$E == 'Level 1' & df$M == 'Level 2', 'y'] - df[df$N == 'Group 2' & df$E == 'Level 2' & df$M == 'Level 2', 'y']
  
  # Difference of differences (E1M1 – E2M1) – (E1M2 – E2M2)
  diff7     <- diff4 - diff5
  
  mean(diff7)
  mean(diff3)
  
  test4     <- ttestBF(x = diff7,
                       y = diff3,
                       nullInterval = c(-Inf, 0))
  
    bfs <- c(as.numeric(as.vector(test1[2]/test1[1])), # What's the evidence for a main effect of N?
             as.numeric(as.vector(test2[2]/test2[1])), # What's the evidence for an interaction between N and E?
             as.numeric(as.vector(test3[2]/test3[1])), # What's the evidence for an interaction between N and M?
             as.numeric(as.vector(test4[2]/test4[1]))) # What's the evidence for an interaction between N and E and M?
  
  return(bfs)
}

# Setting parameters
targetBF    <- 6
nIterations <- 10000
effectSize  <- 0.2

# SBF version + max
paramsH0 <- data.frame(targetBF   = rep(targetBF, nIterations *length(nTotal)),
                       beta0      = rep(0, nIterations * length(nTotal)),
                       beta1      = rep(0, nIterations * length(nTotal)),
                       beta2      = rep(0, nIterations * length(nTotal)),
                       beta3      = rep(0, nIterations * length(nTotal)),
                       beta4      = rep(0, nIterations * length(nTotal)),
                       beta5      = rep(0, nIterations * length(nTotal)),
                       beta6      = rep(0, nIterations * length(nTotal)),
                       beta7      = rep(0, nIterations * length(nTotal))) 

paramsH1 <- data.frame(targetBF   = rep(targetBF, nIterations *length(nTotal)),
                       beta0      = rep(0, nIterations * length(nTotal)),
                       beta1      = rep(effectSize, nIterations * length(nTotal)),
                       beta2      = rep(0, nIterations * length(nTotal)),
                       beta3      = rep(0, nIterations * length(nTotal)),
                       beta4      = rep(0, nIterations * length(nTotal)),
                       beta5      = rep(0, nIterations * length(nTotal)),
                       beta6      = rep(0, nIterations * length(nTotal)),
                       beta7      = rep(0, nIterations * length(nTotal))) 

paramsH2 <- data.frame(targetBF   = rep(targetBF, nIterations *length(nTotal)),
                       beta0      = rep(0, nIterations * length(nTotal)),
                       beta1      = rep(effectSize, nIterations * length(nTotal)),
                       beta2      = rep(0, nIterations * length(nTotal)),
                       beta3      = rep(0, nIterations * length(nTotal)),
                       beta4      = rep(effectSize, nIterations * length(nTotal)),
                       beta5      = rep(0, nIterations * length(nTotal)),
                       beta6      = rep(0, nIterations * length(nTotal)),
                       beta7      = rep(0, nIterations * length(nTotal))) 

paramsH3 <- data.frame(targetBF   = rep(targetBF, nIterations *length(nTotal)),
                       beta0      = rep(0, nIterations * length(nTotal)),
                       beta1      = rep(effectSize, nIterations * length(nTotal)),
                       beta2      = rep(0, nIterations * length(nTotal)),
                       beta3      = rep(0, nIterations * length(nTotal)),
                       beta4      = rep(effectSize, nIterations * length(nTotal)),
                       beta5      = rep(effectSize, nIterations * length(nTotal)),
                       beta6      = rep(0, nIterations * length(nTotal)),
                       beta7      = rep(0, nIterations * length(nTotal))) 

paramsH4 <- data.frame(targetBF   = rep(targetBF, nIterations *length(nTotal)),
                       beta0      = rep(0, nIterations * length(nTotal)),
                       beta1      = rep(effectSize, nIterations * length(nTotal)),
                       beta2      = rep(0, nIterations * length(nTotal)),
                       beta3      = rep(0, nIterations * length(nTotal)),
                       beta4      = rep(effectSize, nIterations * length(nTotal)),
                       beta5      = rep(effectSize, nIterations * length(nTotal)),
                       beta6      = rep(0, nIterations * length(nTotal)),
                       beta7      = rep(effectSize, nIterations * length(nTotal))) 

paramsH0 <- as.matrix(paramsH0)
paramsH1 <- as.matrix(paramsH1)
paramsH2 <- as.matrix(paramsH2)
paramsH3 <- as.matrix(paramsH3)
paramsH4 <- as.matrix(paramsH4)

# Creating cluster
numCores <- detectCores() - 1
print(paste('Cores used:', numCores))
cluster  <- makeCluster(numCores)
clusterExport(cluster, c('ttestBF', 
                         'ANOVA_between_one_within_two',
                         'groupSize',
                         'summarise',
                         'ddply',
                         'X',
                         'df'))

# Running analysis
startTime <- Sys.time()
print(paste('Start time:', startTime))
bfH0      <- parRapply(cluster, paramsH0, fixed_tTest_between_one_within_two)
time1     <- Sys.time()
print(paste('Finished 1:', time1))
bfH1      <- parRapply(cluster, paramsH1, fixed_tTest_between_one_within_two)
time2     <- Sys.time()
print(paste('Finished 2:', time2))
bfH2      <- parRapply(cluster, paramsH2, fixed_tTest_between_one_within_two)
time3     <- Sys.time()
print(paste('Finished 3:', time3))
bfH3      <- parRapply(cluster, paramsH3, fixed_tTest_between_one_within_two)
time4     <- Sys.time()
print(paste('Finished 4:', time4))
bfH4      <- parRapply(cluster, paramsH4, fixed_tTest_between_one_within_two)
time5     <- Sys.time()
print(paste('Finished 5:', time5))

# Stopping analysis
stopCluster(cluster)

# Saving results
save.image(file = datedFileNam('noveltyVR_fixed_DesignAnalysis_tTest', '.RData'))
