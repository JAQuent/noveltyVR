# This script simulates an fixed N Bayes Factor analysis
# for the project noveltyVR

# Setting seed
set.seed(39257)
 
# Libraries
library(BayesFactor) # Check version
library(parallel)

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
groupSize   <- 48
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
A <- c(rep(1, groupSize * totalLevels), rep(-1, groupSize * totalLevels))
B <- c(rep(c(1, 1, -1, -1), each = groupSize), rep(c(1, 1, -1, -1), each = groupSize))
C <- c(rep(c(1, -1, 1, -1), each = groupSize), rep(c(1, -1, 1, -1), each = groupSize))

# Prepared data frame
df <- data.frame(A = as.factor(A),
                 B = as.factor(B),
                 C = as.factor(C),
                 id = as.factor(c(rep(1:groupSize, totalLevels), rep((groupSize + 1):nTotal, totalLevels))))
levels(df$A) <- c('Group 1', 'Group 2')
levels(df$B) <- c('Level 1', 'Level 2')
levels(df$C) <- c('Level 1', 'Level 2')

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

# Open SBF with upper limit
fixed_ANOVA_between_one_within_two <- function(params){
  targetBF    <- as.numeric(params[1])
  
  # Generating data and adding to data.frame
  df$y  <- as.numeric(ANOVA_between_one_within_two(as.numeric(params[2:9])))
  
  bf <- anovaBF(y ~ A*B*C + id,
                  whichRandom = 'id',
                  data = df,
                  progress = FALSE)
    bfs <- c(as.numeric(as.vector(bf[14]/bf[13])), # What's the evidence for a main effect of A?
             as.numeric(as.vector(bf[9]/bf[8])),   # What's the evidence for an interaction between A and B?
             as.numeric(as.vector(bf[11]/bf[8])),  # What's the evidence for an interaction between A and C?
             as.numeric(as.vector(bf[18]/bf[17]))) # What's the evidence for an interaction between A and B and C?
  
  return(bfs)
}

# Setting parameters
targetBF    <- 6
nIterations <- 3000
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
numCores <- detectCores() - 2
print(paste('Cores used:', numCores))
cluster  <- makeCluster(numCores)
clusterExport(cluster, c('anovaBF', 
                         'ANOVA_between_one_within_two',
                         'groupSize',
                         'X',
                         'df'))

# Running analysis
startTime <- Sys.time()
print(paste('Start time:', startTime))
bfH0      <- parRapply(cluster, paramsH0, fixed_ANOVA_between_one_within_two)
time1     <- Sys.time()
print(paste('Finished 1:', time1))
bfH1      <- parRapply(cluster, paramsH1, fixed_ANOVA_between_one_within_two)
time2     <- Sys.time()
print(paste('Finished 2:', time2))
bfH2      <- parRapply(cluster, paramsH2, fixed_ANOVA_between_one_within_two)
time3     <- Sys.time()
print(paste('Finished 3:', time3))
bfH3      <- parRapply(cluster, paramsH3, fixed_ANOVA_between_one_within_two)
time4     <- Sys.time()
print(paste('Finished 4:', time4))
bfH4      <- parRapply(cluster, paramsH4, fixed_ANOVA_between_one_within_two)
time5     <- Sys.time()
print(paste('Finished 5:', time5))

# Stopping analysis
stopCluster(cluster)

# Saving results
save.image(file = datedFileNam('noveltyVR_fixed_DesignAnalysis_ANOVA', '.RData'))
