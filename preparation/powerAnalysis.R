# Script to run power analysis for novelty VR
# Version 1.0
# Date: 02/10/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
library(ggplot2)
library(BayesFactor)
library(plyr)
library(assortedRFunctions)


# /* 
# ----------------------------- Getting report effect sizes ---------------------------
# */
# Get previous cohen's d from
# Fenker, D. E., Frey, J. U., Schuetze, H., Heipertz, D., Heinze, H.-J., & Düzel, E. (2008). 
# Novel scenes improve recollection and recall of words. Journal of Cognitive Neuroscience, 20(7), 
# 1250–1265. https://doi.org/10.1162/jocn.2008.20086

# Schomaker, J., van Bronkhorst, M. L. V, & Meeter, M. (2014). Exploring a novel environment improves 
# motivation and promotes recall of words. Frontiers in Psychology, 5(AUG), 1–6. https://doi.org/10.3389/fpsyg.2014.00918

# For instance: From Table 1: Rem/fa Familiar 3% (SD = 4%) and novel 6% (SD = 6%).
fenker_exp1_immediate_remember <- (6 - 3)/sqrt((6 * 6 + 4 * 4)/2)
fenker_exp1_delayed_remember   <- (3 - 1)/sqrt((3 * 3 + 1 * 1)/2)
fenker_exp2_immediate_recall   <- (55 - 45)/sqrt((12 * 12 + 9 * 9)/2)

# f = sqr( eta^2 / ( 1 - eta^2 ) ). Thus d = 2*f fits with https://www.psychometrica.de/effect_size.html
schomaker_recall     <- 2*sqrt(0.16/( 1 - 0.16))

effSizes <- c(fenker_exp1_immediate_remember, 
              fenker_exp1_delayed_remember, 
              fenker_exp2_immediate_recall,
              schomaker_recall)
medianD <- median(effSizes)


# /* 
# ----------------------------- Simulations ---------------------------
# */
# Simluation wide parameters
nRuns  <- 1000
ns     <- seq(10, 40, 1)
rscale <- sqrt(2)/2

# /* 
# ----------------------------- One-tailed comparisons ---------------------------
# */
# H1 
bfs    <- c()
ps     <- c()
ds     <- c()
d_true <- medianD
index  <- 1
for(j in ns){
  for(i in 1:nRuns){
    a          <- rnorm(j, 0, 1)
    b          <- rnorm(j, d_true, 1)
    bfs[index] <- as.numeric(as.vector(ttestBF(a, b, nullInterval = c(0, Inf), rscale = rscale)[2]))
    ps[index]  <- t.test(a, b, var.equal = TRUE, alternative = 'less')$p.value
    ds[index]  <- cohens_d(a, b)
    index      <- index + 1 
  }
}

onetailed_H1 <- data.frame(n = rep(ns, each = nRuns),
                           d = ds,
                           p = ps,
                           BF10 = bfs)

onetailed_H1_agg <- ddply(onetailed_H1, 
                          c('n'),
                          summarise,
                          d = mean(d),
                          p = mean(p < 0.05),
                          missBF10 = mean(BF10 < 1/6),
                          BF10 = mean(BF10 > 6))

# H0
bfs    <- c()
ps     <- c()
ds     <- c()
d_true <- 0
index  <- 1
for(j in ns){
  for(i in 1:nRuns){
    a          <- rnorm(j, 0, 1)
    b          <- rnorm(j, d_true, 1)
    bfs[index] <- as.numeric(as.vector(ttestBF(a, b, nullInterval = c(0, Inf), rscale = rscale)[2]))
    ps[index]  <- t.test(a, b, var.equal = TRUE, alternative = 'less')$p.value
    ds[index]  <- cohens_d(a, b)
    index      <- index + 1 
  }
}


onetailed_H0 <- data.frame(n = rep(ns, each = nRuns),
                           d = ds,
                           p = ps,
                           BF01 = 1/bfs)

onetailed_H0_agg <- ddply(onetailed_H0, 
                          c('n'),
                          summarise,
                          d = mean(d),
                          p = mean(p < 0.05),
                          missBF01 = mean(BF01 < 1/6),
                          BF01 = mean(BF01 > 6))


# /* 
# ----------------------------- two-tailed comparisons ---------------------------
# */
# H1 
bfs    <- c()
ps     <- c()
ds     <- c()
d_true <- medianD
index  <- 1
for(j in ns){
  for(i in 1:nRuns){
    a          <- rnorm(j, 0, 1)
    b          <- rnorm(j, d_true, 1)
    bfs[index] <- as.numeric(as.vector(ttestBF(a, b, rscale = rscale)))
    ps[index]  <- t.test(a, b, var.equal = TRUE)$p.value
    ds[index]  <- cohens_d(a, b)
    index      <- index + 1 
  }
}


twotailed_H1 <- data.frame(n = rep(ns, each = nRuns),
                           d = ds,
                           p = ps,
                           BF10 = bfs)

twotailed_H1_agg <- ddply(twotailed_H1, 
                          c('n'),
                          summarise,
                          d = mean(d),
                          p = mean(p < 0.05),
                          missBF10 = mean(BF10 < 1/6),
                          BF10 = mean(BF10 > 6))

# H0
bfs    <- c()
ps     <- c()
ds     <- c()
d_true <- 0
index  <- 1
for(j in ns){
  for(i in 1:nRuns){
    a          <- rnorm(j, 0, 1)
    b          <- rnorm(j, d_true, 1)
    bfs[index] <- as.numeric(as.vector(ttestBF(a, b, rscale = rscale)))
    ps[index]  <- t.test(a, b, var.equal = TRUE)$p.value
    ds[index]  <- cohens_d(a, b)
    index      <- index + 1 
  }
}


twotailed_H0 <- data.frame(n = rep(ns, each = nRuns),
                           d = ds,
                           p = ps,
                           BF01 = 1/bfs)

twotailed_H0_agg <- ddply(twotailed_H0, 
                          c('n'),
                          summarise,
                          d = mean(d),
                          p = mean(p < 0.05),
                          missBF01 = mean(BF01 < 1/6),
                          BF01 = mean(BF01 > 6))

# /* 
# ----------------------------- Saving data ---------------------------
# */
save.image(file = datedFileNam('powerAnalysis', '.RData'))