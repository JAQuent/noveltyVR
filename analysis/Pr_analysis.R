# This script quickly runs a Pr analysis to confirm the MTP results. 


######################################################
# Path to parent folder noveltyVR
#path2parent <- "C:/Users/aq01/Documents/noveltyVR" # This need to be changed to run this document
path2parent <- "C:/Users/Alex/Documents/noveltyVR" # This need to be changed to run this document
######################################################


# Libs
library(plyr)
library(matrixTests)
library(reshape2)
library(knitr)
library(chron)
library(ggplot2)
library(MRColour)
library(cowplot)
library(BayesFactor)
library(assortedRFunctions)
theme_set(theme_grey())

# Parameters
digits <- 3 # for signif/rounding

# Load data
folderLocation  <- '/data/'
load(paste0(path2parent, folderLocation, 'noveltyVR_data_forSharing.RData'))

# Calculate hits and false alarms
recogData$hit <- ifelse(recogData$studied == 'studied' & (recogData$response == 'r' | recogData$response == 'f'), 1, 0)
recogData$fa  <- ifelse(recogData$studied == 'unstudied' & (recogData$response == 'r' | recogData$response == 'f'), 1, 0)


# Aggregate
agg_pr <- ddply(recogData, c('subNum', 'group', 'condition'), summarise, pHit = sum(hit)/288, pFA = sum(fa/144))
agg_pr$pr <- agg_pr$pHit - agg_pr$pFA

reportBF(1/ttestBF(agg_pr$pr[agg_pr$group == 1], agg_pr$pr[agg_pr$group == 2], paired = FALSE, nullInterval = c(-Inf, 0))[2], 3, 'signif')

mean_SD_str2(agg_pr$pr[agg_pr$group == 1], type = 2, digits =  3, rounding_type = 'signif')
mean_SD_str2(agg_pr$pr[agg_pr$group == 2], type = 2, digits =  3, rounding_type = 'signif')

# Hypothesis 0 
# Calculate Pr for alphabetical task (so not living)
agg_pr_alpha    <- ddply(recogData[recogData$task != 'living',], c('subNum', 'group', 'condition'), summarise, pHit = sum(hit)/144, pFA = sum(fa/144))
agg_pr_alpha$pr <- agg_pr_alpha$pHit - agg_pr_alpha$pFA

# Calculate Pr for living task (so not alphabetical)
agg_pr_living    <- ddply(recogData[recogData$task != 'alphabetical',], c('subNum', 'group', 'condition'), summarise, pHit = sum(hit)/144, pFA = sum(fa/144))
agg_pr_living$pr <- agg_pr_living$pHit - agg_pr_living$pFA

test0 <- ttestBF(agg_pr_alpha$pr, agg_pr_living$pr, paired = TRUE)
bf0   <- reportBF(test0)
str1 <- mean_SD_str2(agg_pr_alpha$pr, 1, digits, 'signif')
str2 <- mean_SD_str2(agg_pr_living$pr, 1, digits, 'signif')

diffScore <-  agg_pr_living$pr - agg_pr_alpha$pr
d0   <- signif(mean(diffScore)/sd(diffScore), digits)


# Hypothesis 2
diff2.2.1_val <- agg_pr_living[agg_pr_living$group == 2, 'pr'] - agg_pr_alpha[agg_pr_alpha$group == 2, 'pr'] 

diff2.2.2_val <- agg_pr_living[agg_pr_living$group == 1, 'pr'] - agg_pr_alpha[agg_pr_alpha$group == 1, 'pr'] 

# Get values
val1 <- diff2.2.1_val
val2 <- diff2.2.2_val

# Create report strings
str3 <- mean_SD_str2(val1, 1, digits, 'signif')
str4 <- mean_SD_str2(val2, 1, digits, 'signif')
d2   <- signif(cohens_d_raw(val1, val2), digits)

test2.2   <- ttestBF(x = diff2.2.1_val,
                     y = diff2.2.2_val,
                     nullInterval = c(-Inf, 0))

results[4, 2] <- as.numeric(as.vector(test2.2[2]))
results[4, 3] <- 1/as.numeric(as.vector(test2.2[2]))
bf2           <- signif(results[4, 2], digits)
