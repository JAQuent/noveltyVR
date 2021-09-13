#setwd("~/noveltyVR/analysis")
setwd("~/noveltyVR")

# # Load all files in the folder
# folderLocation  <- '/data/ignore_rawData/recognition/'
# allFiles        <- paste0(path2parent, folderLocation, list.files(paste0(path2parent, folderLocation)))
# 
# recogData <- do.call(rbind, lapply(allFiles, function(x) read.csv(x, stringsAsFactors = FALSE)))



# Calculate hits and false alarms
recogData$hit <- ifelse(recogData$studied == 'studied' & (recogData$response == 'r' | recogData$response == 'f'), 1, 0)
recogData$fa  <- ifelse(recogData$studied == 'unstudied' & (recogData$response == 'r' | recogData$response == 'f'), 1, 0)

# Aggregate
agg_pr <- ddply(recogData, c('subNum', 'group', 'condition'), summarise, pHit = sum(hit)/288, pFA = sum(fa/144))
agg_pr$pr <- agg_pr$pHit - agg_pr$pFA

# Bootstrap
nIter         <- 10000
subNums       <- unique(agg_pr$subNum)
agg_pr$cut_80 <- NA
agg_pr$cut_95 <- NA
agg_pr$hit_fa <- NA

# Loop across all subjects
for(j in 1:length(subNums)){
  # Bootstrap within the subject
  
  # Reset prs
  prs           <- c()
  
  for(i in 1:nIter){
    # Subset for only one subject
    bootstrap_df <- recogData[recogData$subNum == subNums[j], ]
    
    # Shuffle
    bootstrap_df$response <- sample(bootstrap_df$response)
    
    # Calculate hits and false alarms
    bootstrap_df$hit <- ifelse(bootstrap_df$studied == 'studied'   & (bootstrap_df$response == 'r' | bootstrap_df$response == 'f'), 1, 0)
    bootstrap_df$fa  <- ifelse(bootstrap_df$studied == 'unstudied' & (bootstrap_df$response == 'r' | bootstrap_df$response == 'f'), 1, 0)
    
    # Aggregate
    pHit = sum(bootstrap_df$hit)/288
    pFA = sum(bootstrap_df$fa)/144
    
    # Add results
    prs <- c(prs, pHit - pFA)
  }
  
  # Add the cut off values
  agg_pr$hit_fa <- sum(c(bootstrap_df$hit, bootstrap_df$fa))
  agg_pr$cut_80[agg_pr$subNum == subNums[j]] <- quantile(prs, probs = 0.80)
  agg_pr$cut_95[agg_pr$subNum == subNums[j]] <- quantile(prs, probs = 0.95)
}


# How many people do I have to replace for 80%
sum(agg_pr$pr < agg_pr$cut_80)
agg_pr$subNum[agg_pr$pr < agg_pr$cut_80]
agg_pr$group[agg_pr$pr < agg_pr$cut_80]
agg_pr$condition[agg_pr$pr < agg_pr$cut_80]

# How many people do I have to replace for 95%
sum(agg_pr$pr < agg_pr$cut_95)
agg_pr$subNum[agg_pr$pr < agg_pr$cut_95]
agg_pr$group[agg_pr$pr < agg_pr$cut_95]
agg_pr$condition[agg_pr$pr < agg_pr$cut_95]

# Save  agg_pr
save(agg_pr, file = 'bootstrap_20210621.RData')
