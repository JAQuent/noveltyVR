# This script creates figures for chapter 3 (noveltyVR)

# It creates the following plots
# 1. Recall group by living
# 2. Recognition quality by living by group
# 3. Hypothesis plots 1.1 1.2 etc.
# 4. Questionnaire: IPQ + Quest1, Quest2 and Quest3

# /* 
# ----------------------------- Setting up ---------------------------
# */
######################################################
# Path to parent folder noveltyVR
path2parent <- "C:/Users/Alex/Documents/noveltyVR" # This need to be changed to run this document
######################################################

# Libs
library(ggplot2)
library(MRColour)
library(plyr)
library(MPTinR)
library(cowplot)
library(assortedRFunctions)
library(reshape2)
theme_set(theme_grey())

# Load data
folderLocation  <- '/data/'
load(paste0(path2parent, folderLocation, 'noveltyVR_data_forSharing.RData'))


# Plot parameters
# Set theme default
updatedTheme <- theme_grey() + theme(axis.title.x  = element_text(size = 12),
                                     axis.title.y  = element_text(size = 10),
                                     axis.text.y  = element_text(size = 10),
                                     axis.text.x  = element_text(size = 10),
                                     plot.title = element_text(size = 12))

theme_set(updatedTheme)

# Variables
conditions     <- 0:11
t1             <- 'alphabetical'
t2             <- 'living'
conditionTable <- data.frame(condition = conditions,
                             list1 = c(1, 1, 2, 2, 3, 3, 1, 1, 2, 2, 3, 3),
                             list2 = c(2, 3, 1, 3, 1, 2, 2, 3, 1, 3, 1, 2),
                             list3 = c(3, 2, 3, 1, 2, 1, 3, 2, 3, 1, 2, 1),
                             aTask = c(rep(t1, 6), rep(t2, 6)),
                             bTask = c(rep(t2, 6), rep(t1, 6)))

conditionTable$aTask <- as.character(conditionTable$aTask)
conditionTable$bTask <- as.character(conditionTable$bTask)

# Note that the col list1, list2 and list3 do not refer to the actual word lists wordList1.txt etc but only 
# to the 1st, 2nd and 3rd list. They col contains which actual wordList is assign to that spot. This might be confusing as in the recallSheet list1 etc refer to the wordLists

# /* 
# ----------------------------- Prepare 1. Recall group by living ---------------------------
# */
# Prepare data
# Assign correct conditions to list 1, 2 and 3
recallData <- data.frame(subNum    = rep(recallSheet$subNum, each = 2),
                         group     = rep(recallSheet$group, each = 2),
                         condition = rep(recallSheet$condition, each = 2),
                         encodTask = rep(c('alphabetical', 'living'), dim(recallSheet)[1]),
                         recalled  = rep(NA_integer_,  dim(recallSheet)[1]*2))

# Loop through recallSheet
for(i in 1:dim(recallSheet)[1]){
  # Parse condition from conditionTable and save in tempCond
  tempCond <- conditionTable[which(recallSheet$condition[i] == conditionTable$condition), ]
  
  # Values from 1st and 2nd list
  val1 <- recallSheet[i, paste0('list', tempCond$list1)]
  val2 <- recallSheet[i, paste0('list', tempCond$list2)]
  
  # Assign to recallData according to the aTask
  if(tempCond$aTask == 'alphabetical'){
    recallData[recallData$subNum == recallSheet$subNum[i] & recallData$encodTask == 'alphabetical', 5] <- val1
    recallData[recallData$subNum == recallSheet$subNum[i] & recallData$encodTask == 'living', 5]       <- val2
  } else {
    recallData[recallData$subNum == recallSheet$subNum[i] & recallData$encodTask == 'alphabetical', 5] <- val2
    recallData[recallData$subNum == recallSheet$subNum[i] & recallData$encodTask == 'living', 5]       <- val1
  }
}

# Make factor
recallData$group <- factor(recallData$group, level = 1:2, labels = c('Novelty', 'Control'))

hypo2.1 <- recallData


# /* 
# ----------------------------- Prepare 2. Recognition quality by living by group ---------------------------
# */
# Split data into two each encoding task
recogData_alpha  <- subset(recogData, task == 'alphabetical' | task == 'unstudied')
recogData_living <- subset(recogData, task == 'living' | task == 'unstudied')

recogData_alpha_agg <- ddply(recogData_alpha, c('subNum', 'group', 'condition'),
                             summarise, 
                             studiedRemebered = sum(studied == 'studied'   & response == 'r'),
                             studiedFamiliar  = sum(studied == 'studied'   & response == 'f'),
                             studiedNew       = sum(studied == 'studied'   & response == 'n'),
                             newRemebered     = sum(studied == 'unstudied' & response == 'r'),
                             newFamiliar      = sum(studied == 'unstudied' & response == 'f'),
                             newNew           = sum(studied == 'unstudied' & response == 'n'))

fit_alpha            <- fit.mpt(recogData_alpha_agg[, 4:9], "analysis/MPT_RKN.model", n.optim = 50)
fit_alpha_individual <- fit_alpha$parameters$individual
fit_alpha_individual <- as.data.frame(t(fit_alpha_individual[,1,]))

recogData_living_agg <- ddply(recogData_living, c('subNum', 'group', 'condition'),
                              summarise,
                              studiedRemebered = sum(studied == 'studied'   & response == 'r'),
                              studiedFamiliar  = sum(studied == 'studied'   & response == 'f'),
                              studiedNew       = sum(studied == 'studied'   & response == 'n'),
                              newRemebered     = sum(studied == 'unstudied' & response == 'r'),
                              newFamiliar      = sum(studied == 'unstudied' & response == 'f'),
                              newNew           = sum(studied == 'unstudied' & response == 'n'))

fit_living            <- fit.mpt(recogData_living_agg[, 4:9], "analysis/MPT_RKN.model", n.optim = 50)
fit_living_individual <- fit_living$parameters$individual
fit_living_individual <- as.data.frame(t(fit_living_individual[,1,]))

participants <- unique(recogData$subNum)

row.names(fit_alpha_individual)  <- NULL
row.names(fit_living_individual) <- NULL

mainData_wide            <- rbind(fit_alpha_individual, fit_living_individual)
mainData_wide$subNum     <- rep(recogData_living_agg$subNum, 2)
mainData_wide$group      <- rep(recogData_living_agg$group, 2)
mainData_wide$encodTask  <- c(rep('alphabetical', length(participants)), rep('living', length(participants)))

# Reshape from wide to long
mainData <- melt(mainData_wide, id.vars = c("subNum", "group", "encodTask"))

# Exclude gr and gf
names(mainData)[4] <- 'parameter'
mainData           <- subset(mainData, parameter != 'gf' & parameter != 'gr')


# Make group factor
mainData$group <- factor(mainData$group, level = 1:2, labels = c('Novelty', 'Control'))

# Arcsine transformation
mainData$trans_value <- asin((mainData$value*2)-1)

hypo4 <- mainData


# /* 
# ----------------------------- Create and Combine the two main plots ---------------------------
# */
hypo2.1$Group <- hypo2.1$group
hypo2.1$encodTask[hypo2.1$encodTask == 'living'] <- 'animacy'

pl1.1 <- ggplot(hypo2.1, aes(x = encodTask, y = recalled, colour = Group)) + 
  geom_boxplot(outlier.shape = NA) + #avoid plotting outliers twice
  geom_point(position = position_jitterdodge()) +
  stat_summary(geom = "point", fun = "mean", col = 'black', size = 3, shape = 24, aes(fill = Group),
               position=position_dodge(width =  0.75),
               key_glyph = "rect") +
  scale_fill_mrc(palette = 'primary') +
  labs(y = 'Number of words recalled', x = 'Encoding task') +
  scale_color_mrc(palette = 'primary') +
  theme(legend.justification=c(0,1), 
        legend.position=c(0,1),
        legend.title = element_blank())

# Change parameter names
hypo4$quality <- ifelse(hypo4$parameter == 'r', 'recollection', 'familiarity')
hypo4$encodTask[hypo4$encodTask == 'living'] <- 'animacy'

pl1.2 <- ggplot(hypo4, aes(x = encodTask, y = trans_value, colour = group)) + 
  facet_grid(.~quality) +
  geom_boxplot(outlier.shape = NA) + #avoid plotting outliers twice
  geom_point(position = position_jitterdodge()) +
  stat_summary(geom = "point", fun = "mean", col = 'black', size = 3, shape = 24, aes(fill = group),
               position=position_dodge(width =  0.75),
               key_glyph = "rect") +
  scale_fill_mrc(palette = 'primary') +
  labs(y = 'Transformed probability estimate', x = 'Encoding task') +
  scale_color_mrc(palette = 'primary') +
  theme(legend.position = 'none')


# Make 1 figure
figure1 <- plot_grid(pl1.1,
                     pl1.2,
                     nrow = 2, labels = 'AUTO')

# save as image
save_plot("analysis/figures/figure1.png", figure1,
          base_height = 18/cm(1),
          base_width = 18/cm(1),
          base_aspect_ratio = 1)

# /* 
# ----------------------------- Prepare 3. Hypothesis plots 1.1 1.2 etc.---------------------------
# */
hypo1.1 <- ddply(recallData, c('subNum', 'group'), summarise, recalled = sum(recalled))

# Calculate hits and false alarms
recogData$hit <- ifelse(recogData$studied == 'studied' & (recogData$response == 'r' | recogData$response == 'f'), 1, 0)
recogData$fa  <- ifelse(recogData$studied == 'unstudied' & (recogData$response == 'r' | recogData$response == 'f'), 1, 0)

# Aggregate
agg_pr <- ddply(recogData, c('subNum', 'group', 'condition'), summarise, pHit = sum(hit)/288, pFA = sum(fa/144))
agg_pr$pr <- agg_pr$pHit - agg_pr$pFA


hypo1.2 <- agg_pr
hypo1.2$group <- factor(hypo1.2$group, levels = c(1, 2), labels =  c("Novelty", "Control"))


# Hypothesis 2.1
diff2.1.1 <- hypo2.1[hypo2.1$group == 'Control' & hypo2.1$encodTask == 'living', 'recalled'] - 
             hypo2.1[hypo2.1$group == 'Control' & hypo2.1$encodTask == 'alphabetical', 'recalled']

diff2.1.2 <- hypo2.1[hypo2.1$group == 'Novelty' & hypo2.1$encodTask == 'living', 'recalled'] - 
             hypo2.1[hypo2.1$group == 'Novelty' & hypo2.1$encodTask == 'alphabetical', 'recalled']


# Plot difference
df_diff2.1 <- data.frame(group = c(rep('Control', length(diff2.1.1)), rep('Novelty', length(diff2.1.2))),
                         value = c(diff2.1.1, diff2.1.2))

# Hypothesis 2.2
# Calculate Pr for alphabetical task (so not living)
agg_pr_alpha    <- ddply(recogData[recogData$task != 'living',], c('subNum', 'group', 'condition'), summarise, pHit = sum(hit)/144, pFA = sum(fa/144))
agg_pr_alpha$pr <- agg_pr_alpha$pHit - agg_pr_alpha$pFA

# Calculate Pr for living task (so not alphabetical)
agg_pr_living    <- ddply(recogData[recogData$task != 'alphabetical',], c('subNum', 'group', 'condition'), summarise, pHit = sum(hit)/144, pFA = sum(fa/144))
agg_pr_living$pr <- agg_pr_living$pHit - agg_pr_living$pFA


diff2.2.1_val <- agg_pr_living[agg_pr_living$group == 2, 'pr'] - agg_pr_alpha[agg_pr_alpha$group == 2, 'pr'] 

diff2.2.2_val <- agg_pr_living[agg_pr_living$group == 1, 'pr'] - agg_pr_alpha[agg_pr_alpha$group == 1, 'pr'] 


# Plot difference
df_diff2.2 <- data.frame(group = c(rep('Control', length(diff2.2.1_val)), rep('Novelty', length(diff2.2.2_val))),
                         value = c(diff2.2.1_val, diff2.2.2_val))


# Hypothesis 3
hypo3    <- ddply(mainData, c('subNum', 'group', 'parameter'), summarise, trans_value = mean(trans_value))
diff3.1  <- hypo3[hypo3$group == 'Control' & hypo3$parameter == 'r', 'trans_value'] - 
            hypo3[hypo3$group == 'Control' & hypo3$parameter == 'f', 'trans_value']
diff3.2  <- hypo3[hypo3$group == 'Novelty' & hypo3$parameter == 'r', 'trans_value'] - 
            hypo3[hypo3$group == 'Novelty' & hypo3$parameter == 'f', 'trans_value']


# Plot difference
df_diff3 <- data.frame(group = c(rep('Control', length(diff3.1)), rep('Novelty', length(diff3.2))),
                       value = c(diff3.1, diff3.2))

# Hypothesis 4
row.names(mainData) <- as.character(1:dim(mainData)[1])

# (E1M1 – E2M1)
diff4.1 <- mainData[mainData$group == 'Control' & mainData$encodTask == 'living' & mainData$parameter == 'f', 'trans_value'] - 
           mainData[mainData$group == 'Control' & mainData$encodTask == 'alphabetical' & mainData$parameter == 'f', 'trans_value']
# (E1M2 – E2M2)
diff4.2 <- mainData[mainData$group == 'Control' & mainData$encodTask == 'living' & mainData$parameter == 'r', 'trans_value'] - 
           mainData[mainData$group == 'Control' & mainData$encodTask == 'alphabetical' & mainData$parameter == 'r', 'trans_value']

# Difference of differences (E1M1 – E2M1) – (E1M2 – E2M2)
diff4.3     <- diff4.1 - diff4.2

# Group 2
# (E1M1 – E2M1)
diff4.4 <- mainData[mainData$group == 'Novelty' & mainData$encodTask =='living' & mainData$parameter == 'f', 'trans_value'] - 
           mainData[mainData$group == 'Novelty' & mainData$encodTask == 'alphabetical' & mainData$parameter == 'f', 'trans_value']
# (E1M2 – E2M2)
diff4.5 <- mainData[mainData$group == 'Novelty' & mainData$encodTask == 'living' & mainData$parameter == 'r', 'trans_value'] - 
           mainData[mainData$group == 'Novelty' & mainData$encodTask == 'alphabetical' & mainData$parameter == 'r', 'trans_value']
# Difference of differences (E1M1 – E2M1) – (E1M2 – E2M2)
diff4.7     <- diff4.4 - diff4.5


df_diff4 <- data.frame(group = c(rep('Control', length(diff4.3)), rep('Novelty', length(diff4.7))),
                       value = c(diff4.3, diff4.7))


# /* 
# ----------------------------- Create and Combine the hypothesis plots ---------------------------
# */
# Plots
# Hypothesis 1.1
pl_hypo1.1 <- ggplot(hypo1.1, aes(x = group, y = recalled, colour = group)) + 
  geom_boxplot(outlier.shape = NA) + #avoid plotting outliers twice
  geom_point(position = position_jitterdodge()) +
  stat_summary(geom = "point", fun = "mean", col = 'black', size = 3, shape = 24, aes(fill = group),
               position=position_dodge(width =  0.75),
               key_glyph = "rect") +
  scale_fill_mrc(palette = 'primary') +
  labs(title = 'Hypothesis 1.1', y = 'Words recalled', x = 'Group') +
  scale_color_mrc(palette = 'primary') +
  theme(legend.position = c(0, 1),
       legend.justification = c(0, 1),
       legend.key.size = unit(0.2, "cm"),
       legend.title = element_blank())

# Hypothesis 1.2
pl_hypo1.2 <- ggplot(hypo1.2, aes(x = group, y = pr, colour = group)) + 
  geom_boxplot(outlier.shape = NA) + #avoid plotting outliers twice
  geom_point(position = position_jitterdodge()) +
  stat_summary(geom = "point", fun = "mean", col = 'black', size = 3, shape = 24, aes(fill = group),
               position=position_dodge(width =  0.75),
               key_glyph = "rect") +
  scale_fill_mrc(palette = 'primary') +
  labs(title = 'Hypothesis 1.2', y = 'Pr', x = 'Group') +
  scale_color_mrc(palette = 'primary') +
  theme(legend.position = 'none')

# Hypothesis 2.1
pl_hypo2.1 <- ggplot(df_diff2.1, aes(x = group, y = value, colour = group)) + 
  geom_boxplot(outlier.shape = NA) + #avoid plotting outliers twice
  geom_point(position = position_jitterdodge()) +
  stat_summary(geom = "point", fun = "mean", col = 'black', size = 3, shape = 24, aes(fill = group),
               position=position_dodge(width =  0.75),
               key_glyph = "rect") +
  scale_fill_mrc(palette = 'primary') +
  labs(title = 'Hypothesis 2.1', y = 'Words recalled', x = 'Group') +
  scale_color_mrc(palette = 'primary') +
  theme(legend.position = 'none')

# Hypothesis 2.2
pl_hypo2.2 <- ggplot(df_diff2.2, aes(x = group, y = value, colour = group)) + 
  geom_boxplot(outlier.shape = NA) + #avoid plotting outliers twice
  geom_point(position = position_jitterdodge()) +
  stat_summary(geom = "point", fun = "mean", col = 'black', size = 3, shape = 24, aes(fill = group),
               position=position_dodge(width =  0.75),
               key_glyph = "rect") +
  scale_fill_mrc(palette = 'primary') +
  labs(title = 'Hypothesis 2.2', y = 'Pr', x = 'Group') +
  scale_color_mrc(palette = 'primary') +
  theme(legend.position = 'none')

# Hypothesis 3
pl_hypo3 <- ggplot(df_diff3, aes(x = group, y = value, colour = group)) + 
  geom_boxplot(outlier.shape = NA) + #avoid plotting outliers twice
  geom_point(position = position_jitterdodge()) +
  stat_summary(geom = "point", fun = "mean", col = 'black', size = 3, shape = 24, aes(fill = group),
               position=position_dodge(width =  0.75),
               key_glyph = "rect") +
  scale_fill_mrc(palette = 'primary') +
  labs(title = 'Hypothesis 3', y = 'Trans. est. probability', x = 'Group') +
  scale_color_mrc(palette = 'primary') +
  theme(legend.position = 'none')

# Hypothesis 4
pl_hypo4 <- ggplot(df_diff4, aes(x = group, y = value, colour = group)) + 
  geom_boxplot(outlier.shape = NA) + #avoid plotting outliers twice
  geom_point(position = position_jitterdodge()) +
  stat_summary(geom = "point", fun = "mean", col = 'black', size = 3, shape = 24, aes(fill = group),
               position=position_dodge(width =  0.75),
               key_glyph = "rect") +
  scale_fill_mrc(palette = 'primary') +
  labs(title = 'Hypothesis 4', y = 'Trans. est. probability', x = 'Group') +
  scale_color_mrc(palette = 'primary') +
  theme(legend.position = 'none')

# Make 1 figure
figure2 <- plot_grid(pl_hypo1.1, pl_hypo1.2, pl_hypo2.1, pl_hypo2.2, pl_hypo3, pl_hypo4, nrow = 3 , labels = 'AUTO')

# save as image
save_plot("analysis/figures/figure2.png", figure2,
          base_height = 18/cm(1),
          base_width = 18/cm(1),
          base_aspect_ratio = 1)
# /* 
# ----------------------------- Prepare 4. Questionnaire: IPQ + Quest1, Quest2 and Quest3---------------------------
# */
# Fix error of NA as rowname
names_quest <- names(questData)
names_quest[which(is.na(names_quest))] <- 'group1'
names(questData) <- names_quest

quests <- unique(questData$question)
nQuest <- length(quests)

# Recode so that scale goes from 0 to 6 like the original 
questData$trans_rating <- (questData$rating + 100)*0.03

# Change reverse coding of sp2, inv3  & real1
# SP2
questData$trans_rating[questData$itemName == 'SP2']   <- -1*questData$trans_rating[questData$itemName == 'SP2'] + 6
# INV3
questData$trans_rating[questData$itemName == 'INV3']  <- -1*questData$trans_rating[questData$itemName == 'INV3'] + 6
# REAL1
questData$trans_rating[questData$itemName == 'REAL1'] <- -1*questData$trans_rating[questData$itemName == 'REAL1'] + 6


# Only IPQ
ipq        <- questData[questData$copyright != 'noveltyVR',]
ipq_scores <- ddply(ipq, c('subNum', 'group'), summarise, score = sum(trans_rating))

# Quest1: 
quest1 <- "This experience was novel."
quest1_only <-  questData[questData$question == quest1,]

# Quest2:
quest2 <- "This experience was exciting."
quest2_only <-  questData[questData$question == quest2,]

# Quest3: 
quest3 <- "This experiences was uncomfortable."
quest3_only <-  questData[questData$question == quest3,]

# /* 
# ----------------------------- Create and Combine the questionnaire plots ---------------------------
# */
# IPQ score
pl_ipq <- ggplot(ipq_scores, aes(x = group, y = score, colour = group)) + 
  geom_boxplot(outlier.shape = NA) + #avoid plotting outliers twice
  geom_point(position = position_jitterdodge()) +
  stat_summary(geom = "point", fun = "mean", col = 'black', size = 3, shape = 24, aes(fill = group),
               position=position_dodge(width =  0.75),
               key_glyph = "rect") +
  scale_fill_mrc(palette = 'primary') +
  labs(title = 'IPQ', y = 'Score', x = 'Group') +
  scale_color_mrc(palette = 'primary') +
  theme(legend.position = c(0.5,0.96),
        legend.key.size = unit(0.15, "cm"),
        legend.title = element_blank())
  # theme(legend.position = c(0, 1),
  #       legend.justification = c(0, 1),
  #       legend.key.size = unit(0.15, "cm"),
  #       legend.title = element_blank())


pl_quest1 <- ggplot(quest1_only, aes(x = group, y = trans_rating, colour = group)) + 
  geom_boxplot(outlier.shape = NA) + #avoid plotting outliers twice
  geom_point(position = position_jitterdodge()) +
  stat_summary(geom = "point", fun = "mean", col = 'black', size = 3, shape = 24, aes(fill = group),
               position=position_dodge(width =  0.75),
               key_glyph = "rect") +
  scale_fill_mrc(palette = 'primary') +
  labs(title = quest1, y = 'Score', x = 'Group') +
  scale_color_mrc(palette = 'primary') +
  theme(legend.position = 'none')

pl_quest2 <- ggplot(quest2_only, aes(x = group, y = trans_rating, colour = group)) + 
  geom_boxplot(outlier.shape = NA) + #avoid plotting outliers twice
  geom_point(position = position_jitterdodge()) +
  stat_summary(geom = "point", fun = "mean", col = 'black', size = 3, shape = 24, aes(fill = group),
               position=position_dodge(width =  0.75),
               key_glyph = "rect") +
  scale_fill_mrc(palette = 'primary') +
  labs(title = quest2, y = 'Score', x = 'Group') +
  scale_color_mrc(palette = 'primary') +
  theme(legend.position = 'none')

pl_quest3 <- ggplot(quest3_only, aes(x = group, y = trans_rating, colour = group)) + 
  geom_boxplot(outlier.shape = NA) + #avoid plotting outliers twice
  geom_point(position = position_jitterdodge()) +
  stat_summary(geom = "point", fun = "mean", col = 'black', size = 3, shape = 24, aes(fill = group),
               position=position_dodge(width =  0.75),
               key_glyph = "rect") +
  scale_fill_mrc(palette = 'primary') +
  labs(title = "This experience was uncomfortable.", y = 'Score', x = 'Group') +
  scale_color_mrc(palette = 'primary') +
  theme(legend.position = 'none')

# Make 1 figure
figure3 <- plot_grid(pl_ipq, pl_quest1, pl_quest2, pl_quest3, nrow = 2 , labels = 'AUTO')

# save as image
save_plot("analysis/figures/figure3.png", figure3,
          base_height = 18/cm(1),
          base_width = 18/cm(1),
          base_aspect_ratio = 1)