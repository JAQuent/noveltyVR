# Preparing data for analysis and sharing
# Date: 21/06/2021
# Explanation: Data are loaded, added to data frames and identifiable information is removed and then it 
# is saved for easy sharing

######################################################
# Path to parent folder noveltyVR
path2parent <- "C:/Users/Alex/Documents/noveltyVR" # This need to be changed to run this document
######################################################

# Setting seed
set.seed(20210621)


# /*
# ----------------------------- Libraries --------------------------
# */
library(assortedRFunctions)
library(plyr)
library(reshape2)

# /*
# ----------------------------- Demographics  --------------------------
# */
# Load all files in the folder
folderLocation  <- '/data/ignore_rawData/demographics/'
allFiles        <- paste0(path2parent, folderLocation, list.files(paste0(path2parent, folderLocation)))

demographics        <- do.call(rbind, lapply(allFiles, function(x) read.table(x, stringsAsFactors = FALSE, header = FALSE)))
names(demographics) <- c('subNum', 'group', 'condition', 'usedVR', 'gender', 'age', 'date', 'startTime', 'endTime')

# /*
# ----------------------------- Encoding task --------------------------
# */
# Load all files in the folder
folderLocation  <- '/data/ignore_rawData/judgement/'
allFiles        <- paste0(path2parent, folderLocation, list.files(paste0(path2parent, folderLocation)))

encodData <- do.call(rbind, lapply(allFiles, function(x) read.table(x, stringsAsFactors = FALSE, header = TRUE)))

# Invert accuracy for 1, 2, 73, 5
bool     <- encodData$subNum == 1 | encodData$subNum == 2 | encodData$subNum == 73 | encodData$subNum == 5
tempData <- encodData[bool, 'acc']
acc      <- rep(NA, length(tempData))
acc[tempData == 0]     <- 1
acc[tempData == 1]     <- 0
encodData[bool, 'acc'] <- acc

# Aggregate
encodData_agg <- ddply(encodData,
                       c('subNum', 'group', 'counterBalancing', 'task'),
                       summarise, 
                       rt = mean(RT, na.rm = TRUE), 
                       acc = mean(acc, na.rm = TRUE))

# /*
# ----------------------------- Recall task --------------------------
# */
recallSheet <- read.csv(paste0(path2parent, '/data/ignore_rawData/recallData.csv'))

# /*
# ----------------------------- Recognition task --------------------------
# */
# Load all files in the folder
folderLocation  <- '/data/ignore_rawData/recognition/'
allFiles        <- paste0(path2parent, folderLocation, list.files(paste0(path2parent, folderLocation)))

recogData <- do.call(rbind, lapply(allFiles, function(x) read.csv(x, stringsAsFactors = FALSE)))

# Prepare the data for analysis.
# Correct subNum
recogData$subNum[recogData$subNum == 3] <- 73

# Only the test_part recogTask is relevant. 
recogData <- subset(recogData, test_part == 'recogTask')

# Recode variables (1 means studied)
studied  <- rep(1, dim(recogData)[1])
studied[recogData$task == 'unstudied'] <- 0
recogData$studied <- factor(studied, levels = c(0, 1), c('unstudied', 'studied'))

# Response r= 82, n = 78, f = 70
response <- rep(NA, dim(recogData)[1])
response[recogData$key_press == 82] <- 'r'
response[recogData$key_press == 78] <- 'n'
response[recogData$key_press == 70] <- 'f'
recogData$response <- response

# Calculate accuracy
acc  <- rep(NA, dim(recogData)[1])
acc[studied == 1 & (response == 'r' | response == 'f')] <- 1
acc[studied == 0 & (response == 'n')] <- 1
acc[studied == 1 & (response == 'n')] <- 0
acc[studied == 0 & (response == 'r' | response == 'f')] <- 0
recogData$acc <- acc

# Correct sub 63
sub63   <- recogData[recogData$subNum == '63',]
sub14   <- recogData[recogData$subNum == '14',] # is condition 6
sub56   <- recogData[recogData$subNum == '56',]
sub79 <- recogData[recogData$subNum == '71' & recogData$day == 25,] # is actually condition 3

# Order according to words
sub63 <- sub63[order(sub63$word),]
sub14 <- sub14[order(sub14$word),]
sub56 <- sub56[order(sub56$word),]
sub79 <- sub79[order(sub79$word),] 

# Change  important values
sub63$task      <- sub14$task
sub63$studied   <- sub14$studied
sub63$condition <- 6

sub79$task      <- sub56$task
sub79$subNum    <- 79
sub79$studied   <- sub56$studied
sub79$condition <- 3

# Replug into the dataframe
recogData[recogData$subNum == '63',1:ncol(recogData)] <- sub63
recogData[recogData$subNum == '71' & recogData$day == 25, 1:ncol(recogData)] <- sub79

# /*
# ----------------------------- Questionnaire --------------------------
# */
# Load all files in the folder
folderLocation  <- '/data/ignore_rawData/questionnaire/'
allFiles        <- paste0(path2parent, folderLocation, list.files(paste0(path2parent, folderLocation)))

questData        <- do.call(rbind, lapply(allFiles, function(x) read.csv(x, stringsAsFactors = FALSE)))
names(questData) <- c('num', 
                      'interNum', 
                      'itemName', 
                      'shortcut', 
                      'loading', 
                      'question', 
                      'leftAnchor', 
                      'middleAnchor', 
                      'rightAnchor', 
                      'copyright',
                      'subNum',
                      'date',
                      'time',
                      'rating',
                      'rt')

# Add group to questionnaire
questData$group <- NA
participants    <- unique(questData$subNum)
for(i in 1:length(participants)){
  questData[questData$subNum == participants[i], 'group'] <- encodData_agg$group[encodData_agg$subNum == participants[i]][1]
}

# Make factor
questData$group <- factor(questData$group, level = 1:2, labels = c('Novelty', 'Control'))





# /*
# ----------------------------- Check for missing data --------------------------
# */
# Get all subjects from all tasks etc. 
subNum_demo        <- demographics$subNum
subNum_encoding    <- unique(encodData_agg$subNum)
subNum_recall      <- recallSheet$subNum
subNum_recognition <- unique(recogData$subNum)
subNum_question    <- unique(questData$subNum)

# Difference between demographics and encoding task
setdiff(subNum_demo, subNum_encoding)
setdiff(subNum_encoding, subNum_demo)

# Difference between demographics and recall task
setdiff(subNum_demo, subNum_recall)
setdiff(subNum_recall, subNum_demo)

# Difference between demographics and recognition task
setdiff(subNum_demo, subNum_recognition)
setdiff(subNum_recognition, subNum_demo)

# Difference between demographics and questionnaire
setdiff(subNum_demo, subNum_question)
setdiff(subNum_question, subNum_demo)

# Remove again
rm(subNum_recall, subNum_encoding, subNum_demo, subNum_recognition, subNum_question)

# /*
# ----------------------------- Anonymsation --------------------------
# */

# Exclude
# 7 because code wrong

# Other comments:
# Can't find recall for 41
# 1 & 37 didn't complete the online task
# 62 failed the boot strap test
# 78 did not do the task the next day
exclude <- c(7, 1 , 37, 62, 78, 69, 77)

# Exclusion
demographics   <- demographics[!(demographics$subNum %in% exclude),]
encodData_agg  <- encodData_agg[!(encodData_agg$subNum %in% exclude),]
encodData      <- encodData[!(encodData$subNum %in% exclude),]
recallSheet    <- recallSheet[!(recallSheet$subNum %in% exclude),]
recogData      <- recogData[!(recogData$subNum %in% exclude),]
questData      <- questData[!(questData$subNum %in% exclude),]

# Seed
set.seed(120412)

# /*
# ----------------------------- Anonymsation --------------------------
# */
# Anonymisation
subNums    <- demographics$subNum
subStrings <- anonymise(subNums, 'anonKey_noveltyVR', 7)

encodData_agg$subNum <- replace_all_subs(encodData_agg$subNum, subNums, subStrings)
encodData$subNum     <- replace_all_subs(encodData$subNum, subNums, subStrings)
recallSheet$subNum   <- replace_all_subs(recallSheet$subNum, subNums, subStrings)
recogData$subNum     <- replace_all_subs(recogData$subNum, subNums, subStrings)
questData$subNum     <- replace_all_subs(questData$subNum, subNums, subStrings)

# /*
# ----------------------------- Calculate time difference between encoding and recognition --------------------------
# */
# If the time is before 10 am, there is problem because the zero is removed
encodData$time <- as.character(encodData$time)
# Add zero when only 5 numbers
encodData$time[nchar(encodData$time) == 5] <- paste0(0, encodData$time[nchar(encodData$time) == 5])

# Format: "2020-01-27 13:45:57"
# Convert data and time for encoding task
encod_startTime      <- strptime(x = paste(encodData$date, encodData$time), format = "%Y%m%d %H%M%S")
encodData$startTime  <- as.character(encod_startTime)

# Get times sorted for participants
encodData_times <- ddply(encodData, 
                         c('subNum', 'group', 'counterBalancing'), 
                         summarise, encodTime = startTime[1])

# Combine recog to 1 string
recog_startTime <- paste0(recogData$year, 
                          '-', 
                          recogData$month + 1, 
                          '-',
                          recogData$day, 
                          ' ',
                          recogData$hour,
                          ':',
                          recogData$minute)
recogData$startTime <- as.character(recog_startTime)

# Get times sorted for participants
recogData_times <- ddply(recogData, 
                         c('subNum', 'group', 'condition'), 
                         summarise, recogTime = startTime[1])

# Combine to one data frame
times           <- recogData_times
times$encodTime <- NA
times$encodTime[recogData_times$subNum %in% encodData_times$subNum] <- encodData_times$encodTime[encodData_times$subNum %in% recogData_times$subNum]
times$diff      <- as.numeric(difftime(times$recogTime, times$encodTime, units = 'hours'))

# /*
# ----------------------------- Remove all unnecessary data that could be used to identify participants --------------------------
# */
# Shuffle
demographics <- demographics[sample(1:nrow(demographics)),]

# Delete from demographics
demographics$subNum <- NULL
demographics$date   <- NULL

times$recogTime <- NULL
times$encodTime <- NULL

encodData$date      <- NULL
encodData$startTime <- NULL

questData$date <- NULL

recogData$day <- NULL

# Select variables to save
vars2save <- c('demographics', 'encodData', 'questData', 'encodData_agg', 'recallSheet', 'times', 'recogData')

# /*
# ----------------------------- Save data --------------------------
# */
folderLocation  <- '/data/'
save(list = vars2save, file = paste0(path2parent, folderLocation, 'noveltyVR_data_forSharing.RData'))
