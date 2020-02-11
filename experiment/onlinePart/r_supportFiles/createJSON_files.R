# Create json files for online part of noveltyVR
# /* 
# ----------------------------- General stuff ---------------------------
# */
# Set WD
setwd("U:/Projects/noveltyVR/experiment/onlinePart/r_supportFiles")

# Library
library(rjson)
library(assortedRFunctions)

# Loads wordlists
wordList1  <- read.table('wordList_1.txt', header = FALSE)
wordList2  <- read.table('wordList_2.txt', header = FALSE)
wordList3  <- read.table('wordList_3.txt', header = FALSE)

# Converts to character
wordList1$V1 <- as.character(wordList1$V1)
wordList2$V1 <- as.character(wordList2$V1)
wordList3$V1 <- as.character(wordList3$V1)
wordList1$V2 <- as.character(wordList1$V2)
wordList2$V2 <- as.character(wordList2$V2)
wordList3$V2 <- as.character(wordList3$V2)
wordList1$V3 <- as.character(wordList1$V3)
wordList2$V3 <- as.character(wordList2$V3)
wordList3$V3 <- as.character(wordList3$V3)


# Variables
nBlock         <- 72
listLength     <- 144
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
# /* 
# ----------------------------- Creating conditions ---------------------------
# */
# Loop trough all conditions and create the respective variables
for(i in 1:length(conditions)){
  # Parses and assigns list and task depending on the condition
  list1 <- get(paste0('wordList', conditionTable$list1[i])) # A list
  list2 <- get(paste0('wordList', conditionTable$list2[i])) # B list
  list3 <- get(paste0('wordList', conditionTable$list3[i])) # Unstudied words
  aTask <- conditionTable$aTask[i] # Determines which task is A task
  bTask <- conditionTable$bTask[i] # Determines which task is B task
  
  # Generates the variables based on the parsed input. Parsed lists a split according to ABBA design
  tempWords        <- c(list1[1:nBlock, 'V1'], list2[, 'V1'], list1[(nBlock+1):(nBlock*2), 'V1'], list3[, 'V1'])
  tempLiving       <- c(list1[1:nBlock, 'V2'], list2[, 'V2'], list1[(nBlock+1):(nBlock*2), 'V2'], list3[, 'V2']) 
  tempAlphabetical <- c(list1[1:nBlock, 'V3'], list2[, 'V3'], list1[(nBlock+1):(nBlock*2), 'V3'], list3[, 'V3'])
  tempTask         <- c(rep(aTask, nBlock), rep(bTask, listLength), rep(aTask, nBlock), rep('unstudied', listLength))
  TempBlock        <- c(rep(1:4, each = nBlock), rep(NA, listLength))
  
  # Assign to variables
  assign(paste0('words', conditions[i]), tempWords)
  assign(paste0('living', conditions[i]), tempLiving)
  assign(paste0('alphabetical', conditions[i]), tempAlphabetical)
  assign(paste0('task', conditions[i]), tempTask)
  assign(paste0('block', conditions[i]), TempBlock)
}

# Add list

# /* 
# ----------------------------- Creating JSON strings ---------------------------
# */
words <- list(words0, 
              words1,
              words2,
              words3,
              words4,
              words5,
              words6,
              words7,
              words8,
              words9,
              words10,
              words11)
words_str <- create_json_variable_str('words', words)

living <- list(living0, 
               living1,
               living2,
               living3,
               living4,
               living5,
               living6,
               living7,
               living8,
               living9,
               living10,
               living11)
living_str <- create_json_variable_str('living', living)

alphabetical <- list(alphabetical0, 
                     alphabetical1,
                     alphabetical2,
                     alphabetical3,
                     alphabetical4,
                     alphabetical5,
                     alphabetical6,
                     alphabetical7,
                     alphabetical8,
                     alphabetical9,
                     alphabetical10,
                     alphabetical11)
alphabetical_str <- create_json_variable_str('alphabetical', alphabetical)

task <- list(task0, 
             task1,
             task2,
             task3,
             task4,
             task5,
             task6,
             task7,
             task8,
             task9,
             task10,
             task11)
task_str <- create_json_variable_str('task', task)

block <- list(block0, 
              block1,
              block2,
              block3,
              block4,
              block5,
              block6,
              block7,
              block8,
              block9,
              block10,
              block11)
block_str <- create_json_variable_str('block', block)

encodingTrial     <- list(c(1:(listLength*2), rep('NA', listLength)))
encodingTrial_str <- create_json_variable_str('encodingTrial', encodingTrial)
# /* 
# ----------------------------- Create JS file  ---------------------------
# */
sink('wordLists.js')
cat(words_str)
cat('\n\n')
cat('words = JSON.parse(words);')
cat('\n\n')
cat(living_str)
cat('\n\n')
cat('living = JSON.parse(living);')
cat('\n\n')
cat(alphabetical_str)
cat('\n\n')
cat('alphabetical = JSON.parse(alphabetical);')
cat('\n\n')
cat(task_str)
cat('\n\n')
cat('task = JSON.parse(task);')
cat('\n\n')
cat(block_str)
cat('\n\n')
cat('block = JSON.parse(block);')
cat('\n\n')
cat(encodingTrial_str)
cat('\n\n')
cat('encodingTrial = JSON.parse(encodingTrial);')
sink()

# /* 
# ----------------------------- Save image ---------------------------
# */
save.image(datedFileNam('counterBalancing', '.RData'))

