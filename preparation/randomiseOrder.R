# This scripts randomises the order of the words.
set.seed(389)

setwd("U:/Projects/noveltyVR/preparation")


listLength <- 144
halfLength <- listLength/2

list1 <- read.table("wordList_1.txt")
list2 <- read.table("wordList_2.txt")
list3 <- read.table("wordList_3.txt")

list1_1 <- rbind(list1[1:18, ],
                 list1[37:54, ],
                 list1[73:90, ],
                 list1[109:126, ])
list1_2 <- rbind(list1[19:36, ],
                 list1[55:72, ],
                 list1[91:108, ],
                 list1[127:144, ])

list2_1 <- rbind(list2[1:18, ],
                 list2[37:54, ],
                 list2[73:90, ],
                 list2[109:126, ])
list2_2 <- rbind(list2[19:36, ],
                 list2[55:72, ],
                 list2[91:108, ],
                 list2[127:144, ])

list3_1 <- rbind(list3[1:18, ],
                 list3[37:54, ],
                 list3[73:90, ],
                 list3[109:126, ])
list3_2 <- rbind(list3[19:36, ],
                 list3[55:72, ],
                 list3[91:108, ],
                 list3[127:144, ])

list1_1 <- list1_1[sample(halfLength),]
list1_2 <- list1_2[sample(halfLength),]
list2_1 <- list2_1[sample(halfLength),]
list2_2 <- list2_2[sample(halfLength),]
list3_1 <- list3_1[sample(halfLength),]
list3_2 <- list3_2[sample(halfLength),]

list1 <- rbind(list1_1, list1_2)
list2 <- rbind(list2_1, list2_2)
list3 <- rbind(list3_1, list3_2)

write.table(list1,
            'wordList_1.txt',
            row.names = FALSE,
            col.names = FALSE,
            quote     = FALSE,
            sep       = ' ')

write.table(list2,
            'wordList_2.txt',
            row.names = FALSE,
            col.names = FALSE,
            quote     = FALSE,
            sep       = ' ')

write.table(list3,
            'wordList_3.txt',
            row.names = FALSE,
            col.names = FALSE,
            quote     = FALSE,
            sep       = ' ')