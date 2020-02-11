# Create participant sheet
# Minimum number of participants
nTotal <- 72
sets   <- c(111, 246, 388, 498, 848)

# Novelty Group 1
group1 <- data.frame(subNum = seq(1, nTotal, 2))
# Control Group 2
group2 <- data.frame(subNum = seq(2, nTotal, 2))

# Counter balancing 0 to 11
group1$cond <- rep(0:11, 3)
group1$set  <- rep(sets, 8)[1:(nTotal/2)]
group2$cond <- rep(0:11, 3)
group2$set  <- rep(sets, 8)[2:(nTotal/2+1)]


# Create URL
#https://lsr-studies-01.mrc-cbu.cam.ac.uk/publix/322/start?batchId=394&generalMultiple
# Example https://lsr-studies-01.mrc-cbu.cam.ac.uk/publix/322/start?sub=1&group=1&cond=0&batchId=394&generalMultiple
prefix_url <- 'https://lsr-studies-01.mrc-cbu.cam.ac.uk/publix/322/start?'
suffix_url <- '&batchId=395&generalMultiple'

# For group 1
url1 <- c()
for(i in 1:dim(group1)[1]){
  url1[i] <- paste0(prefix_url, 'sub=', group1$subNum[i], '&group=1&cond=', group1$cond[i], suffix_url)
}
group1$url <- url1

# For group 2
url2 <- c()
for(i in 1:dim(group2)[1]){
  url2[i] <- paste0(prefix_url, 'sub=', group2$subNum[i], '&group=2&cond=', group2$cond[i], suffix_url)
}
group2$url <- url2


# Write to files
write.table(group1, 'group1.txt', row.names = FALSE)
write.table(group2, 'group2.txt', row.names = FALSE)