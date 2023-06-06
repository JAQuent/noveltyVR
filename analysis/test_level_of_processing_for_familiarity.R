# run the Quent2022_RMarkdown_version.Rmd first

diffValues <- mainData[mainData$encodTask == 'living' & mainData$parameter == 'f', 'value'] - mainData[mainData$encodTask == 'alphabetical' & mainData$parameter == 'f', 'value']

ttestBF(diffValues)
