rm(list=ls())
source("./functions.R")
load("../Chapter 2 - Changepoints in the Holocene Eruption Record/ProcessedSet.RData")
sortedData = sortedData2
rm(sortedData2)
sortedData$VEI = as.numeric(levels(sortedData$VEI)[sortedData$VEI])

#####Cumulative distribution plots to check for data completeness#####
windows(5.5,4)
par(cex=1.0, mar = c(4,4,2,2), mgp=c(1,1,1), xaxp = c(-1000, 2000, 12), 
    xaxt = 's', xaxs='r', yaxs = 'r', mgp = c(2.0,1,0), bty = 'l')

cumEvents(sortedData, xlab = "Year", ylab = "Cumulative number of events", col = 'black')
cumEvents(subset(sortedData, sortedData$`Lahar or mudflow`=='Lahar or mudflow'), add = TRUE, col ='grey')

#Lahar comparison plot
library(dplyr)
regs <- c('Africa and Red Sea',
          'Alaska',
          'Antarctica',
          'Atlantic Ocean',
          'Canada and Western USA',
          'Hawaii and Pacific Ocean',
          'Iceland and Arctic Ocean',
          'Indonesia',
          'Japan, Taiwan, Marianas',
          'Kamchatka and Mainland Asia',
          'Kuril Islands',
          'Mediterranean and W Asia',
          'Melanesia and Australia',
          'Mexico and Central America',
          'Middle East and Indian Ocean',
          'New Zealand to Fiji',
          'Philippines and SE Asia',
          'South America',
          'West Indies')
dates <- c(1820,
  1784,
  1893,
  1988,
  1841,
  1820,
  1702,
  1770,
  1542,
  1737,
  1760,
  1682,
  1855,
  1517,
  1750,
  1836,
  1808,
  1737,
  1965)
cpDates <- data.frame(regs, dates)
cpDates <- group_by(cpDates, regs)

changePcatalogue <- filter(sortedData, sortedData$Region == cpDates$regs[1] & Start.Year > cpDates$dates[1])

for (i in 2:nrow(cpDates))
{
  changePcatalogue <- bind_rows(changePcatalogue, filter(sortedData, sortedData$Region == cpDates$regs[i] & Start.Year > cpDates$dates[i]))
}
hist(changePcatalogue$VEI, breaks = seq(0,7, by = 1), right=FALSE, col = "gray", bty = "l", ylim = range(c(0,2000)), main = NULL,
      ylab = "No. of eruptions", xlab = "Volcanic Explosivity Index (VEI)")#main = "Explosivity of volcanoes",

hall <- hist(changePcatalogue$VEI, breaks = seq(0, 7, by = 1), right = FALSE)
LaharSet <- subset(changePcatalogue, changePcatalogue$`Lahar or mudflow`=='Lahar or mudflow')
NonSet <- subset(changePcatalogue, is.na(changePcatalogue$`Lahar or mudflow`))
hl <- hist(LaharSet$VEI, breaks = seq(0, 7, by = 1), right = FALSE)
hn <- hist(NonSet$VEI, breaks = seq(0, 7, by = 1), right = FALSE)

windows(7,7)
pdf(file = "./prop.pdf", width = 7.5, height = 6)
par(cex=1.0, mar = c(4,4,2,4), mgp=c(1,1,0), xaxt = 's', xaxs='i', yaxs = 'i', mgp = c(2.0,1,0), bty = 'o')
barplot((hl$counts/hall$counts)+(hn$counts/hall$counts), space = 0, col = "firebrick2", yaxt = 'n')
barplot(hn$counts/hall$counts, col = 'gray', space = 0, add = TRUE, names.arg = c(0, 1, 2, 3, 4, 5, 6), 
        xlab = "VEI", ylab = "Proportion without lahars", main = NULL, mgp = c(2.0,0.8,0))

axis(3, at = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5), labels = c("n = 623", "n = 1175", "n = 3326", "n = 614", "n = 123", "n = 21", "n = 5"), 
     tick = FALSE, mgp = c(2, 0.0, 0))
axis(4, at = c(0.0,0.2,0.4,0.6,0.8,1.0), labels = c("1.0","0.8","0.6","0.4","0.2","0.0"))
mtext("Proportion with lahars", side = 4, cex = 1.0, line = 2)
dev.off()
