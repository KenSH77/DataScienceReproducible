library(ggplot2)
library(dplyr)

stormRawData <- read.csv("./StormData.csv", header=TRUE)
processData <- select(stormRawData, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP )
# clean up EVTTYPE
evtTypeList <- c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood", "Cold/Wind Chill", "Debris Flow", "Dense Fog", "Dense Smoke", "Drought", "Dust Devil", "Dust Storm", "Excessive Heat", "Extreme Cold/Wind Chill", "Flash Flood", "Flood", "Frost/Freeze", "Funnel Cloud", "Freezing Fog", "Hail", "Heat", "Heavy Rain", "Heavy Snow", "High Surf", "High Wind", "Hurricane (Typhoon)", "Ice Storm", "Lake-Effect Snow", "Lakeshore Flood", "Lightning", "Marine Hail", "Marine High Wind", "Marine Strong Wind", "Marine Thunderstorm Wind", "Rip Current", "Seiche", "Sleet", "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind", "Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", "Waterspout", "Wildfire", "Winter Storm", "Winter Weather")
processData <- filter(processData, EVTYPE %in% toupper(evtTypeList))
processData[,1] <- sapply(processData[,1],function(row) iconv(row, "latin1", "ASCII", sub=""))

# filter out missing Values
compData1 <- complete.cases(processData[,1], processData[,2], processData[,3])
compData2 <- complete.cases(processData[,1], processData[,4], processData[,5], processData[,6], processData[,7])

# clean up EVTTYPE
processData1 <- processData[compData1,]
processData2 <- processData[compData2,]
grpResult1 <- group_by(processData1, EVTYPE)

# summarize the results for health costs
result1 <- summarise(grpResult1, sum(as.numeric(FATALITIES)) + sum(as.numeric(INJURIES)))
colnames(result1) <- c("EVTYPE", "SUMNUM")

# make the order
result1 <- result1[order(-result1$SUMNUM),]
result1 <- slice(result1, 1:5)
head(result1)
# draw graphs
ggplot(result1, aes(EVTYPE, SUMNUM)) + geom_bar(stat = "identity", fill = "blue")
maxname1 <- filter(result1, SUMNUM==max(result1[,2]))[,1]

# for question 2
processData2 <- mutate(processData2, ecoCost=0)

processData21 <- filter(processData2, toupper(processData2$PROPDMGEXP)=="K")
processData22 <- filter(processData2, toupper(processData2$PROPDMGEXP)=="M")
processData23 <- filter(processData2, toupper(processData2$PROPDMGEXP)=="B")
processData24 <- filter(processData2, toupper(processData2$CROPDMGEXP)=="K")
processData25 <- filter(processData2, toupper(processData2$CROPDMGEXP)=="M")
processData26 <- filter(processData2, toupper(processData2$CROPDMGEXP)=="B")
processData21[,8] <- processData21[,4]*1000
processData22[,8] <- processData22[,4]*1000000
processData23[,8] <- processData23[,4]*1000000000
processData24[,8] <- processData24[,6]*1000
processData25[,8] <- processData25[,6]*1000000
processData26[,8] <- processData26[,6]*1000000000

processDataC2 <- rbind(processData21, processData22, processData23, processData24,  processData25,processData26)
grpResult2 <- group_by(processDataC2, EVTYPE)
result2 <- summarise(grpResult2, sum(ecoCost) )
colnames(result2) <- c("EVTYPE", "SUMECOCOST")
#reorder result2
result2 <- result2[order(-result2$SUMECOCOST),]
result2 <- slice(result2, 1:5)
ggplot(result2, aes(EVTYPE, SUMECOCOST)) + geom_bar(stat = "identity", fill = "blue")

head(processData2)
