# some analysis of storms
setwd("~/GitHub/reproducible-research")

storm_data <- read.csv("StormData.csv.bz2")



levels(storm_data$PROPDMGEXP) <- c(levels(storm_data$PROPDMGEXP), c(1,10,100,1000,10000,100000, 1000000,10000000,100000000, 1000000000))
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == 'M'] <- 1000000
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == 'm'] <- 1000000
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == 'B'] <- 1000000000
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == 'b'] <- 1000000000
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == 'h'] <- 100
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == 'H'] <- 100
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == 'k'] <- 1000
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == 'K'] <- 1000
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == '0'] <- 1
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == '1'] <- 10
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == '2'] <- 100
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == '3'] <- 1000
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == '4'] <- 10000
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == '5'] <- 100000
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == '6'] <- 1000000
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == '7'] <- 10000000
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == '8'] <- 100000000

storm_data$PROPDMGEXP <-  as.numeric(as.character(storm_data$PROPDMGEXP))
storm_data$PROPDMG <- (storm_data$PROPDMG) * (storm_data$PROPDMGEXP)

levels(storm_data$CROPDMGEXP) <- c(levels(storm_data$CROPDMGEXP), c(1,10,100,1000,10000,100000, 1000000,10000000,100000000, 1000000000))
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == 'M'] <- 1000000
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == 'm'] <- 1000000
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == 'B'] <- 1000000000
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == 'b'] <- 1000000000
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == 'h'] <- 100
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == 'H'] <- 100
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == 'k'] <- 1000
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == 'K'] <- 1000
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == '0'] <- 1

storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == '2'] <- 100


storm_data$CROPDMGEXP1 <-  as.numeric(as.character(storm_data$CROPDMGEXP))
storm_data$CROPDMG1 <- (storm_data$CROPDMG) * (storm_data$CROPDMGEXP1)

storm_data$TOTALDMG <- rowSums( cbind(storm_data$PROPDMG,storm_data$CROPDMG1), na.rm=TRUE)

damage <- aggregate( TOTALDMG~EVTYPE,data = storm_data, sum)

significant_damage <- subset(damage, TOTALDMG> 0)
significant_damage <- significant_damage[order(-significant_damage$TOTALDMG),]
top_damage <- significant_damage[1:5,]

t <- ggplot(top_damage, aes(reorder(EVTYPE, -TOTALDMG), TOTALDMG/1000000000)) +
    geom_bar(stat="identity")+
    
    labs(x="Type", y="Total Damage (billions)")+
    labs(title="Damage Caused by Storms")
print(t)


########injury
storm_data$TOTALINJURY <- rowSums(cbind(storm_data$FATALITIES,storm_data$INJURIES), na.rm=TRUE)
deaths_injury <- aggregate( TOTALINJURY~EVTYPE,data = storm_data, sum)
significant_injuries <- subset(deaths_injury, TOTALINJURY> 0)
significant_injuries <- significant_injuries[order(-significant_injuries$TOTALINJURY),]
top_injuries <- significant_injuries[1:5,]

t <- ggplot(top_injuries, aes(reorder(EVTYPE, -TOTALINJURY), TOTALINJURY))+
    geom_bar(stat="identity")+
    
    labs(x="Type", y="Total Fatalities and Injury")+
    labs(title="Deaths and Injuries caused by Storms")
print(t)