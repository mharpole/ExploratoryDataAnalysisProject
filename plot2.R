library(tidyverse)
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
BaltimorYearlySummary <- NEI %>%
  filter(fips == "24510") %>% 
  group_by(year) %>% 
  summarise(totalPollution = sum(Emissions))
head(BaltimorYearlySummary)
barplot(height =BaltimorYearlySummary$totalPollution,
        names=BaltimorYearlySummary$year,
        xlab="Year",
        ylab="Total Emissions",
        main="Total emissions in the Baltimore City, Maryland from 1999 to 2008")
dev.copy(png,"plot2.png")
dev.off()
rm(list=ls())
