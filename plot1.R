library(tidyverse)
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
head(NEI)
NEIYearlySummary <- NEI %>% 
  group_by(year) %>% 
  summarise(totalPollution=sum(Emissions))
head(NEIYearlySummary)
barplot(height=NEIYearlySummary$totalPollution,names =NEIYearlySummary$year,ylab = "Total Pollution",xlab = "Year", main = "Total PM2.5 emmsions from all sources")
dev.copy(png,"plot1.png")
dev.off()
rm(list=ls())
