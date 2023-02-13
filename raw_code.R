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
# Of the four types of sources indicated by the type type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
BaltimorYearlySummary <- NEI %>%
  filter(fips == "24510") %>% 
  group_by(year,type) %>% 
  summarise(totalPollution = sum(Emissions),.groups = "keep")
head(BaltimorYearlySummary)

myplot <- BaltimorYearlySummary %>% ggplot(aes(factor(year),totalPollution))
myplot+
  geom_bar(stat="identity",aes(fill=type),position = "dodge")+
  facet_grid(1~type)+
  theme_minimal()+
  xlab(label = "Year")
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
View(SCC)
coalCombustionSCC <- SCC %>% 
  filter(str_detect(EI.Sector,"Comb"), str_detect(EI.Sector,"Coal"))
coalData <-NEI %>% 
  filter(SCC %in% coalCombustionSCC$SCC) %>% 
  group_by(year) %>% 
  summarise(`Coal Combustion Emissions`= sum(Emissions))
coalplot <- coalData %>% ggplot(aes(factor(year),`Coal Combustion Emissions`))+
  geom_bar(stat="identity")+
  xlab("Year")+
  ggtitle("Total Emission from Coal Combustion")+
  theme_minimal()
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
carSCC <- SCC %>% filter(str_detect(EI.Sector,"Vehicle"))
baltimoreNEIVehicle <- NEI %>% 
  filter(fips == "24510", SCC %in% carSCC$SCC) %>% 
  group_by(year) %>% 
  summarise(
    `Total Pollution`=sum(Emissions)
  )
baltimoreNEIVehicle %>% 
  ggplot(aes(factor(year),`Total Pollution`)) +
  geom_bar(stat="identity")+
  xlab("Year")+
  ggtitle("Emissions from motor vehicle sources in Baltimore City")
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037". Which city has seen greater changes over time in motor vehicle emissions?
carSCC <- SCC %>% filter(str_detect(EI.Sector,"Vehicle"))
baltimoreVsLosAngelesCar <- NEI %>% 
  filter(fips == "24510" | fips == "06037", SCC %in% carSCC$SCC) %>% 
  mutate(city = case_when(
    fips == "24510"~ "Baltimore",
    fips == "06037"~ "Los Angeles"
  )) %>% 
  group_by(city,year) %>% 
  summarise(
    `Total Pollution`=sum(Emissions),
    .groups = "keep"
  )
baltimoreVsLosAngelesCar %>% 
  ggplot(aes(factor(year),`Total Pollution`,fill = city))+
  geom_bar(stat="identity",position = "dodge")+
  xlab("Year")+
  ggtitle("Comparision of vehicle emmisions in Los Angeles and Baltimore")
