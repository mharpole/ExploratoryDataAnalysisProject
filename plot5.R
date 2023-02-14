library(tidyverse)
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
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

dev.copy(png,"plot5.png")
dev.off()
rm(list=ls())
