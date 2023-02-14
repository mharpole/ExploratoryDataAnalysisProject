library(tidyverse)
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# Of the four types of sources indicated by the type type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
BaltimorYearlySummary <- NEI %>%
  filter(fips == "24510") %>% 
  group_by(year,type) %>% 
  summarise(totalPollution = sum(Emissions),.groups = "keep")
# head(BaltimorYearlySummary)

myplot <- BaltimorYearlySummary %>% ggplot(aes(factor(year),totalPollution))
myplot+
  geom_bar(stat="identity",aes(fill=type),position = "dodge")+
  facet_grid(1~type)+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab(label = "Year")+
  ggtitle("Pollution by type for Baltimore City")
dev.copy(png,"plot3.png")
dev.off()
rm(list=ls())
