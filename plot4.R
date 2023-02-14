library(tidyverse)
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
# View(SCC)
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
coalplot
dev.copy(png,"plot4.png")
dev.off()
rm(list=ls())