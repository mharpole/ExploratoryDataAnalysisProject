library(tidyverse)
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
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
  ggtitle("Comparision of vehicle emmisions in Los Angeles and Baltimore")+
  theme_minimal()
dev.copy(png,"plot6.png")
dev.off()
rm(list=ls())
