#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
#which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City?
#Which have seen increases in emissions from 1999-2008? 
#Use the ggplot2 plotting system to make a plot answer this question
plot3 <- function(){
  library(dplyr)
  library(ggplot2)
  unzip("exdata-data-NEI_data.zip")
  NEI <- readRDS("summarySCC_PM25.rds")
  groupedNEI <- NEI %>% 
    group_by(year,type) %>%
    filter(fips == "24510") %>% 
    summarize(tem = sum(Emissions))
  png(file="plot3.png",bg="transparent",width = 480 ,height = 480)
  gplot <- ggplot(groupedNEI,aes(x=year,y=tem)) + 
    geom_line() +
    facet_grid(type ~ .) + 
    ggtitle("Total Emissions in Baltimore by type per Year")+ 
    ylab("Tons Of Emissions") +
    xlab("Year")
  print(gplot)
  dev.off()
}