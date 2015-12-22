#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510")
#from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.
plot2 <- function(){
  library(dplyr)
  unzip("exdata-data-NEI_data.zip")
  NEI <- readRDS("summarySCC_PM25.rds")
  groupedNEI <- NEI %>% 
    group_by(year) %>% 
    filter(fips == "24510") %>% 
    summarize(tem = sum(Emissions))
  png(file="plot2.png",bg="transparent",width = 480 ,height = 480)
  plot(x=groupedNEI$year,
       y=groupedNEI$tem,
       type = "l",
       xlab = "Year",
       ylab = "Tons of emmissions",
       main = "Baltimore Total emissions per year")
  dev.off()
}