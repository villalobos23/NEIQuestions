#Have total emissions from PM2.5 decreased in the United States
#from 1999 to 2008? Using the base plotting system, make a plot 
#showing the total PM2.5 emission from all sources for each of 
#the years 1999, 2002, 2005, and 2008.
plot1 <- function(){
  unzip("exdata-data-NEI_data.zip")
  NEI <- readRDS("summarySCC_PM25.rds")
  D1999 <- sum(NEI[NEI$year == 1999,"Emissions"])
  D2002 <- sum(NEI[NEI$year == 2002,"Emissions"])
  D2005 <- sum(NEI[NEI$year == 2005,"Emissions"])
  D2008 <- sum(NEI[NEI$year == 2008,"Emissions"])
  totalEM.df <- data.frame(TotEm=c(D1999,D2002,D2005,D2008),
                           year=c(1999,2002,2005,2008))
  png(file="plot1.png",bg="transparent",width = 480 ,height = 480)
  plot(y=totalEM.df$TotEm/1000000,x=totalEM.df$year,
       type = "l", xlab = "Year",
       ylab="Total Emissions (Millions of Tons)",main="Total Emissions per Year")
  dev.off()
}