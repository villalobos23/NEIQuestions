#Compare emissions from motor vehicle sources in 
#Baltimore City  (fips == "24510") with emissions from motor vehicle 
#sources in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?
plot6 <- function(){
  library(dplyr)
  library(ggplot2)
  unzip("exdata-data-NEI_data.zip")
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  motorSCC <- SCC %>% filter(grepl('motor',Short.Name,ignore.case = T)|
                               grepl('motor',EI.Sector,ignore.case = T)|
                               grepl('motor',SCC.Level.One,ignore.case = T)|
                               grepl('motor',SCC.Level.Two,ignore.case = T)|
                               grepl('motor',SCC.Level.Three,ignore.case = T)|
                               grepl('motor',SCC.Level.Four,ignore.case = T))
  NEIMotor <- NEI[NEI$SCC %in% motorSCC$SCC,]
  NEIMotor <- merge(NEIMotor,motorSCC,by.x = "SCC",by.y = "SCC")
  groupedNEI <- NEIMotor %>%
    filter(fips=='24510'|fips=='06037') %>%
    group_by(year,fips) %>%
    summarize(tem = sum(tem = sum (Emissions)))
  png(file="plot6.png",bg="transparent",width = 480 ,height = 480)
  gplot <- ggplot()+
    geom_line(data=groupedNEI[groupedNEI$fips=='06037',],
              aes(x=year,y=tem,color=fips))+
    geom_line(data = groupedNEI[groupedNEI$fips=='24510',],
              aes(x=year,y=tem,color=fips))+
    xlab("Year")+
    ylab("Tons of Emissions")+
    ggtitle("Motor related Emissions Comparison Los Angeles vs Baltimore")
  print(gplot)
  dev.off()
}