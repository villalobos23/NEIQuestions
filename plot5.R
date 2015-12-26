#How have emissions from motor vehicle sources changed from 1999-2008 
#in Baltimore City? fips == 24510
plot5 <- function(){
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
    filter(fips=='24510') %>%
    group_by(year) %>%
    summarize(tem = sum(tem = sum (Emissions)))
  png(file="plot5.png",bg="transparent",width = 480 ,height = 480)
  gplot <- ggplot(groupedNEI,aes(x=year,y=tem)) + 
    geom_line()  + 
    ggtitle("Total Emissions in Baltimore from motor vehicle Sources")+ 
    ylab("Tons Of Emissions") +
    xlab("Year")
  print(gplot)
  dev.off()  
}