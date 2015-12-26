#Across the United States, how have emissions from coal 
#combustion-related sources changed from 1999-2008
plot4 <- function(){
  library(dplyr)
  library(ggplot2)
  unzip("exdata-data-NEI_data.zip")
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  coalSCCRed <- SCC %>% 
    filter(grepl('coal',Short.Name,ignore.case = T)|
             grepl('coal',EI.Sector,ignore.case = T)|
             grepl('coal',SCC.Level.Three,ignore.case = T)|
             grepl('coal',SCC.Level.Four,ignore.case = T))
  coalCombSCC <- coalSCCRed %>% 
    filter(grepl('comb',Short.Name,ignore.case = T)|
             grepl('comb',EI.Sector,ignore.case = T)|
             grepl('comb',SCC.Level.One,ignore.case = T)|
             grepl('comb',SCC.Level.Two,ignore.case = T)|
             grepl('comb',SCC.Level.Three,ignore.case = T)|
             grepl('comb',SCC.Level.Four,ignore.case = T))
  NEICoalComb <- NEI[NEI$SCC %in% coalCombSCC$SCC,]
  mergedData <- merge(NEICoalComb,coalCombSCC,by.x = 'SCC',by.y = 'SCC')
  groupedNEI <- mergedData %>% 
    group_by(year) %>%
    summarize(tem = sum (Emissions))
  png(file="plot4.png",bg="transparent",width = 480 ,height = 480)
  gplot <- ggplot(groupedNEI,aes(x=year,y=tem)) + 
    geom_line()  + 
    ggtitle("Total Emissions in USA for Coal Combustion Sources")+ 
    ylab("Tons Of Emissions") +
    xlab("Year")
  print(gplot)
  dev.off()
}