plot3 <- function() {
  
  library(base)
  library(reshape2)
  library(ggplot2)
  
  ## Read the data file
  NEI <- readRDS("summarySCC_PM25.rds")
  
  ## Subset to Baltimore City, Maryland
  subset_data <- subset(NEI, fips == "24510")
  
  ## Melt and cast data to get total Emissions per year by type
  melted_data <- melt(subset_data, id=c("year","type"), measure = "Emissions")
  casted_data <- dcast(melted_data, year + type ~ variable, sum)
  
  ## Set type as factor for ggplot
  casted_data$type <- as.factor(casted_data$type)
  
  ## Create plot3.png
  png(filename="plot3.png",width=740,height=480)
  g <- ggplot(casted_data, aes(year,Emissions))
  p <- g + geom_point(aes(color=type),size = 4) + xlim(1997,2010) + xlab("Year") + ylab("Emissions (in tons)") + facet_grid(.~type) + geom_smooth(method="lm",se=FALSE,size=1.1) + ggtitle("Total Emissions in Baltimore City, Maryland by Type") + geom_text(aes(label=round(Emissions,0)),hjust=0.5, vjust=-0.5)
  print(p)
  dev.off()
  
}
