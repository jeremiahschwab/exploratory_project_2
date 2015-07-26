plot6 <- function() {
  
  library(base)
  library(reshape2)
  library(ggplot2)
  
  ## Read the data files
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  ## Grab mobile sourced SCC identifiers
  mobile_index <- SCC[grep("Onroad",SCC$Data.Category),"SCC"]
  
  ## Subset to Mobile Sourced Emissions
  subset_data <- NEI[which(NEI$SCC %in% as.vector(mobile_index)),]
  
  ##Subset to Baltimore/LA
  subset_data_md <- subset(subset_data, fips == "24510")
  subset_data_la <- subset(subset_data, fips == "06037")
  subset_data_md$Location <- "Baltimore City, MD"
  subset_data_la$Location <- "Los Angeles County, CA"
  combined_data <- rbind(subset_data_md,subset_data_la)
  
  ## Melt and cast data to get Emissions per year by Location
  melted_data <- melt(combined_data, id=c("year","Location"), measure = "Emissions")
  casted_data <- dcast(melted_data, year + Location ~ variable, sum)
  
  ## Set Location as factor for ggplot
  casted_data$Location <- as.factor(casted_data$Location)
  
  ## Create plot6.png
  png(filename="plot6.png",width=740,height=480)
  g <- ggplot(casted_data, aes(year,Emissions))
  p <- g + geom_point(aes(color=Location),size = 4) + xlim(1997,2010) + xlab("Year") + ylab("Emissions (in tons)") + facet_grid(.~Location) + geom_smooth(method="lm",se=FALSE,size=1.1) + ggtitle("Total Mobile Sourced Emissions by Location") + geom_text(aes(label=round(Emissions,0)),hjust=0.5, vjust=-0.5)
  print(p)
  dev.off()
  
}
