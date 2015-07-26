plot5 <- function() {
  
  library(base)
  library(reshape2)
  
  ## Read the data files
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  ## Grab mobile sourced SCC identifiers
  mobile_index <- SCC[grep("Onroad",SCC$Data.Category),"SCC"]
  
  ## Subset to Mobile Sourced Emissions
  subset_data <- NEI[which(NEI$SCC %in% as.vector(mobile_index)),]
  
  ## Subset to Baltimore City
  subset_data_md <- subset(subset_data, fips == "24510")
  
  ## Melt and cast data to get Emissions per year
  melted_data <- melt(subset_data_md, id="year", measure = "Emissions")
  casted_data <- dcast(melted_data, year ~ variable, sum)
  
  ## Create plot5.png
  png(filename="plot5.png",width=480,height=480)
  
  ## Create initial histogram of Emissions
  plot(casted_data$year, casted_data$Emissions, type = "h", col = "cyan", 
       ylab = "Emissions (in tons)", xlab = "Year", main = "Total Mobile Sourced Emissions in Baltimore City, Maryland", 
       lwd=20, xaxt='n', xlim=c(1997,2010), ylim = c(50,375))
  
  ## Add axis ticks
  axis(1, at = seq(1999, 2008, by = 3), las=1)
  
  ## Add the trendline
  fit <- lm(casted_data$Emissions ~ casted_data$year)
  abline(fit, col = "red", lwd = 2)
  
  ## Add values to each year for easier reading
  text(casted_data$year, casted_data$Emissions, round(casted_data$Emissions, 0), cex=1.5, col = "black")
  
  ## add legend to identify the trendline
  legend("topright", legend = "Trendline", lty=1, col="red", lwd = 2)
  
  dev.off()
  
}
