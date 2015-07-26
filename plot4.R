plot4 <- function() {
  
  library(base)
  library(reshape2)
  
  ## Read the data files
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  ## Grab coal SCC identifiers
  coal_index <- SCC[grep("Coal",SCC$EI.Sector),"SCC"]
  
  ## Subset to Coal sourced emissions
  subset_data <- NEI[which(NEI$SCC %in% as.vector(coal_index)),]
  
  ## Melt and cast data to get total Emissions per year
  melted_data <- melt(subset_data, id="year", measure = "Emissions")
  casted_data <- dcast(melted_data, year ~ variable, sum)
  
  ## Divide totals by 1000 for easier reading, now measured in thousands of tons
  casted_data$Emissions <- casted_data$Emissions/1000
  
  ## Create plot4.png
  png(filename="plot4.png",width=480,height=480)
  
  ## Create initial histogram of Emissions
  plot(casted_data$year, casted_data$Emissions, type = "h", col = "cyan", 
       ylab = "Emissions (in thousands of tons)", xlab = "Year", main = "Total Coal Related Emissions in the US", 
       lwd=20, xaxt='n', xlim=c(1997,2010), ylim = c(300,700))
  
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
