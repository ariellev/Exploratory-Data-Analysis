plot1 <- function(NEI) {
  require(dplyr)
  df <- NEI %>% group_by(year) %>% summarize(total_emission = sum(Emissions)) %>% mutate(total_emission = total_emission / 1000)
  png(filename="plot1.png")
  plot(df$year, df$total_emission, "o", xlab = "Year", ylab = "Emission (thousands of tons)", main="PM2.5 total emission USA")
  dev.off()
	message("plot1.png was created successfully")
}