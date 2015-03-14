plot2 <- function(NEI) {
  require(dplyr)
  df <- NEI %>% filter(fips == "24510") %>% group_by(year) %>% summarize(total_emission = sum(Emissions)) %>% mutate(total_emission = total_emission / 1000)
  png(filename="plot2.png")
  plot(df$year, df$total_emission, "o", xlab = "Year", ylab = "Emission (thousands of tons)", main="PM2.5 total emission Baltimore City, Maryland")
  dev.off()
	message("plot2.png was created successfully")
}
