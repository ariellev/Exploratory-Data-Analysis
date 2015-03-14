plot3 <- function(NEI) {
  require(dplyr)
  require(ggplot2)
  Blt <- NEI %>% filter(fips == "24510") %>% group_by(year, type) %>% summarize(total_emission = sum(Emissions)) %>% mutate(total_emission = total_emission / 1000)
  png(filename="plot3.png")
  Blt_gplot <- qplot(year, total_emission, data = Blt, xlab = "Year", ylab = "Emission (thousands of tons)", color = type, geom = "line") 
  Blt_gplot <- Blt_gplot + geom_point() + ggtitle(expression(atop("Total Emission " * PM[2.5], atop("Baltimore City, Maryland"))))  
  print(Blt_gplot)
  dev.off()
	message("plot3.png was created successfully")
}
