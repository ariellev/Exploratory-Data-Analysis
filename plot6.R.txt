plot6 <- function(NEI, SCC) {
  require(dplyr)
  require(ggplot2)
  ## subsetting sources collecting motor emission in Baltimore and Los Angeles County
  scc_pool <- SCC %>% filter(Data.Category == "Onroad") %>% select(SCC)
  nei_df <- NEI %>% filter((fips == "24510" | fips == "06037") & SCC %in% scc_pool$SCC)
  
  ## summarizing emission by year. 
  ## Scaling down 3 exponents
  nei_df <- nei_df %>% group_by(fips, year) %>% summarize(total_emission = sum(Emissions)) %>% mutate(total_emission = total_emission / 1000)
  
  nei_df <- nei_df %>% mutate(area = ifelse(fips=="24510", "Baltimore City", "Los Angeles County"))
  ## actual plotting begins  
  pl <- qplot(year, total_emission, data = nei_df, xlab = "Year", ylab = "Emission (thousands of tons)", color = area, geom = "line") 
  pl <- pl + geom_point() + ggtitle(expression(atop("Motor Vehicla Emission " * PM[2.5], atop("Los Angeles County and Baltimore City 1998-2008")))) 
  pl <- pl + theme(axis.text.y = element_text(size=8))
  
  ggsave("plot6.png", pl, width=7, height=7, dpi=300)

	message("plot6.png was created successfully")
}
