plot5 <- function(NEI, SCC) {
  require(dplyr)
  require(ggplot2)
  ## subsetting sources collecting motor emission in Baltimore
  scc_pool <- SCC %>% filter(Data.Category == "Onroad") %>% select(SCC)
  nei_df <- NEI %>% filter(fips == "24510" & SCC %in% scc_pool$SCC)
  
  ## summarizing emission by year. 
  ## Scaling down 3 exponents
  nei_df <- nei_df %>% group_by(year) %>% summarize(total_emission = sum(Emissions)) %>% mutate(total_emission = total_emission / 1000)
  
  ## actual plotting begins  
  pl <- qplot(year, total_emission, data = nei_df, xlab = "Year", ylab = "Emission (thousands of tons)", geom = "line") 
  pl <- pl + geom_point() + ggtitle(expression(atop("Motor Vehicla Emission " * PM[2.5], atop("Baltimore City, Maryland 1998-2008")))) 
  pl <- pl + theme(axis.text.y = element_text(size=8))
  
  ggsave("plot5.png", pl, width=5, height=5, dpi=300)

	message("plot5.png was created successfully")
}
