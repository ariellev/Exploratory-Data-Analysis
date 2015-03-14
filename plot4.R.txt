plot4 <- function(NEI, SCC) {
  require(dplyr)
  require(ggplot2)
  ## subsetting sources collecting coal combustion emission   
  scc_pool <- SCC %>% filter(grepl(".*Coal.*",EI.Sector) & grepl(".*Comb.*",EI.Sector)) %>% select(SCC)
  nei_df <- NEI %>% filter(SCC %in% scc_pool$SCC)
  
  ## Extracting state fips from the regional fips codes by subsetting first two digits
  ## Later state fips will be converted to state code, e.g "06" to CA
  ## source: http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt
  nei_df <- nei_df %>% mutate(fips = as.numeric(substr(fips,start = 0,stop = 2)))
  
  ## summarizing emission by year and fips. 
  ## Scaling down 3 exponents
  nei_df <- nei_df %>% group_by(fips, year) %>% summarize(total_emission = sum(Emissions)) %>% mutate(total_emission = total_emission / 1000)
  
  ## Converting fips to state codes
  if (!file.exists("states.txt")) {
    download.file(url="http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", destfile="states.txt", method="curl")
  }
  
  fips <- read.csv("states.txt", header=F)
  fips_df <- data.frame(fips)
  fips_df <- fips_df %>% select(V1,V2) %>% distinct %>% mutate(State = V1, fips = V2)
  
  joined <- inner_join(x=nei_df,y=fips_df)
  
  ## actual plotting begins  
  pl <- qplot(year, total_emission, data = joined, xlab = "Year", ylab = "Emission (thousands of tons)", color = State, geom = "line") 
  pl <- pl + geom_point() + ggtitle(expression(atop("Coal Combustion Emission " * PM[2.5], atop("USA States 1998-2008")))) 
  pl <- pl + facet_wrap(~State) + scale_color_discrete(guide = FALSE) + theme(axis.text.x = element_text(angle=45, size=8))
  
  ggsave("plot4.png", pl, width=10, height=10, dpi=300)

	message("plot4.png was created successfully")
}
