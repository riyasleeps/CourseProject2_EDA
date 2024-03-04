file_name <- "summarySCC_PM25.rds"
data <- readRDS(file_name)

# Calculate total PM2.5 emissions for each year
emi_year <- data %>%
  group_by(year) %>%
  summarise(total = sum(Emissions))
plot1 <- barplot(emi_year$total/1000,
                 main = "Total PM2.5 Emissions",
                 xlab = "Year",
                 ylab = "PM2.5 Emissions in Kilotons",
                 names.arg = emi_year$year,
                 col = "grey",
                 ylim = c(0, 8300))
text(plot1, round(emi_year$total/1000),
     label = round(emi_year$total/1000),
     pos = 3, cex = 1.2)
png("plot1.png")
dev.off()
