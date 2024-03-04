# Calculate total PM2.5 emissions for Baltimore City per year
emi_balt <- data %>%
  group_by(year) %>%
  filter(fips == "24510") %>%
  summarise(total = sum(Emissions))

# Plot Baltimore City emissions per year
plot2 <- barplot(emi_balt$total,
                 main = "Total PM2.5 Emissions in Baltimore City, Maryland",
                 xlab = "Year",
                 ylab = "PM2.5 Emissions in Tons",
                 names.arg = emi_balt$year,
                 col = "grey",
                 ylim = c(0, 3600))
text(plot2,
     round(emi_balt$total),
     label = round(emi_balt$total),
     pos = 3,
     cex = 1.2)
png("plot2.png")
dev.off()
