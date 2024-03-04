# Calculate total PM2.5 emissions for each type and year in Baltimore City
emi_balt_t <- data %>%
  group_by(type, year) %>%
  filter(fips == "24510") %>%
  summarise(total = sum(Emissions))

# Plot Baltimore City emissions by type for each year
ggplot(emi_balt_t, aes(x = factor(year),
                       y = total, fill = type, label = round(total))) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ type) +
  ggtitle("Total PM2.5 Emissions in Baltimore City, Maryland") +
  xlab("Year") +
  ylab("PM2.5 Emissions in Tons") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
png("plot3.png")
dev.off()
