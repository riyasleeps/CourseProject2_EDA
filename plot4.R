
source_file <- "Source_Classification_Code.rds"
source_data <- readRDS(source_file)

# Select coal combustion-related sources using keywords from the Sector column
coal_data <- source_data[grepl("Comb.*Coal", source_data$Sector), ]

# Calculate total coal combustion-related emissions
coal_scc <- unique(coal_data$SCC)
coal_emissions <- data[data$SCC %in% coal_scc, ]
coal_yearly_totals <- coal_emissions %>%
  group_by(year) %>%
  summarise(total = sum(Emissions))

ggplot(coal_yearly_totals, aes(factor(year), total/1000, label = round(total/1000))) +
  geom_bar(stat = "identity", fill = "grey") +
  ggtitle("PM2.5 Coal Combustion Source Emissions Across US from 1999-2008") +
  xlab("Year") +
  ylab("Total PM2.5 Emissions (10^5 Tons)") +
  ylim(c(0, 620)) +
  theme_classic() +
  geom_text(size = 5, vjust = -1) +
  theme(plot.title = element_text(hjust = 0.5))
png("plot4.png")
dev.off()
