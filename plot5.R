
data_motor <- data_ssc[grepl("Vehicle", data_ssc$SCC.Level.Two), ]

# Calculate total emissions from motor vehicle sources in Baltimore City
motor_scc <- unique(data_motor$SCC)
motor_emi <- data[(data$SCC %in% motor_scc), ]
motor_year <- motor_emi %>%
  filter(fips == "24510") %>%
  group_by(year) %>%
  summarise(total = sum(Emissions))

# Plot total emissions from motor vehicle sources in Baltimore City
ggplot(motor_year, aes(factor(year), total, label = round(total))) +
  geom_bar(stat = "identity", fill = "lightpink") +
  ggtitle("Total Motor Vehicle Emissions in Baltimore City") +
  xlab("Year") +
  ylab("PM2.5 Emissions in Tonnes") +
  ylim(c(0, 450)) +
  theme_classic() +
  geom_text(size = 5, vjust = -1) +
  theme(plot.title = element_text(hjust = 0.5))
png("plot5.png")
dev.off()
