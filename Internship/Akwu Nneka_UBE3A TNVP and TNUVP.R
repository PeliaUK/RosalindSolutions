# Set working directory
setwd("C:/Users/nneka/Documents/KGI_2nd Year/Internship 2024-KGI/FINI")

# Data for TNVP and TNUVP
tnvp <- c(Pathogenic = 148, `Likely pathogenic` = 39, Benign = 71, `Likely Benign` = 277)
tnuvp <- c(Pathogenic = 84, `Likely pathogenic` = 34, Benign = 36, `Likely Benign` = 224)

# Calculate percentages and create labels for TNVP
tnvp_percent <- round(100 * tnvp / sum(tnvp), 1)
tnvp_labels <- paste(names(tnvp), "\n", tnvp, "\n", tnvp_percent, "%", sep = " ")

# Calculate percentages and create labels for TNUVP
tnuvp_percent <- round(100 * tnuvp / sum(tnuvp), 1)
tnuvp_labels <- paste(names(tnuvp), "\n", tnuvp, "\n", tnuvp_percent, "%", sep = " ")

# Save the TNVP pie chart to a PNG file
png("TNVP_pie_chart.png", width = 600, height = 600)
pie(tnvp, labels = tnvp_labels, main = "TNVP: Total Variant Positions", col = rainbow(length(tnvp)))
dev.off()  # Close the graphics device

# Save the TNUVP pie chart to a PNG file
png("TNUVP_pie_chart.png", width = 600, height = 600)
pie(tnuvp, labels = tnuvp_labels, main = "TNUVP: Total Unique Variant Positions", col = rainbow(length(tnuvp)))
dev.off()  # Close the graphics device