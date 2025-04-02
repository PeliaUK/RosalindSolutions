setwd("C:/Users/nneka/Documents/KGI_2nd Year/Internship 2024-KGI/FINI")

library(ggplot2)
library(dplyr)
library(ggrepel)
library(ggnewscale)  # Allows multiple fill scales in one plot

set.seed(123)  # For reproducibility

# Define UBE3A isoform 1 protein domains
domains <- tribble(
  ~domain, ~start, ~end,
  "AZUL", 25375586, 25375705,
  "HECT", 25339140, 25354390
)

# Define the exon positions of the UBE3A gene (reverse strand, exons 1 to 13)
gene_structure <- data.frame(
  exon_start = c(25438488, 25411907, 25409087, 25405460, 25375464, 25370565, 25360382, 25356690, 25355891, 25354527, 25354352, 25340084, 25333727),
  exon_end   = c(25439024, 25411971, 25409207, 25405502, 25375763, 25371812, 25360527, 25356896, 25356056, 25354683, 25354426, 25340228, 25339257),
  exon_label = paste0("Exon", 1:13),
  exon_color = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                 "#D55E00", "#CC79A7", "#999999", "#E41A1C", "#377EB8", 
                 "#4DAF4A", "#984EA3", "#FF7F00")
)

# For a reverse strand gene the 3' UTR is at the higher coordinate and 5' UTR at the lower.
# Here we define a small width for visualization.
utr_3_prime <- data.frame(start = 25439024, end = 25439124)  # 3' UTR
utr_5_prime <- data.frame(start = 25333727, end = 25333827)  # 5' UTR

# Create a continuous gene line from the 3' UTR through the exons to the 5' UTR
gene_continuous <- data.frame(
  x = c(utr_3_prime$start, gene_structure$exon_start, utr_5_prime$end),
  y = rep(-0.1, length(c(utr_3_prime$start, gene_structure$exon_start, utr_5_prime$end)))
)

# Calculate intron positions (between consecutive exons)
introns_df <- data.frame(
  intron = paste0("Intron ", 1:(nrow(gene_structure)-1)),
  start = gene_structure$exon_end[-nrow(gene_structure)],
  end = gene_structure$exon_start[-1]
)

# Variant positions (example positions within UBE3A gene)
specified_positions <- c(25339151, 25339185, 25339187,25339188,25339189,25339195,25339195, 25339204, 25339207,
                         25339216, 25339224, 25340149, 25340149, 25340155, 25340160, 25340182, 25340197, 25340201,
                         25340219, 25354361, 25355984, 25356729,25356729, 25356842, 25356883, 25368455, 25370573,
                         25370598, 25370670, 25370697,25370745, 25370851, 25370865, 25370975, 25371043, 25371108,
                         25371162, 25371276,25371404, 25371517,25371559,25371710, 25371750,25371781,25371792,
                         25371798, 25375654, 25375762, 25339187,25339147,25354684,25370920, 25339197, 25339187,
                         25370765, 25339189, 25370736, 25371792, 25339185, 25370670, 25340149, 25340083, 25356040,
                         25339188, 25370697, 25370920, 25339197, 25339187, 25370765, 25339189, 25370736, 25371792,
                         25339185, 25370670, 25340149, 25340083, 25356040, 25339188, 25370697,25371797, 25340219,
                         25405461, 25370967, 25371548, 25339187, 25370751, 25371750, 25371235, 25371135, 25371000,
                         25370765, 25370628, 25370634, 25356859, 25356859, 25356846, 25355975, 25355967, 25355903,
                         25356061, 25354566, 25340208, 25339205, 25339189, 25339187, 25356777, 25375547, 25340173,
                         25371678, 25371639, 25371639, 25375759, 25375554, 25370765, 25370734, 25370670, 25339187,
                         25356047, 25354536, 25370598, 25371798, 25371750, 25371235, 25371135, 25371000, 25370765,
                         25370697, 25356859, 25356859, 25356859, 25356846, 25339189, 25339187, 25354566, 25340208,
                         25339205, 25356061, 25355975, 25355967, 25355903, 25405459, 25375759, 25370670, 25356750,
                         25354684, 25339189, 25339187, 25339185)

# Create a data frame for variants with random y values and classifications
data_variants <- data.frame(
  x = specified_positions,
  y = abs(rnorm(length(specified_positions))),
  classification = sample(c("Pathogenic", "Likely Pathogenic", "Benign", "Likely Benign", "VUS"),
                          length(specified_positions), replace = TRUE)
)

# Define color mapping for variant classifications
variant_colors <- c(
  "Pathogenic" = "red",
  "Likely Pathogenic" = "pink",
  "Benign" = "blue",
  "Likely Benign" = "purple",
  "VUS" = "black"
)

# Function to create a plot for a given variant classification
create_plot_with_gene <- function(classification) {
  subset_data <- data_variants %>% filter(classification == classification)
  
  ggplot() +
    # Draw continuous gene line (from 3' to 5')
    geom_line(data = gene_continuous, aes(x = x, y = y), color = "black", linewidth = 1) +
    
    # Plot intron segments as dashed blue lines
    geom_segment(data = introns_df,
                 aes(x = start, xend = end, y = -0.1, yend = -0.1),
                 color = "blue", linetype = "dashed", size = 0.8) +
    
    # Plot UTR regions as rectangles
    geom_rect(data = utr_3_prime,
              aes(xmin = start, xmax = end, ymin = -0.2, ymax = 0),
              fill = "lightgray", color = "black", alpha = 1) +
    geom_rect(data = utr_5_prime,
              aes(xmin = start, xmax = end, ymin = -0.2, ymax = 0),
              fill = "lightgray", color = "black", alpha = 1) +
    
    # Plot exons as rectangles using their assigned colors
    geom_rect(data = gene_structure,
              aes(xmin = exon_start, xmax = exon_end, ymin = -0.2, ymax = 0, fill = exon_color),
              color = "black", alpha = 1) +
    scale_fill_identity() +
    
    # Add exon labels (rotated vertically)
    geom_text(data = gene_structure,
              aes(x = (exon_start + exon_end) / 2, y = -0.4, label = exon_label),
              size = 2, fontface = "bold", color = "black", angle = 90) +
    
    # Add variant segments and points for the chosen classification
    geom_segment(data = subset_data,
                 aes(x = x, xend = x, y = -0.1, yend = y),
                 color = "black", linewidth = 0.8) +
    geom_point(data = subset_data,
               aes(x = x, y = y),
               size = 4, alpha = 0.8, color = variant_colors[classification]) +
    
    # Annotate the UTRs: with scale_x_reverse(), the highest coordinate appears on the left.
    # So we place the "3'" label (high coordinate) on the left and the "5'" label (low coordinate) on the right.
    geom_text(aes(x = utr_3_prime$start + 1000, y = 0.1, label = "3'"),
              size = 5, fontface = "bold", color = "black") +
    geom_text(aes(x = utr_5_prime$end - 1000, y = 0.1, label = "5'"),
              size = 5, fontface = "bold", color = "black") +
    
    # Start a new fill scale for protein domains
    ggnewscale::new_scale_fill() +
    geom_rect(data = domains,
              aes(xmin = start, xmax = end, ymin = -0.2, ymax = 0, fill = domain),
              color = "black", alpha = 1) +
    scale_fill_manual(values = c("AZUL" = "darkred", "HECT" = "lightgreen"),
                      guide = guide_legend(title = "Domains")) +
    
    # Reverse the x-axis so that the plot goes from 3' (left) to 5' (right)
    scale_x_reverse() +
    
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(),
      legend.position = "bottom",
      axis.title.y = element_blank(),
      panel.grid = element_blank()
    ) +
    
    labs(title = bquote(italic(UBE3A) ~ "[chr15:25,333,728-25,439,024] Gene Structure (3' to 5') with Variants - " ~ .(classification)),
         x = "Genomic Position (3' to 5')",
         y = "")
}

# Generate and display the plot for Pathogenic variants
pathogenic_plot_2 <- create_plot_with_gene("Pathogenic")
print(pathogenic_plot_2)

# Save the plot to file
ggsave("pathogenic_plot_2.png", plot = pathogenic_plot_2, width = 8, height = 6, dpi = 300)

# Print variant counts
total_count <- length(specified_positions)
cat("Total number of variant positions:", total_count, "\n")
unique_count <- length(unique(specified_positions))
cat("Total number of unique variant positions:", unique_count, "\n")
