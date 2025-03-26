
setwd("C:/Users/nneka/Documents/KGI_2nd Year/Internship 2024-KGI/R_Code")
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



# Define the real exon positions of the UBE3A gene (reverse strand)
gene_structure <- data.frame(
  exon_start = c(25438488, 25411907, 25409087, 25405460, 25375464, 25370565, 25360382, 25356690, 25355891, 25354527, 25354352, 25340084, 25333727),
  exon_end   = c(25439024, 25411971, 25409207, 25405502, 25375763, 25371812, 25360527, 25356896, 25356056, 25354683, 25354426, 25340228, 25339257),
  exon_label = paste0("Exon", 1:13),  # Labels for exons with vertical text
  exon_color = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                 "#D55E00", "#CC79A7", "#999999", "#E41A1C", "#377EB8", 
                 "#4DAF4A", "#984EA3", "#FF7F00")
)

# Define the 5' and 3' UTR regions for the reverse strand
utr_5_prime <- data.frame(start = 25333728, end = 25333727)  # 5' UTR (higher coordinates)
utr_3_prime <- data.frame(start = 25439024, end = 25439024)  # 3' UTR (lower coordinates)

# Define a continuous gene line that spans from the 3' UTR through exons to the 5' UTR
gene_continuous <- data.frame(
  x = c(utr_3_prime$start, gene_structure$exon_start, utr_5_prime$end),
  y = rep(-0.1, length(c(utr_3_prime$start, gene_structure$exon_start, utr_5_prime$end)))
)

# Calculate intron positions from gene_structure (between consecutive exons)
introns_df <- data.frame(
  intron = paste0("Intron ", 1:(nrow(gene_structure)-1)),
  start = gene_structure$exon_end[-nrow(gene_structure)],
  end = gene_structure$exon_start[-1]
)

# Variant positions (example positions within UBE3A gene)
specified_positions <- c(25339151, 25339185, 25339187, 25339187,25339188,25339189,25339195,25339195, 25339204, 25339207,25339216, 25339224,25340083,25340149, 25340149, 25340155, 25340160, 25340182, 25340197, 25340201, 25340219, 25354361, 25355984, 25356729,25356729, 25356842, 25356883, 25368455, 25370573, 25370598, 25370670, 25370697,25370745,25370765,25370851, 25370865,
                         25370975, 25371043, 25371108, 25371162, 25371276,25371404, 25371517,25371559,25371710,25371750,25371781,25371792,25371798, 25375654, 25375762)

# Define the vector of specified positions
specified_positions <- c(25339142,25339152,25339164, 25339164, 25339170, 25339181, 25339209, 25339248, 25339263, 
                         25339267,25339269,25339271,25339275,25340066,25340141,25340153,25340162,25340168,
                         25340195,25340213,25340233,25354337,25354339,25354341,25354343,25354346,25354355,
                         25354364,25354379,25354385,25354394,25354406,25354412,25354435,25354441,25354446,
                         25354510,25354511,25354511,25354513,25354540,25354561,25354561,25354603,25354639,
                         25354645,25354663,25354687,25354694,25354694,25354703,25355873,25355878,25355879,
                         25355882,25355882,25355884,25355961,25356000,25356003,25356015,25356033,25356051,
                         25356071,25356074,25356676,25356677,25356679,25356682,25356736,25356766,25356769,
                         25356774,25356790,25356796,25356814,25356820,25356826,25356841,25356850,25356880,
                         25356886,25356900,25360368,25360372,25360375,25360408,25360417,25360426,25360495,
                         25360513,25360516,25360536,25360537,25360538,25360544,25370575,25370578,25370608,
                         25370620,25370635,25370647,25370653,25370665,25370677,25370692,25370713,25370722,
                         25370746,25370773,25370776,25370791,25370824,25370827,25370836,25370839,25370869,
                         25370901,25370902,25370920,25370926,25370946,25370953,25370968,25370995,25371013,
                         25371013,25371040,25371067,25371088,25371110,25371115,25371121,25371136,25371142,
                         25371148,25371160,25371172,25371190,25371193,25371211,25371213,25371220,25371235,
                         25371247,25371247,25371301,25371304,25371307,25371313,25371316,25371316,25371346,
                         25371348,25371355,25371359,25371379,25371385,25371406,25371430,25371442,25371454,
                         25371469,25371475,25371478,25371490,25371514,25371520,25371523,25371550,25371556,
                         25371571,25371592,25371616,25371667,25371697,25371715,25371715,25371730,25371739,
                         25371763,25371768,25371772,25371784,25371787,25371822,25371822,25371822,25371825,
                         25375449,25375451,25375481,25375484,25375484,25375487,25375523,25375535,25375556,
                         25375604,25375616,25375637,25375650,25375667,25375667,25375670,25375673,25375685,
                         25375718,25375739,25375745,25375767,25375779,25405441,25405447,25405452,25405453,
                         25405454)

# Calculate the total number of variant positions (including duplicates)
total_count <- length(specified_positions)
cat("Total number of variant positions:", total_count, "\n")

# Optionally, calculate the number of unique variant positions
unique_count <- length(unique(specified_positions))
cat("Total number of unique variant positions:", unique_count, "\n")

# Create a data frame with variant positions and classifications
data_variants <- data.frame(
  x = specified_positions,
  y = abs(rnorm(length(specified_positions))),
  classification = sample(c("Pathogenic", "Likely Pathogenic", "Benign", "Likely Benign", "VUS"),
                          length(specified_positions), replace = TRUE)
)

# Define color mapping for variants
variant_colors <- c(
  "Pathogenic" = "red",
  "Likely Pathogenic" = "pink",
  "Benign" = "blue",
  "Likely Benign" = "purple",
  "VUS" = "black"
)

# Function to create a plot with gene structure, intron positions, and manual domain fill scale
create_plot_with_gene <- function(classification) {
  subset_data <- data_variants %>% filter(classification == classification)
  
  ggplot() +
    # Continuous gene line
    geom_line(data = gene_continuous, aes(x = x, y = y), color = "black", linewidth = 1) +
    
    # Intron positions as dashed blue segments
    geom_segment(data = introns_df,
                 aes(x = start, xend = end, y = -0.1, yend = -0.1),
                 color = "blue", linetype = "dashed", size = 0.8) +
    
    # 5' UTR rectangle
    geom_rect(data = utr_5_prime,
              aes(xmin = start, xmax = end, ymin = -0.2, ymax = 0),
              fill = "lightgray", color = "black", alpha = 1) +
    
    # 3' UTR rectangle
    geom_rect(data = utr_3_prime,
              aes(xmin = start, xmax = end, ymin = -0.2, ymax = 0),
              fill = "lightgray", color = "black", alpha = 1) +
    
    # Exons as rectangles with fill defined by the hex codes (identity scale)
    geom_rect(data = gene_structure,
              aes(xmin = exon_start, xmax = exon_end, ymin = -0.2, ymax = 0, fill = exon_color),
              color = "black", alpha = 1) +
    scale_fill_identity() +
    
    # Exon labels underneath (rotated vertically)
    geom_text(data = gene_structure,
              aes(x = (exon_start + exon_end) / 2, y = -0.4, label = exon_label),
              size = 2, fontface = "bold", color = "black", angle = 90) +
    
    # Variant lines and points (for the subset of variants of interest)
    geom_segment(data = subset_data,
                 aes(x = x, xend = x, y = -0.1, yend = y),
                 color = "black", linewidth = 0.8) +
    geom_point(data = subset_data,
               aes(x = x, y = y),
               size = 4, alpha = 0.8, color = variant_colors[classification]) +
    
    # UTR labels (with clear annotation)
    geom_text(aes(x = max(utr_5_prime$end) + 1000, y = 0.1, label = "5'"),
              size = 5, fontface = "bold", color = "black") +
    geom_text(aes(x = min(utr_3_prime$start) - 1000, y = 0.1, label = "3'"),
              size = 5, fontface = "bold", color = "black") +
    
    # Start a new fill scale so that domain fills are controlled separately
    ggnewscale::new_scale_fill() +
    
    # UBE3A protein domains as rectangles (aligned with the exons)
    geom_rect(data = domains,
              aes(xmin = start, xmax = end, ymin = -0.2, ymax = 0, fill = domain),
              color = "black", alpha = 1) +
    
    # Add the manual fill scale for the domains along with theme adjustments
    scale_fill_manual(values = c("AZUL" = "darkred", "HECT" = "lightgreen"),
                      guide = guide_legend(title = "Domains")) +
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
    
    labs(title = paste("UBE3A Gene Structure with Variants -", classification),
         x = "Genomic Position (Reverse Strand)", y = "")
}

# Generate and print plot for Pathogenic variants with UBE3A gene structure
pathogenic_plot <- create_plot_with_gene("Pathogenic")
print(pathogenic_plot)


#save the pathogenic plot
ggsave("pathogenic_plot.png", plot = pathogenic_plot, width = 8, height = 6, dpi = 300)

