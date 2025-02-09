# Libraries
library(ggplot2)

# Create data
data <- data.frame(
  x=LETTERS[1:26],
  y=abs(rnorm(26))
)

# Horizontal version
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )
Baseline
###3
# Libraries
library(ggplot2)

# Create data
data <- data.frame(
  x=LETTERS[1:26],
  y=abs(rnorm(26))
)

# Change baseline
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=1, yend=y), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Value of Y")


###2
# Libraries
library(ggplot2)


library(bio3d)
library(rgl)

# Define the PDB file name
pdb_file <- "2kr1.pdb"

# Check if PDB file exists
if (!file.exists(pdb_file)) {
  stop(paste("Error: PDB file", pdb_file, "not found in the working directory!"))
}



# Read the PDB file
#pdb <- read.pdb(pdb_file)
pdb <- read.pdb("2kr1.pdb")
print(pdb)


#Define variant positions (Replace with actual SNPs or mutation sites)
# Example: Mutations at residues 100, 200, 300, 450 (modify based on your dataset)
variant_positions <- c(100, 200, 300, 450)



# Convert residue numbers to atomic indices
variant_indices <- which(pdb$atom$resno %in% variant_positions)

# Extract XYZ coordinates of the variants
xyz_coords <- pdb$xyz[variant_indices, ]


# Create data
data <- data.frame(x=seq(875), y=abs(rnorm(875)))

# Plot
ggplot(data, aes(x=x, y=y)) +
  geom_point(color="red", size=4, alpha=0.6) + 
  geom_segment( aes(x=x, xend=x, y=0, yend=y))

###4
# Libraries
library(ggplot2)

# Create data
data <- data.frame(
  x=LETTERS[1:26],
  y=abs(rnorm(26))
)

# Change baseline
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=1, yend=y), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Value of Y")
