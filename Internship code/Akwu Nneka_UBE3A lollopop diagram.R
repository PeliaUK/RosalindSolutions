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

###2
# Libraries
library(ggplot2)

# Create data
data <- data.frame(x=seq(1,30), y=abs(rnorm(30)))

# Plot
ggplot(data, aes(x=x, y=y)) +
  geom_point() + 
  geom_segment( aes(x=x, xend=x, y=0, yend=y))

###4
