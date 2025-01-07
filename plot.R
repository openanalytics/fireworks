library(ggplot2)
library(gganimate)
library(dplyr)  



# Define the colors
firework_colors <- c("#E52323", "#32A6D3", "#E6E6E6")

# Generate data
fireworks_data <- data.frame(
  x = rep(0, 1000),  # Increase the number of points
  y = rep(0, 1000),
  group = sample(1:30, 1000, replace = TRUE),  # More groups
  color = sample(firework_colors, 1000, replace = TRUE),  # Use only the specified colors
  angle = runif(1000, 0, 2 * pi),  # Random angles for bursts
  speed = runif(1000, 0.5, 2),     # Random speeds for bursts
  size = runif(1000, 1, 4),        # Random sizes for the points
  alpha = runif(1000, 0.5, 1)      # Random transparency (alpha)
)

# Create frames and simulate the explosion
fireworks_data <- fireworks_data %>%
  mutate(
    frame = rep(1:30, length.out = nrow(fireworks_data)),  # Ensure correct number of frames
    x = x + speed * frame * cos(angle),
    y = y + speed * frame * sin(angle)
  )

# Check for any missing or non-finite values in important columns
fireworks_data <- fireworks_data %>%
  filter(is.finite(x) & is.finite(y) & is.finite(frame))

# Check if the frame variable is finite and valid
if (any(!is.finite(fireworks_data$frame))) {
  stop("Frame values must be finite!")
}

# Create the animation
fireworks_animation <- ggplot(fireworks_data, aes(x = x, y = y, color = color, size = size, alpha = alpha, group = group)) +
  geom_point() +
  coord_fixed() +
  theme_void() +
  scale_color_identity() +
  scale_size_continuous(range = c(1, 5)) +  # Adjust the range of sizes
  scale_alpha_continuous(range = c(0.5, 1)) +  # Adjust the range of alpha values
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.grid = element_blank()
  ) +
  transition_time(frame) +  # Animate based on the frame variable
  ease_aes('linear')        # Smooth transitions

# Save and view the animation
animate(fireworks_animation, nframes = 40, fps = 20, width = 600, height = 600, renderer = gifski_renderer())
anim_save("fireworks.gif")
