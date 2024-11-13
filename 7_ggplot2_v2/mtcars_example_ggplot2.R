library(ggplot2)
library(scales)

# data
mtcars <- mtcars

# Plot 1
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl), size = hp)) +
  geom_jitter(width = .1, height = .1) +
  labs(title = 'Jitter plot of weight VS MPG', 
       caption = 'Points size represents horsepower, color represents cylinder count', 
       x = 'Weight (1000 lbs)', 
       y = 'Miles per gallon (MPG)', 
       color = 'Cylinders', 
       size = 'Horsepower') +
  theme_bw() +
  theme(plot.title = element_text(face = 'bold', size = 15), 
           plot.caption = element_text(face = 'italic', size = 12), 
           legend.position = 'bottom') 


# Plot 2 
ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, color = "black", size = 1.5, alpha = 0.5) +
  labs(
    title = "Boxplot of MPG by Cylinder Count",
    x = "Cylinder Count",
    y = "Miles per Gallon (MPG)"
  ) +
  scale_fill_manual(values = c('darkblue', 'blue', 'skyblue')) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


# Plot 3
ggplot(mtcars, aes(x = mpg, fill = ..count..)) +
  geom_histogram(binwidth = 2, color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Miles per Gallon (MPG)",
    x = "Miles per Gallon (MPG)",
    y = "Count"
  ) +
  scale_fill_gradient(low = "red", high = "navy") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(size = 12)
  )


# Plot 4 (advanced)

# Scaling horsepower to align with MPG for visual comparison
mtcars$hp_scaled <- rescale(mtcars$hp, to = range(mtcars$mpg))

mtcars$italic <- rename()

ggplot(mtcars, aes(x = wt)) +
  geom_line(aes(y = mpg, color = "Miles per Gallon"), linewidth = 1) +
  geom_line(aes(y = hp_scaled, color = "Horsepower (Scaled)"), linewidth = 1, linetype = "dashed") +
  scale_y_continuous(
    name = "Miles per Gallon (MPG)",
    sec.axis = sec_axis(~ . * diff(range(mtcars$hp)) / diff(range(mtcars$mpg)) + 
                          min(mtcars$hp), name = "Horsepower")
  ) +
  labs(
    title = "Comparison of MPG and Horsepower by Car Weight",
    x = "Weight (1000 lbs)"
  ) +
  scale_color_manual(values = c("Miles per Gallon" = "blue", "Horsepower (Scaled)" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.y.left = element_text(color = "blue", face = 'bold'),
    axis.title.y.right = element_text(color = "red", face = 'italic'),
    axis.text.x = element_text(face = 'italic'),
    legend.position = "bottom"
  )

# Plot 5 (advanced)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(aes(color = factor(gear)), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dotted") +
  facet_wrap(~ cyl, ncol = 2) +
  labs(
    title = "MPG vs. Weight Faceted by Cylinder Count",
    subtitle = "Each panel shows a linear regression line with different gears colored",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon (MPG)"
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "grey"),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) +
  # Adding annotations to each facet for median MPG
  geom_text(
    data = aggregate(mpg ~ cyl, data = mtcars, median),
    aes(x = 5, y = mpg, label = paste("Median MPG:", round(mpg, 1))),
    color = "darkblue",
    size = 4,
    fontface = "italic",
    hjust = 1,
    vjust = 0.5
  )
