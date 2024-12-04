## Data Visualization
## Objective: Identify data or model problems using visualization
## Anscombe (1973) Quartlet

data(anscombe)  # Load Anscombe's data
View(anscombe) # View the data
summary(anscombe)

## Simple version
plot(anscombe$x1,anscombe$y1)
summary(anscombe)

# Create four model objects
lm1 <- lm(y1 ~ x1, data=anscombe)
summary(lm1)
lm2 <- lm(y2 ~ x2, data=anscombe)
summary(lm2)
lm3 <- lm(y3 ~ x3, data=anscombe)
summary(lm3)
lm4 <- lm(y4 ~ x4, data=anscombe)
summary(lm4)
plot(anscombe$x1,anscombe$y1)
abline(coefficients(lm1))
plot(anscombe$x2,anscombe$y2)
abline(coefficients(lm2))
plot(anscombe$x3,anscombe$y3)
abline(coefficients(lm3))
plot(anscombe$x4,anscombe$y4)
abline(coefficients(lm4))


## Fancy version (per help file)

ff <- y ~ x
mods <- setNames(as.list(1:4), paste0("lm", 1:4))

# Plot using for loop
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  ## or   ff[[2]] <- as.name(paste0("y", i))
  ##      ff[[3]] <- as.name(paste0("x", i))
  mods[[i]] <- lmi <- lm(ff, data = anscombe)
  print(anova(lmi))
}

sapply(mods, coef)  # Note the use of this function
lapply(mods, function(fm) coef(summary(fm)))

# Preparing for the plots
op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))

# Plot charts using for loop
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2,
       xlim = c(3, 19), ylim = c(3, 13))
  abline(mods[[i]], col = "blue")
}
mtext("Anscombe's 4 Regression data sets", outer = TRUE, cex = 1.5)
par(op)

# Fine-tuned Anscombe's Quartet Plot

# Create custom plotting settings:
op <- par(
  mfrow = c(2, 2),  # 2 rows, 2 columns layout
  mar = 0.1 + c(4, 4, 1, 1),  # Margins: bottom, left, top, right
  oma = c(0, 0, 2, 0),  # Outer margins
  family = "serif"  # Set font to serif
)

# Custom colors and plotting character
custom_colors <- c("#FF6F61", "#6B5B95", "#88B04B", "#F7CAC9")  # Non-default colors
custom_pch <- c(15, 17, 18, 19)  # Custom plotting characters

# Plot each dataset using a loop
for (i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y", "x"), i), as.name)
  
  plot(
    ff, 
    data = anscombe, 
    col = custom_colors[i],  # Use custom color
    pch = custom_pch[i],  # Use custom plotting character
    bg = adjustcolor(custom_colors[i], alpha.f = 0.5),  # Background color for points
    cex = 1.5,  # Increase point size
    xlim = c(3, 19), ylim = c(3, 13),  # Keep consistent axis limits
    main = paste("Dataset", i),  # Add individual plot titles
    xlab = "X values", 
    ylab = "Y values"
  )
  
  abline(mods[[i]], col = "blue", lwd = 2, lty = 2)  # Add regression line with blue color, thicker, and dashed
}

# Add a main title to the overall plot
mtext("Fine-Tuned Anscombe's Quartet Regression Plots", outer = TRUE, cex = 1.5, font = 2)

# Reset plot settings to default
par(op)

# Load packages
library(ggplot2)
library(tidyr)
library(dplyr)

# Reshape Anscombe's data into tidy format
anscombe_tidy <- anscombe %>%
  pivot_longer(cols = everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)") %>%
  mutate(set = factor(set, labels = paste("Dataset", 1:4)))

# Create the ggplot
ggplot(anscombe_tidy, aes(x = x, y = y, color = set)) +
  geom_point(aes(shape = set), size = 3, stroke = 1.5) +  # Custom shapes and sizes
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1) +  # Dashed regression lines
  facet_wrap(~ set, nrow = 2) +  # Split by dataset
  scale_color_manual(values = c("#FF6F61", "#6B5B95", "#88B04B", "#F7CAC9")) +  # Custom colors
  scale_shape_manual(values = c(15, 17, 18, 19)) +  # Custom shapes
  theme_minimal(base_family = "serif") +  # Set serif font with minimal theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  ) +
  labs(
    title = "Fine-Tuned Anscombe's Quartet Regression Plots",
    x = "X values",
    y = "Y values"
  )


