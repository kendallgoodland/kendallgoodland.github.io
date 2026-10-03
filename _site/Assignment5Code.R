# Custom Histogram
set.seed(123)
data_hist <- rnorm(100)

par(mar = c(5, 5, 4, 2), col.axis = "blue", col.lab = "blue", font.axis = 2, font.lab = 2)
hist(data_hist, 
     breaks = 12, 
     col = "red", 
     main = "Customized Histogram", 
     xlab = "Values", 
     ylab = "Frequency", 
     border = "darkblue")

# Custom Vertical Barchart
data_bar <- c(15, 30, 25, 10, 20)
names(data_bar) <- c("A", "B", "C", "D", "E")

par(mar = c(5, 5, 4, 2), col.lab = "darkred", font.axis = 3)
barplot(data_bar, 
        col = c("tomato", "steelblue", "gold", "darkgreen", "purple"), 
        main = "Vertical Bar Chart", 
        xlab = "Categories", 
        ylab = "Values", 
        border = NA)

# Custom Horizontal Barchart
par(mar = c(5, 5, 4, 2), col.axis = "darkgreen", font.lab = 2)
barplot(data_bar, 
        horiz = TRUE, 
        col = c("cyan", "pink", "orange", "lightgreen", "gray"), 
        main = "Horizontal Bar Chart", 
        xlab = "Values", 
        ylab = "Categories", 
        border = NA)

# Custom Piechart
data_pie <- c(10, 20, 30, 40)
labels <- c("Q1", "Q2", "Q3", "Q4")

par(mar = c(1, 1, 1, 1))
pie(data_pie, 
    labels = labels, 
    col = c("red", "green", "blue", "yellow"), 
    main = "Customized Pie Chart", 
    clockwise = TRUE, 
    border = "black")

# Custom Boxplot
data_box <- list(Group1 = rnorm(20, mean = 5, sd = 2), Group2 = rnorm(20, mean = 7, sd = 1))

par(mar = c(5, 5, 4, 2), col.lab = "purple", col.axis = "purple", font.lab = 2)
boxplot(data_box, 
        col = c("lightblue", "pink"), 
        main = "Customized Boxplot", 
        xlab = "Groups", 
        ylab = "Values", 
        notch = TRUE)

# Custom Scatterplot
set.seed(123)
x_scatter <- rnorm(50)
y_scatter <- 2 * x_scatter + rnorm(50)

par(mar = c(5, 5, 4, 2), col.axis = "darkblue", col.lab = "darkblue", font.lab = 2)
plot(x_scatter, y_scatter, 
     col = "darkorange", 
     pch = 16, 
     main = "Customized Scatterplot", 
     xlab = "X Values", 
     ylab = "Y Values", 
     cex = 1.2)
abline(lm(y_scatter ~ x_scatter), col = "blue", lwd = 2)

# Load packages
library(ggplot2)

# Generate sample data
set.seed(123)
data_hist <- data.frame(values = rnorm(100))

# Customized Histogram with ggplot2
ggplot(data_hist, aes(x = values)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "darkblue", size = 0.7) +
  theme_minimal(base_family = "serif") +  # Using serif font
  theme(
    plot.title = element_text(color = "darkblue", size = 16, face = "bold"),
    axis.title = element_text(color = "blue", size = 14),
    axis.text = element_text(color = "blue", size = 12)
  ) +
  labs(
    title = "Customized Histogram (ggplot2)",
    x = "Values",
    y = "Frequency"
  ) +
  geom_vline(aes(xintercept = mean(values)), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean(data_hist$values) + 0.5, y = 8, label = "Mean", color = "red")


