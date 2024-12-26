# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(

# Load the dataset
file_path <- "31276.csv"  # Replace with your file path
data <- read.csv(file_path)

# Inspect the structure of the dataset
cat("\n--- Structure of the Dataset ---\n")
str(data)

# Display dataset dimensions
cat("\n--- Dataset Dimensions ---\n")
cat("Rows:", nrow(data), "\nColumns:", ncol(data), "\n")

# Display the first few rows
cat("\n--- First 5 Rows ---\n")
print(head(data))

# Summary statistics for numerical variables
cat("\n--- Summary of Numerical Variables ---\n")
print(summary(data))

# Identify quantitative and qualitative variables
quantitative_vars <- names(data)[sapply(data, is.numeric)]
qualitative_vars <- names(data)[sapply(data, is.character)]

cat("\n--- Variable Identification ---\n")
cat("Quantitative Variables:\n", paste(quantitative_vars, collapse = ", "), "\n")
cat("Qualitative Variables:\n", paste(qualitative_vars, collapse = ", "), "\n")

# Descriptive Statistics for Quantitative Variables
cat("\n--- Descriptive Statistics ---\n")
for (col in quantitative_vars) {
  cat("\nStatistics for", col, ":\n")
  cat("Mean:", mean(data[[col]], na.rm = TRUE), "\n")
  cat("Median:", median(data[[col]], na.rm = TRUE), "\n")
  cat("Mode:", names(sort(table(data[[col]]), decreasing = TRUE))[1], "\n")
  cat("Standard Deviation:", sd(data[[col]], na.rm = TRUE), "\n")
  cat("Variance:", var(data[[col]], na.rm = TRUE), "\n")
  cat("Minimum:", min(data[[col]], na.rm = TRUE), "\n")
  cat("Maximum:", max(data[[col]], na.rm = TRUE), "\n")
  cat("Range:", max(data[[col]], na.rm = TRUE) - min(data[[col]], na.rm = TRUE), "\n")
  cat("IQR:", IQR(data[[col]], na.rm = TRUE), "\n")
  cat("Five-number Summary:", fivenum(data[[col]]), "\n")
  cat("Six-number Summary:", c(fivenum(data[[col]]), mean(data[[col]], na.rm = TRUE)), "\n")
  
  # Histogram
  p <- ggplot(data, aes_string(x = col)) +
    geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histogram of", col), x = col, y = "Frequency") +
    theme_minimal()
  ggsave(paste0("Histogram_of_", col, ".png"), plot = p)
}

# Outlier Analysis
cat("\n--- Outlier Analysis ---\n")
for (col in quantitative_vars) {
  Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outliers <- data[[col]][data[[col]] < lower_bound | data[[col]] > upper_bound]
  cat("\nOutliers in", col, ":\n")
  print(outliers)
  
  # Boxplot
  p <- ggplot(data, aes_string(y = col)) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(title = paste("Boxplot of", col), y = col) +
    theme_minimal()
  ggsave(paste0("Boxplot_of_", col, ".png"), plot = p)
}

# Covariance and Correlation Analysis
cat("\n--- Covariance and Correlation ---\n")
cov_matrix <- cov(data[quantitative_vars], use = "complete.obs")
cat("\nCovariance Matrix:\n")
print(cov_matrix)

cor_matrix <- cor(data[quantitative_vars], use = "complete.obs")
cat("\nCorrelation Matrix:\n")
print(cor_matrix)

# Correlation Heatmap
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
ggsave("Correlation_Heatmap.png")

# Scatterplot Matrix
pairs(data[quantitative_vars], main = "Scatterplot Matrix")
ggsave("Scatterplot_Matrix.png")

# Frequency Analysis for Qualitative Variables
cat("\n--- Frequency Analysis for Qualitative Variables ---\n")
for (col in qualitative_vars) {
  freq_table <- table(data[[col]])
  rel_freq_table <- prop.table(freq_table)
  
  cat("\nFrequency Table for", col, ":\n")
  print(freq_table)
  
  cat("\nRelative Frequency Table for", col, ":\n")
  print(rel_freq_table)
  
  # Bar chart
  p <- ggplot(data, aes_string(x = col)) +
    geom_bar(fill = "lightblue", color = "black", alpha = 0.7) +
    labs(title = paste("Bar Chart of", col), x = col, y = "Frequency") +
    theme_minimal()
  ggsave(paste0("Bar_Chart_of_", col, ".png"), plot = p)
  
  # Pie chart
  png(paste0("Pie_Chart_of_", col, ".png"))
  pie(freq_table, labels = names(freq_table), main = paste("Pie Chart of", col), col = rainbow(length(freq_table)))
  dev.off()
}
