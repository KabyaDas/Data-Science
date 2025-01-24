library(readxl)
library(ggplot2)
library(dplyr)
library(moments) 


data <- read_excel("D:/Furniture.xlsx")


target_variable <- "revenue"


numerical_columns <- names(Filter(is.numeric, data))
categorical_columns <- setdiff(names(data), c(numerical_columns, target_variable))


mean_price <- mean(data$price, na.rm = TRUE)
median_price <- median(data$price, na.rm = TRUE)
mode_price <- as.numeric(names(sort(table(data$price), decreasing = TRUE))[1])

cat("Univariate Analysis of 'price':\n")
cat("Mean of price:", mean_price, "\n")
cat("Median of price:", median_price, "\n")
cat("Mode of price:", mode_price, "\n\n")


mode_color <- names(sort(table(data$color), decreasing = TRUE))[1]
cat("Mode of 'color':", mode_color, "\n\n")


ggplot(data, aes(x = price)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Price", x = "Price", y = "Frequency") +
  theme_minimal()


ggplot(data, aes(x = price)) +
  geom_density(color = "blue", size = 1) +
  labs(title = "Line Histogram of Price", x = "Price", y = "Density") +
  theme_minimal()


skewness_value <- skewness(data$price, na.rm = TRUE)
cat("Skewness of price:", skewness_value, "\n")
ggplot(data, aes(x = price)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  geom_vline(aes(xintercept = mean_price), color = "red", linetype = "dashed", size = 1) +
  labs(title = paste("Histogram of Price (Skewness:", round(skewness_value, 2), ")"),
       x = "Price", y = "Frequency") +
  theme_minimal()


ggplot(data, aes(x = color, y = price)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Price by Color", x = "Color", y = "Price") +
  theme_minimal()


pearson_results <- list()
spearman_results <- list()
anova_results <- list()


for (col in numerical_columns) {
  if (col != target_variable) {
    pearson_results[[col]] <- cor(data[[col]], data[[target_variable]], method = "pearson", use = "complete.obs")
    spearman_results[[col]] <- cor(data[[col]], data[[target_variable]], method = "spearman", use = "complete.obs")
  }
}


for (col in categorical_columns) {
  anova_model <- aov(data[[target_variable]] ~ data[[col]], data = data)
  anova_results[[col]] <- summary(anova_model)[[1]]["Pr(>F)"][1]
}


cat("Pearson Correlation Results:\n")
print(pearson_results)

cat("\nSpearman Correlation Results:\n")
print(spearman_results)

cat("\nANOVA Results (p-values):\n")
print(anova_results)


for (col in numerical_columns) {
  if (col != target_variable) {
    plot <- ggplot(data, aes_string(x = col, y = target_variable)) +
      geom_point(color = "blue") +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      labs(title = paste("Scatter Plot:", col, "vs", target_variable),
           x = col, y = target_variable) +
      theme_minimal()
    print(plot)
    ggsave(filename = paste0("scatter_", col, ".png"), plot = plot)
  }
}


for (col in categorical_columns) {
  plot <- ggplot(data, aes_string(x = col, y = target_variable)) +
    geom_violin(fill = "skyblue") +
    geom_boxplot(width = 0.1, color = "black", fill = "white") +
    labs(title = paste("Violin Plot:", col, "vs", target_variable),
         x = col, y = target_variable) +
    theme_minimal()
  print(plot)
  ggsave(filename = paste0("violin_", col, ".png"), plot = plot)
}


for (col in numerical_columns) {
  if (col != target_variable) {
    plot <- ggplot(data, aes_string(x = col, y = target_variable)) +
      geom_line(color = "blue") +
      labs(title = paste("Line Graph:", col, "vs", target_variable),
           x = col, y = target_variable) +
      theme_minimal()
    print(plot)
    ggsave(filename = paste0("line_", col, ".png"), plot = plot)
  }
}
