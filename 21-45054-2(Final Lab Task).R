library(readxl)
library(dplyr)
library(VIM)  
library(stats)
library(writexl)

file_path <- "D:/Furniture.xlsx"
data <- read_excel(file_path)

cat("Missing Value Counts by Column:\n")
missing_counts <- colSums(is.na(data))
print(missing_counts)

data <- distinct(data)
cat("Number of rows after removing duplicates:", nrow(data), "\n")

data_original <- data 

if ("price" %in% names(data)) {
  Q1 <- quantile(data$price, 0.25, na.rm = TRUE)
  Q3 <- quantile(data$price, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  data <- data %>% filter(price >= lower_bound & price <= upper_bound)
}

write_xlsx(data, path = "D:/cleaned_Furniture.xlsx")

target_variable <- "revenue"  


numerical_columns <- names(Filter(is.numeric, data))
categorical_columns <- setdiff(names(data), c(numerical_columns, target_variable))


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