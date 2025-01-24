library(dplyr)
library(ggplot2)
library(tidyr)
library(ROSE)    
library(VIM)     
library(readxl)
file_path <- "D:/Filtered_dataset.csv"  # Replace with your file path
df <- read.csv(file_path)

cat("Dataset Columns:\n")
print(names(df))
str(df)

cat("Missing Value Counts by Column:\n")
missing_counts <- colSums(is.na(df))
print(missing_counts)

if (any(missing_counts > 0)) {
  # Create a custom bar plot for missing values
  barplot(
    missing_counts[missing_counts > 0],
    main = "Missing Values in Each Column",
    xlab = "Columns", 
    ylab = "Number of Missing Values",
    col = "darkorange",       # Change color of bars
    border = "white",         # Remove border for cleaner look
    ylim = c(0, max(missing_counts) + 5), # Adjust y-axis limit to add space
    las = 2,                  # Rotate x-axis labels for better readability
    cex.names = 0.8,          # Change size of x-axis labels
    cex.axis = 0.8            # Change size of y-axis labels
  )
  
  # Add additional plot with 'VIM' aggr function for missing data pattern
  aggr(df, 
       numbers = TRUE, 
       prop = FALSE, 
       cex.axis = 0.7, 
       combined = TRUE, 
       sortVars = TRUE, 
       gap = 2, 
       ylab = c("Missing Data Pattern", "Proportion"))
} else {
  cat("No missing values.\n")
}


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

df_original <- df

for (col_name in names(df)) {
  if (is.numeric(df[[col_name]])) {
    df[[col_name]][is.na(df[[col_name]])] <- median(df[[col_name]], na.rm = TRUE)
  } else {
    df[[col_name]][is.na(df[[col_name]])] <- Mode(df[[col_name]])
  }
}

cat("Missing Value Counts After Imputation:\n")
print(colSums(is.na(df)))

df <- distinct(df)
cat("Number of rows after removing duplicates:", nrow(df), "\n")


if ("Age" %in% names(df)) {
  df <- df %>% filter(Age > 0 & Age <= 120)
}

if ("Academic.Pressure" %in% names(df)) {
  Q1 <- quantile(df$Academic.Pressure, 0.25, na.rm = TRUE)
  Q3 <- quantile(df$Academic.Pressure, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  
  df <- df %>% filter(Academic.Pressure >= lower_bound & Academic.Pressure <= upper_bound)
}

if ("Depression" %in% names(df) && "Age" %in% names(df)) {
  filtered_data <- df %>% filter(Depression == "Yes" & Age > 30)
  write.csv(filtered_data, "filtered_data.csv", row.names = FALSE)
}

categorical_columns <- c("Gender", "Sleep.Duration", "Dietary.Habits", 
                         "Family.History.of.Mental.Illness", "Depression", 
                         "Have.you.ever.had.suicidal.thoughts.")

for (col_name in categorical_columns) {
  if (col_name %in% names(df)) {
    df[[col_name]] <- as.factor(df[[col_name]])
  }
}

df <- df %>% mutate(across(where(is.character), ~as.numeric(as.factor(.))))


if ("Age" %in% names(df)) {
  df$AgeGroup <- cut(df$Age, 
                     breaks = c(0, 18, 30, 45, 60, Inf), 
                     labels = c("0-18", "19-30", "31-45", "46-60", "60+"),
                     right = FALSE)
}

if ("Study.Hours" %in% names(df)) {
  df$Normalized_Study_Hours <- (df$Study.Hours - min(df$Study.Hours, na.rm = TRUE)) / 
    (max(df$Study.Hours, na.rm = TRUE) - min(df$Study.Hours, na.rm = TRUE))
}

if ("Depression" %in% names(df)) {
  cat("Class Distribution Before Balancing:\n")
  print(table(df$Depression))
  
  df_balanced <- ROSE(Depression ~ ., data = df, seed = 1)$data
  
  cat("Class Distribution After Balancing:\n")
  print(table(df_balanced$Depression))
}

descriptive_stats <- summary(df)
write.csv(df, "D:/Filtered_dataset.csv", row.names = FALSE)
write.csv(descriptive_stats, "D:/Filtered_dataset.csv")

getwd()

dir.exists("D:/")


