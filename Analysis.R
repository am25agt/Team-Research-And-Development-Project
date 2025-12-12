# 1. Load Dataset

df <- read.csv("chennai.csv", stringsAsFactors = FALSE)

# Print column names
print("Column Names:")
print(colnames(df))

# Print first 5 rows
print("First 5 Rows of the Dataset:")
print(head(df, 5))

# 2. Check Missing Values

print("Summary of Missing Values per Column:")
print(colSums(is.na(df)))

# 3. Data Cleaning

# Convert Price to numeric (remove symbols/commas)
df$Price <- as.numeric(gsub("[^0-9\\.]", "", df$Price))

# Convert Rating to numeric
df$Rating <- as.numeric(df$Rating)

# Remove rows with missing Price or Rating
df <- df[!is.na(df$Price) & !is.na(df$Rating), ]

# 4. Descriptive Statistics

desc_table <- data.frame(
  Variable = c("Price", "Rating"),
  Mean = c(mean(df$Price), mean(df$Rating)),
  Median = c(median(df$Price), median(df$Rating)),
  SD = c(sd(df$Price), sd(df$Rating)),
  N = c(length(df$Price), length(df$Rating))
)

print("Descriptive Statistics Table:")
print(desc_table)

# 5. Visualisations Required for Correlation RQ

# Histogram of Price + Normal Distribution Curve
hist(df$Price, breaks = 20, 
     main = "Histogram of Hotel Prices",
     xlab = "Price (INR)", prob = TRUE)

x <- seq(min(df$Price), max(df$Price), length = 100)
lines(x, dnorm(x, mean(df$Price), sd(df$Price)))

# Histogram of Ratings
hist(df$Rating, breaks = 10,
     main = "Histogram of Ratings",
     xlab = "Rating")

# Scatter Plot of Price vs Rating
plot(df$Price, df$Rating,
     pch = 19,
     main = "Scatter Plot: Price vs Rating",
     xlab = "Price (INR)", ylab = "Rating")
abline(lm(Rating ~ Price, data = df), col = "red")

# 6. Correlation Analysis

# Pearson correlation
pearson_test <- cor.test(df$Price, df$Rating, method = "pearson")
print("Pearson Correlation Test:")
print(pearson_test)

# Spearman correlation
spearman_test <- cor.test(df$Price, df$Rating, method = "spearman")
print("Spearman Correlation Test:")
print(spearman_test)

# 7. Regression Analysis

lm_model <- lm(Price ~ Rating, data = df)
print("Linear Regression Summary:")
print(summary(lm_model))

# Confidence intervals for regression
print("95% Confidence Intervals for Regression:")
print(confint(lm_model))

