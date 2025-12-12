
df <- read.csv("chennai.csv", stringsAsFactors = FALSE)

print("Column Names:")
print(colnames(df))

print("First 5 Rows of the Dataset:")
print(head(df, 5))

print("Summary of Missing Values per Column:")
print(colSums(is.na(df)))

df$Price <- as.numeric(gsub("[^0-9\\.]", "", df$Price))

df$Rating <- as.numeric(df$Rating)

df <- df[!is.na(df$Price) & !is.na(df$Rating), ]

print("Dataset After Removing Missing Values:")
print(head(df, 5))

print("Remaining Missing Values After Cleaning:")
print(colSums(is.na(df)))
