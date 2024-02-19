#This dataset contains the information about the nutrition level of 80 cereal products manufactured
##by different manufacturers(mfr: Manufacturer of cereal)

library(readxl)
getwd()
# Load the data from Excel file
data <- read_excel("C:/Users/TANNU/Documents/80cereal.xlsx")
data
str(data)
# Summary statistics
summary(data)
# Check for missing values
sum(is.na(data))

library(ggplot2)
# The number of cereals by manufacturer
#mfr Key
"A = American Home Food Products;
G = General Mills
K = Kelloggs
N = Nabisco
P = Post
Q = Quaker Oats
R = Ralston Purina"

library(dplyr)
install.packages("plotly")
library(plotly)

# Manufacturer distribution
manufacturer_distribution <- data %>%
  group_by(mfr) %>%
  summarize(count = n())

plot_ly(labels = manufacturer_distribution$mfr, values = manufacturer_distribution$count, type = "pie")

#Share of each mfr
manufacturer_share <- table(data$mfr)
manufacturer_share
# Create a data frame for plotting
share_data <- data.frame(manufacturer = names(manufacturer_share),
                         share = manufacturer_share)
share = manufacturer_share

ggplot(data, aes(x = mfr, fill = mfr)) +
  geom_bar() +
  labs(title = "Number of Cereals by Manufacturer", x = "Manufacturer", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Kellogs manufactures the largest number of cereal products followed by General Mills
# American Home Food Products manufactures the least cereal products

# Cereal Distribution by Manufacturer and Type
ggplot(data, aes(x = mfr, fill = type)) +
  geom_bar() +
  labs(title = "Cereal Distribution by Manufacturer and Type", x = "Manufacturer", y = "Count")
#American Home Food Products produce cereals of hot type only while Nabisco and Quaker Oats manufacture
##small part of their products of hot type. Other manufacturer only produce cold type cereals

# Shelf distribution
ggplot(data, aes(x = shelf, fill = factor(shelf))) +
  geom_bar() +
  labs(title = "Cereal Distribution by Shelf", x = "Shelf", y = "Count")
#most of the cereal products have been kept in shelf 3 followed by 2 then 1.

View(data)
#MFR wrt carbo, calories, rating
# Carbo with respect to mfr
ggplot(data, aes(x =mfr, y = carbo, fill = mfr)) +
  geom_bar(stat = "identity") +
  labs(title = "Carbohydrate Content by Manufacturer",
       x = "Cereal Products",
       y = "Carbohydrate",
       fill = "manufacturers") +
  theme_minimal()

# Calories with respect to mfr
ggplot(data, aes(x = mfr, y = calories, fill = mfr)) +
  geom_bar(stat = "identity") +
  labs(title = "Calories Content by Manufacturer",
       x = "Manufacturer",
       y = "Calories",
       fill = "Manufacturer") +
  theme_minimal()

ggplot(data, aes(x = calories, y = rating,col=rating)) +
  geom_line() +
  labs(title = "Calories vs. Rating")
"cereals with high ratings have low calories which is actually a good feature of the cereal products."

# Carbo with respect to mfr
ggplot(data, aes(x = mfr, y = rating, fill = mfr)) +
  geom_bar(stat = "identity") +
  labs(title = "Rating of Manufacturer",
       x = "Manufacturer",
       y = "rating",
       fill = "Manufacturer") +
  theme_minimal()
#Kellogs has the highest ratings followed by General Mills then Nabisco

# Box plot for calories, protein, and fat
library(ggplot2)
ggplot(data, aes(x = type, y = calories, fill = type)) +
  geom_boxplot() +
  labs(title = "Distribution of Calories by Cereal Type", x = "Cereal Type", y = "Calories")

ggplot(data, aes(x = type, y = data$protein, fill = type)) +
  geom_boxplot() +
  labs(title = "Distribution of proteins by Cereal Type", x = "Cereal Type", y = "Proteins")

ggplot(data, aes(x = type, y = fat, fill = type)) +
  geom_boxplot() +
  labs(title = "Distribution of fats by Cereal Type", x = "Cereal Type", y = "fats")

# Correlation heatmap
library(corrplot)
correlation_matrix <- cor(data[, c("calories", "protein", "fat", "sodium", "fiber", "carbo", "sugars", 
                                   "potass", "vitamins", "rating")])
correlation_matrix
corrplot(correlation_matrix, method = "number",tl.col="blue")
#Potass and fiber are highly correlated.

library(cluster)
cereal_features <- select(data, -name, -mfr, -type)

# Performing clustering using k-means algorithm
set.seed(123) # For reproducibility
kmeans_model <- kmeans(cereal_features, centers = 3) # 3 clusters for demonstration

# Adding cluster labels to the dataset
data$cluster <- as.factor(kmeans_model$cluster)

# Visualizing clusters using PCA (Principal Component Analysis)
pca_result <- prcomp(cereal_features, scale. = TRUE)
plot(pca_result$x[,1:2], col = data$cluster, pch = 16, main = "PCA Plot of Cereal Clusters")

cereal_data <- mutate_if(cereal_data, is.character, as.factor)
cereal_data <- mutate_if(cereal_data, is.factor, as.numeric)
# Building a linear regression model to estimate ratings
lm_model <- lm(rating ~ ., data = cereal_data[, !(names(cereal_data) %in% c("name", "mfr", "type", "cluster"))])
lm_model
summary(lm_model)
#All coefficients have very small p-values (<2e-16), indicating that they are statistically significant.
"The negative coefficients for vitamins, sodium & potass suggest that higher levels of vitamins and these minerals
in the cereal are associated with lower ratings, holding all other variables constant. 
negative coefficients for calories and sugars suggest that higher levels of sugars in the cereal is not
favourable and they are associated with lower ratings, holding all other variables constant.
However, it's important to note that this interpretation is based solely on the statistical analysis and 
does not imply a causal relationship. 
Other factors not included in the model could also influence the relationship between these variables.
Overall, this summary indicates that all the independent variables(calories, protein, fat, sodium, fiber, 
carbo, sugars, potass, vitamins) are statistically significant predictors of the cereal rating in this model."

# Correlation plot of variables
corr_matrix <- cor(cereal_features)
corrplot(corr_matrix, method = "circle", type = "upper", tl.col = "black")
"ratings has perfect negative correlation with calories amd sugars due to obvious reasons."

# Identify influential factors on ratings
# Extract coefficients and their significance levels
coefficients <- coef(lm_model)
significance <- summary(lm_model)$coefficients[, "Pr(>|t|)"]

# Combine coefficients and significance into a data frame
coefficients_df <- data.frame(variable = names(coefficients), coefficient = coefficients, significance = significance)
coefficients_df
# Order by coefficient magnitude
coefficients_df <- coefficients_df[order(abs(coefficients_df$coefficient), decreasing = TRUE), ]

# Print the most influential factors
print(coefficients_df)

# Visualize residuals
plot(lm_model, which = 1)

install.packages("car")
library(car)
# VIF (Variance Inflation Factor)
vif(lm_model)
#calories, fiber & potass have moderate multicollinearity with other variables
#VIF values below 5 are generally considered acceptable.

