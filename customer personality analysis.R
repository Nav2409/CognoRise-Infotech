#Loading the dataset
library(readxl)
setwd("C:/Users/TANNU/Downloads")
my_data<-read_excel("C:/Users/TANNU/Downloads/marketing_campaign.xlsx")
my_data

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(car)

# Data Cleaning

# Convert Dt_Customer to Date type for time-based analysis
my_data$Dt_Customer <- as.Date(my_data$Dt_Customer)
my_data
# Impute missing values in the Income column with the median
my_data$Income[is.na(my_data$Income)] <- median(my_data$Income, na.rm = TRUE)

# Calculate Age from Year_Birth for demographic analysis
my_data$Age <- as.numeric(format(Sys.Date(), "%Y")) - my_data$Year_Birth
View(my_data)

# Data Exploration and Visualization

# Age Distribution of Customers
ggplot(my_data, aes(x = Age)) +
  geom_histogram(bins = 30, fill = "pink", color = "black") +
  theme_minimal() +
  ggtitle("Age Distribution of Customers") +
  xlab("Age") +
  ylab("Frequency")

# The histogram shows a diverse age range of customers with a focus on the 40-60 age group.

# Income Distribution of Customers
ggplot(my_data, aes(x = Income)) +
  geom_histogram(bins = 30, fill = "green", color = "black") +
  theme_minimal() +
  ggtitle("Income Distribution of Customers") +
  xlab("Income") +
  ylab("Frequency")

#Income is right-skewed, indicating most customers are in the moderate income range.

# Education Level of Customers
ggplot(my_data) +
  geom_bar(aes(x = reorder(Education, Education, function(x)-length(x))), 
           fill = "orange") +
  theme_minimal() +
  ggtitle("Education Level of Customers") +
  xlab("Education Level") +
  ylab("Count") +
  coord_flip()

# Most customers are well-educated, with 'Graduation' being the most common level of education.

# Marital Status of Customers
ggplot(my_data) +
  geom_bar(aes(x = reorder(Marital_Status, Marital_Status, function(x)-length(x))),
           fill = "red") +
  theme_minimal() +
  ggtitle("Marital Status of Customers") +
  xlab("Marital Status") +
  ylab("Count") +
  coord_flip()

# A significant number of customers are married or living together.

# Total Spending in Different Categories
my_data$Total_Spending <- rowSums(my_data[,c("MntWines", "MntFruits", "MntMeatProducts", 
                                       "MntFishProducts", "MntSweetProducts", "MntGoldProds")])
my_data$Total_Spending
hist(my_data$Total_Spending,main = "Total Spending of Customers", col = "darkgreen")
 
# Relationships Between Demographics and Spending Habits

# Total Spending vs Age
ggplot(my_data, aes(x = Age, y = Total_Spending)) +
  geom_point(color = "red") +
  theme_minimal() +
  ggtitle("Total Spending vs Age") +
  xlab("Age") +
  ylab("Total Spending")

# Middle-aged customers tend to spend more, with a decline in spending for older age groups.

# Total Spending vs Income
ggplot(data, aes(x = Income, y = Total_Spending)) +
  geom_point(color = "red") +
  theme_minimal() +
  ggtitle("Total Spending vs Income") +
  xlab("Income") +
  ylab("Total Spending")

regn <- lm(Total_Spending ~ Income, data=my_data )
regn
summary(regn)
#As income increases by 1 unit, the total spending increases by approx 0.016 units which is a very minimal amount
#p-value is very small which means the income has a significant effect on total spending

# Total Spending by Education Level
ggplot(my_data, aes(x = Total_Spending, y = Education)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  ggtitle("Total Spending by Education Level") +
  xlab("Total Spending") +
  ylab("Education")

# Higher education levels, particularly PhD, correspond to higher spending.

# Total Spending by Marital Status
ggplot(my_data, aes(x = Total_Spending, y = Marital_Status)) +
  geom_boxplot(fill = "yellow") +
  theme_minimal() +
  ggtitle("Total Spending by Marital Status") +
  xlab("Total Spending") +
  ylab("Marital Status")

# Marital status does not significantly affect spending habits.

library(corrplot)
# Correlation Analysis
numeric_columns <- sapply(my_data, is.numeric)
cor_mat <- cor(my_data[, numeric_columns], use = "complete.obs")
corrplot(cor_mat, method = "circle")

"Income and Total Spending: 
There is a strong positive correlation between 'Income' and 'Total Spending'.
This indicates that as income increases, total spending on products also tends to increase.

Age and Spending:
Age shows moderate correlations with different spending categories. For example,
there might be a noticeable relationship between age and spending on wines.

Recency and Spending :
The 'Recency' variable, indicating the number of days since the last purchase, shows 
low to moderate negative correlations with spending categories.This might suggest that recent customers tend to spend more."

# ANOVA Test - Total Spending by Education Level
anova_model <- aov(Total_Spending ~ Education, data = my_data)
anova_result <- anova(anova_model)
print(anova_result)
# If the p-value is less than 0.05, it indicates significant differences in total spending across education levels.


 
 


