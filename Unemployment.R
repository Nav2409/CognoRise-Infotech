# The following report provides an analysis of unemployment rates across
# different states of India, with a focus on the impact of the COVID-19 
# pandemic. By exploring various dimensions of the dataset, including state-wise
# unemployment, urban vs. rural disparities, and labor participation rates,it aim to uncover patterns 
#and insights that could inform policy decisions.
# The dataset contains monthly unemployment data from Kaggle spanning from 31st May, 2019 to 30th june 2020. 

# DATASET OVERVIEW
"The provided dataset delves into the unemployment landscape across diverse states in India:

States: Various states constituting the Indian subcontinent.
Date: The specific dates of unemployment rate recordings.
Measuring Frequency: The regularity of measurement collection (Monthly).
Estimated Unemployment Rate (%): The proportion of unemployed individuals in each Indian state.
Estimated Employed Individuals: The tally of presently engaged individuals.
Estimated Labour Participation Rate (%): The percentage of the working-age population(16-64 years) 
actively involved in the job market, including both employed individuals and those actively seeking jobs."

library(readxl)

# Loading the dataset
excel<-"C:/Users/TANNU/Documents/unemployment.xlsx"
data <- read_excel(excel)
data


"Step 2 : Cleaning and preprocessing the data "

"Duplicate entries were removed, and rows with missing values were omitted to
ensure the accuracy of the analysis. Date formats were standardized, and 
extraneous whitespace was trimmed from the 'Frequency' column."

library(dplyr)
# Remove duplicate rows
data <- distinct(data)
data
# Remove rows with all NAs
data <- na.omit(data)

# Convert the Date column to Date objects
data$Date <- as.Date(data$Date, format = "%d-%m-%Y")
data
# Strip leading/trailing whitespace from the Frequency column
data$Frequency <- trimws(data$Frequency)
data

"Step 3 : Exploratory data analysis"

# Summary statistics
summary(data$`Estimated Unemployment Rate (%)`)
sd(data$`Estimated Unemployment Rate (%)`)

"Unemployment Rate
The unemployment rate has a wide range, from 0.5% to 75.85%, with a mean around 12.24%.
The standard deviation is quite high at 10.80%, indicating significant variability.
50% of the data (the median) falls below 9.65%, which suggests that the distribution
   might be right-skewed with some very high unemployment rates pulling the mean upwards."

summary(data$`Estimated Employed`)
sd(data$`Estimated Employed`)
# the average number of employed person is 13962106
#The number of employed individuals also varies widely across the dataset.
#The standard deviation is nearly as large as the mean, indicating a substantial 
#  spread in the number of employed people across different regions or times.
#The median is less than half of the maximum value, again suggesting a right-skewed distribution.

summary(data$`Estimated Labour Participation Rate (%)`)
sd(data$`Estimated Labour Participation Rate (%)`)

"Labour Participation Rate

The labor participation rate has a mean of 41.68% with a standard deviation of 7.85%, 
    suggesting moderate variability.
The median is very close to the mean, which may indicate a more symmetric distribution
    of labor participation rates."

"Next, we should visualize these distributions and look at how the unemployment rate
changed over time, especially in light of the COVID-19 pandemic, as well as
differences between regions and urban vs. rural areas."
library(ggplot2)
library(dplyr)

# Histogram of Estimated Unemployment Rate (%)
ggplot(data, aes(x = `Estimated Unemployment Rate (%)`)) +
  geom_histogram(binwidth = 2.5, fill = "blue",colour = "black", alpha = 0.7) + 
  labs(title = "Distribution of Estimated Unemployment Rate (%)", x = "Unemployment Rate (%)", y = "Frequency")

"The distribution shows a peak at lower percentages, indicating that most data points 
    have a relatively low unemployment rate.
There is a long tail toward higher percentages, confirming the right-skewness suggested by the summary statistics.
The presence of very high unemployment rates may indicate extreme cases such as the COVID-19 pandemic."

ggplot(data, aes(x = `Estimated Labour Participation Rate (%)`)) +
  geom_histogram(binwidth = 3, fill = "blue",colour = "black", alpha = 0.7) + 
  labs(title = "Distribution of Estimated Labour Participation Rate(%) ", 
       x = "Labour Participation Rate(%)", y = "Frequency")

"The distribution appears to be symmetric, with a clear central peak and tails on both sides.
This suggests that the labor participation rate varies less dramatically across different regions
or over time than the unemployment rate or the number of employed individuals."


data
#time series plot to visualize the trend of unemployment rate in different regions
"This will help us understand if there were any significant changes that might be associated
with the COVID-19 pandemic or other events."
ggplot(data, aes(x = Date, y = `Estimated Unemployment Rate (%)`, 
                 group = Region...7, color = Region...7 )) +
  geom_line() 
labs(title = "Unemployment Rate Over Time by Region", x = "Date",
     y = "Unemployment Rate (%)")

"The time series plots provide the following insights:

Overall Trend:
There is a noticeable spike around mid-2020, which aligns with the onset of the COVID-19 pandemic 
and the subsequent lockdowns and economic disruptions.
  
By Region 

The plot shows the overall trend without specifically focusing on the COVID-19 period. 
The unemployment rate in the south region during the months of April to June seems to be higher
than other regions. It further declines after the month of June.
We can see from the month of July to October that the estimated unemployment rate is highest in the North region.

After the spike, there is a downward trend indicating a recovery phase,but the rates have not returned to 
the pre-pandemic levels, especially in urban areas."


# State-wise analysis
state_unemployment <- data %>%
  group_by(Region...1) %>%
  summarize(Average_Unemployment = mean(`Estimated Unemployment Rate (%)`)) %>%
  arrange(Average_Unemployment)
state_unemployment

ggplot(state_unemployment, aes(x = Average_Unemployment, y = reorder(Region...1, Average_Unemployment))) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Average Unemployment Rate by State", x = "Average Unemployment Rate (%)", y = "State")


#The bar chart shows the average unemployment rate for each state over the time period covered by the dataset. 
#Here are some observations:

"There is a wide range in the average unemployment rates across different states,
indicating regional disparities in employment conditions.
Some states have notably higher average unemployment rates, while others have managed to maintain lower averages.
Haryana has the highest average unemployment rate following by Tripura and Jharkhand
This state-wise analysis is valuable for policymakers to identify which regions
may need more attention and resources to combat unemployment."

state_labour_part <- data %>%
  group_by(Region...1) %>%
  summarize(Avg_labour_participation = mean(`Estimated Labour Participation Rate (%)`) )%>%
  arrange(Avg_labour_participation)
state_labour_part

ggplot(state_labour_part, aes(x = Avg_labour_participation, y = reorder(Region...1, Avg_labour_participation))) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Average Labour Participation Rate by State", x = "Average Labour Participation Rate (%)", y = "State")

"This bar chart shows the average labour force participation rate  for each state over the time period
covered by the dataset. 
All the states have maintained the average at more than 30%
There is not much variation in the average labour force participation rate of different states.
Meghalaya has the highest average labour participation rate over the months followed by Tripura & Telangana."

# Region wise analysis
region_unemployment <- data %>%
  group_by(Region...7, Date) %>%
  summarize(Average_Unemployment = mean(`Estimated Unemployment Rate (%)`))
print(region_unemployment,n=40)

ggplot(region_unemployment, aes(x = Date, y = Average_Unemployment, group = Region...7, color =Region...7)) +
  geom_line() +
  labs(title = "Region-wise Unemployment Rate Over Time", x = "Date", y = "Average Unemployment Rate (%)")

"The line plot illustrates the region-wise unemployment rate trends over time:

The east and south regions have witnessed large fluctuations in the average unemployment rate while
the rates in the north region lie between 10% to 25% (not showing much fluctuations)
The northeast and west regions have relatively lower average unemployment rates(<20%)
The spike around mid-2020, likely corresponding to the COVID-19 pandemic's impact,is clearly visible 
in all the regions.
Post the spike, there appears to be a gradual decline, suggesting a recovery from
the peak unemployment rates experienced during the pandemic."

# Pre and Post COVID-19 analysis
pre_pandemic <- filter(data, Date < as.Date("2020-03-01"))
post_pandemic <- filter(data, Date >= as.Date("2020-03-01"))

mean(pre_pandemic$`Estimated Unemployment Rate (%)`)
mean(post_pandemic$`Estimated Unemployment Rate (%)`)

"The comparison of average unemployment rates before and after the onset 
of the COVID-19 pandemic reveals:

Pre-pandemic average unemployment rate: approximately 12.23%
Post-pandemic average unemployment rate: approximately 12.96%
This indicates a significant increase in the unemployment rate following the onset of the
pandemic. The data suggests that the economic impact of COVID-19 was considerable,
nearly doubling the unemployment rate on average across the regions in the dataset.
"