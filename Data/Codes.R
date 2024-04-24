library(dplyr)

df_cities_month <- read.csv('/Users/lorahuhn/Desktop/ADA/Project/house-prices-us-master/data/cities-month_15.csv', na.strings = "")
# Make sure the data file is located in the same Directory as your "R" file is located


dim(df_cities_month)     # Size of the DF
colnames(df_cities_month)# Column names
head(df_cities_month)    # See a few observations

install.packages("tidyverse")  # Install tidyverse package if not already installed
library(tidyverse)

# Assuming df is your dataframe with city names as columns and monthly prices as rows

# Convert row names (assuming they are dates) to Date format
df <- as.data.frame(df_cities_month)
df$Date <- as.Date(rownames(df))

# Reshape data from wide to long format for easier manipulation
df_long <- df %>% 
  pivot_longer(cols = -Date, names_to = "City", values_to = "Price")

# Check for missing values and handle if necessary
df_long <- na.omit(df_long)  # Remove rows with missing values

# Calculate annual percentage change for each city
df_changes <- df_long %>%
  group_by(City) %>%
  mutate(Price_Change = (Price - lag(Price)) / lag(Price) * 100) %>%
  filter(!is.na(Price_Change))  # Remove NA values resulting from lag operation

# Summarize total price change for each city
total_price_change <- df_changes %>%
  group_by(City) %>%
  summarise(Total_Price_Change = sum(Price_Change, na.rm = TRUE)) %>%
  arrange(desc(Total_Price_Change))  # Sort cities by total price change in descending order

# Display top cities with the biggest price increases
top_cities <- head(total_price_change, 10)  # Adjust `head(10)` for desired number of top cities
print(top_cities)

# Plot total price change for top cities
top_cities %>%
  ggplot(aes(x = reorder(City, Total_Price_Change), y = Total_Price_Change)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Cities with Biggest Increase in Housing Prices",
       x = "City", y = "Total Price Change (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

