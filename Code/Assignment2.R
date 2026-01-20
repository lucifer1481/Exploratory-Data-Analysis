install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")

library(ggplot2)
library(dplyr)
library(lubridate)

ecommerce_data <- read.csv("ecommerce_transactions.csv")
ecommerce_data <- na.omit(ecommerce_data)

cat("Name: S. Sanjay\n")
cat("Roll No: 23BAD097\n")

# --------------------------
# Histogram
# --------------------------
ggplot(ecommerce_data, aes(x = Transaction_Amount)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  ggtitle("Histogram of Transaction Amounts\nS. Sanjay| 23BAD097") +
  theme_minimal()

# --------------------------
# Boxplot
# --------------------------
ggplot(ecommerce_data, aes(y = Transaction_Amount)) +
  geom_boxplot(fill = "orange") +
  ggtitle("Outlier Detection using Boxplot\nS. Sanjay| 23BAD097") +
  theme_minimal()

# --------------------------
# Heatmap of Monthly Sales
# --------------------------
ecommerce_data$Month <- floor_date(as.Date(ecommerce_data$Transaction_Date), "month")

monthly_sales <- ecommerce_data %>%
  group_by(Month) %>%
  summarise(Total_Sales = sum(Transaction_Amount))

ggplot(monthly_sales, aes(x = Month, y = "Sales", fill = Total_Sales)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  ggtitle("Monthly Sales Heatmap\nS. Sanjay | 23BAD097") +
  xlab("Month") +
  ylab("") +
  theme_minimal()