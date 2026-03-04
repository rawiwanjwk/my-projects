library(tidyverse)
library(readr)
library(scales)

df <- read_csv("online_retail_rfm_final.csv")

str(df)
View(df)

# Summarize each customer's segment
## Bar Chart
ggplot(df, aes(Customer_Segment, fill = Customer_Segment)) +
  geom_bar() +
  theme_minimal() +
  labs(title= "Summarize customer's segment")

count(df, Customer_Segment)
  
# Relationship between Frequency(F) & Monetary(M)
## Scatter Plot
set.seed(99)
small_df <- sample_n(df, 1000)
  
ggplot(df, aes(x = Frequency, y = Monetary, col = Customer_Segment))+
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal() +
  labs(title= "Relationship between Frequency(F) & Monetary(M)",
       y = "Total Spending",
       x = "Purchase Frequency")

# Top 10 goods in valued customers
df %>%
  filter(Customer_Segment == "Valued Customer") %>%
  count(Description, sort = TRUE) %>%
  head(10)

# Top 5 total spending in valued customers
top5spending <- 
  df %>%
    select(CustomerID, Country, Customer_Segment, Monetary) %>% 
    filter(Customer_Segment == "Valued Customer") %>%
    arrange(desc(Monetary)) %>%
    head(5)

ggplot(top5spending, aes(x = reorder(CustomerID, -Monetary), y = Monetary, fill = Country)) +
  geom_col() +
  geom_text(aes(label = comma(round(Monetary, 0))), vjust = 1.5) +
  scale_y_continuous(labels = label_comma()) +
  theme_minimal() +
  labs(title = "Top 5 Total Spending in Valued Customers",
       subtitle = "แยกตามรายชื่อลูกค้าและประเทศ",
       x = "Customer ID",
       y = "Total Spending (Monetary)")

# Top 10 most sales descriptions(products)
top10sales <-
  df %>% 
    group_by(Description) %>% 
    summarise(Total_spending = sum(Monetary)) %>% 
    arrange(desc(Total_spending)) %>%
    head(10)

ggplot(top10sales, aes(x = Total_spending, y = reorder(Description, Total_spending), fill = Description)) +
  geom_col(show.legend = FALSE) + # ปิด Legend เพื่อเพิ่มพื้นที่ 
  geom_text(aes(label = comma(Total_spending)), hjust = 1.1) + # ใช้ hjust ขยับlabel และใส่ comma
  scale_x_continuous(labels = label_comma(), expand = expansion(mult = c(0, .15))) + # เพิ่มพื้นที่ปลายกราฟไม่ให้ตัวเลขหลุดขอบ
  theme_minimal() +
  labs(title = "Top 10 most sales description",
       x = "Total Spending",
       y = "Product Description")

# Top 10 quantity sales descriptions(products)
top10product <-
  df %>% 
  group_by(Description) %>% 
  summarise(Total_products = n()) %>% 
  arrange(desc(Total_products)) %>%
  head(10)

ggplot(top10product, aes(x = Total_products, y = reorder(Description, Total_products), fill = Description)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = Total_products), hjust = 1.1, color = "white", fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0, .20))) +
  theme_minimal() +
  labs(title = "Top 10 quantity sales descriptions",
       x = "Quantity",
       y = "Products")
