library(tidyverse)
library(readr)
library(GGally)
library(stringr)
library(arules)
library(arulesViz)
library(tidyverse)

options(scipen=10000)
BlackFriday <- suppressMessages(read_csv("BlackFriday.csv"))
# Data Preprocessing
# Getting the dataset into the correct format
customers_products = BlackFriday %>%
  select(User_ID, Product_ID) %>%   # Selecting the columns we will need
  group_by(User_ID) %>%             # Grouping by "User_ID"          
  arrange(User_ID) %>%              # Arranging by "User_ID" 
  mutate(id = row_number()) %>%     # Defining a key column for each "Product_ID" and its corresponding "User_ID" (Must do this for spread() to work properly)
  spread(User_ID, Product_ID) %>%   # Converting our dataset from tall to wide format, and grouping "Product_IDs" to their corresponding "User_ID"
  t()                               # Transposing the dataset from columns of "User_ID" to rows of "User_ID"

# Now we can remove the Id row we created earlier for spread() to work correctly.
customers_products = customers_products[-1,]

write.csv(customers_products, file = 'customers_products.csv')
customersProducts = read.transactions('customers_products.csv', sep = ',', rm.duplicates = TRUE) # remove duplicates with rm.duplicates
