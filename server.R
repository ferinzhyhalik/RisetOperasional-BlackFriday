library(arules)
library(arulesViz)
library(tidyverse)
#source("data1.R")
BlackFriday <- suppressMessages(read_csv("BlackFriday.csv"))
options(scipen=10000)
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
rules <- reactive({
  
  rules <- apriori(data = customersProducts, parameter = list(support = 0.008, confidence = 0.80, maxtime = 0)) # maxtime = 0 will allow our algorithim to run until completion with no time limit
})

server <- function(input, output) {
  output$distPlot <- renderPlot({
    if(input$Plot == "Normal"){
      x    <- faithful$waiting
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      hist(x, breaks = bins, col = "#75AADB", border = "black",
           xlab = "Waiting time to next eruption (in mins)",
           main = "Histogram of waiting times")
    }
    if(input$Plot == "Top Product"){
      BlackFridayTopProd <- BlackFriday %>% group_by(Product_ID) %>% count() %>% arrange(desc(n))
      Top_Product <- head(BlackFridayTopProd, input$Product)
      Best_Prod <- Top_Product %>% ggplot(aes(x=reorder(Product_ID,n),y=n))+geom_col()+theme(axis.text.x = element_text(angle=90,vjust=0.5),legend.position = "none") + scale_fill_brewer(palette = 'PuBuGn')
      print(Best_Prod)
    }
    if(input$Plot == "Purchase"){
      BlackFridayPurchase = BlackFriday %>% select(User_ID, Purchase) %>% group_by(User_ID) %>% summarise(Purchase_Amount = sum(Purchase))
      Top_Buyer <- head(BlackFridayPurchase, 6)
      Best_User <- Top_Buyer %>% ggplot(aes(x = User_ID, y = Purchase_Amount, fill = User_ID)) + geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "none")
      #print(Best_User)
      print(BlackFridayPurchase)
    }
    if(input$Plot == "Gender"){
      if(input$Gender == "All"){
        PurchaseGender<-BlackFriday %>% group_by(Gender) %>% count() %>% ggplot(aes(x=Gender,y=n,fill=Gender))+geom_bar(stat="identity",position="dodge")+labs(x="",y="",fill="gender",title="Purchase by Gender")
        print(PurchaseGender)
      }
      if(input$Gender == "City A"){
        City <- filter(BlackFriday, City_Category == "A")
        CityCatGend<-City %>% group_by(Gender,City_Category) %>% count() %>% ggplot(aes(x=as.factor(Gender),y=n,fill=as.factor(Gender)))+geom_bar(stat="identity",position="dodge")+labs(x="",y="",fill="gender",title="")
        print(CityCatGend)
      }
      if(input$Gender == "City B"){
        City <- filter(BlackFriday, City_Category == "B")
        CityCatGend<-City %>% group_by(Gender,City_Category) %>% count() %>% ggplot(aes(x=as.factor(Gender),y=n,fill=as.factor(Gender)))+geom_bar(stat="identity",position="dodge")+labs(x="",y="",fill="gender",title="")
        print(CityCatGend)
      }
      if(input$Gender == "City C"){
        City <- filter(BlackFriday, City_Category == "C")
        CityCatGend<-City %>% group_by(Gender,City_Category) %>% count() %>% ggplot(aes(x=as.factor(Gender),y=n,fill=as.factor(Gender)))+geom_bar(stat="identity",position="dodge")+labs(x="",y="",fill="gender",title="")
        print(CityCatGend)
      }
    }
    if(input$Plot == "City Category"){
      if(input$City == "Summary All"){
        PurchaseCityCat<-BlackFriday %>% group_by(Gender,City_Category) %>% count() %>% ggplot(aes(x=as.factor(City_Category),y=n,fill=as.factor(Gender)))+geom_bar(stat="identity",position="dodge")+labs(x="",y="",fill="Gender",title="City Resident")
        print(PurchaseCityCat)
      }
      if(input$City == "Product City A"){
        Product <- filter(BlackFriday, City_Category == "A")
        ProdCatGend<-BlackFriday %>% group_by(Product_Category_1,City_Category) %>% count() %>% ggplot(aes(x=Product_Category_1,y=n,fill=as.factor(Product_Category_1)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 0, vjust = 0.5), legend.position = "none")
        print(ProdCatGend)
      }
      if(input$City == "Product City B"){
        Product <- filter(BlackFriday, City_Category == "B")
        ProdCatGend<-BlackFriday %>% group_by(Product_Category_1,City_Category) %>% count() %>% ggplot(aes(x=Product_Category_1,y=n,fill=as.factor(Product_Category_1)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 5, vjust = 1), legend.position = "none")
        print(ProdCatGend)
      }
      if(input$City == "Product City C"){
        Product <- filter(BlackFriday, City_Category == "C")
        ProdCatGend<-BlackFriday %>% group_by(Product_Category_1,City_Category) %>% count() %>% ggplot(aes(x=Product_Category_1,y=n,fill=as.factor(Product_Category_1)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 5, vjust = 1), legend.position = "none")
        print(ProdCatGend)
      }
    }
    if(input$Plot == "Marital Status"){
      if(input$MarStatus == "All"){
        ProdMarStat<-BlackFriday %>% group_by(Marital_Status,City_Category) %>% count() %>% ggplot(aes(x=as.factor(City_Category),y=n,fill=as.factor(Marital_Status)))+geom_bar(stat="identity",position="dodge")+labs(x="",y="")
        print(ProdMarStat)
      }
      if(input$MarStatus == "Product Marital Status 1"){
        Product <- filter(BlackFriday, Marital_Status == "1")
        ProdMarStat<-BlackFriday %>% group_by(Product_Category_1,Marital_Status) %>% count() %>% ggplot(aes(x=Product_Category_1,y=n,fill=as.factor(Product_Category_1)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 5, vjust = 1), legend.position = "none")
        print(ProdMarStat)
      }
      if(input$MarStatus == "Product Marital Status 0"){
        Product <- filter(BlackFriday, Marital_Status == "0")
        ProdMarStat<-BlackFriday %>% group_by(Product_Category_1,Marital_Status) %>% count() %>% ggplot(aes(x=Product_Category_1,y=n,fill=as.factor(Product_Category_1)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 5, vjust = 1), legend.position = "none")
        print(ProdMarStat)
      }
      if(input$MarStatus == "Summary Product"){
        ProdMarStat<-BlackFriday %>% group_by(Marital_Status,Product_Category_1) %>% count() %>% ggplot(aes(x=as.factor(Product_Category_1),y=n,fill=as.factor(Marital_Status)))+geom_bar(stat="identity",position="dodge")+labs(x="",y="")
        print(ProdMarStat)
      }
    }
    if(input$Plot == "Age"){
      if(input$Age == "All"){
        PurCityAge <- BlackFriday %>% group_by(Age, City_Category) %>% summarise(purchase1 = median(Purchase)) %>% ggplot(aes(x = Age, y = City_Category, fill = purchase1)) + geom_tile() + scale_fill_continuous(low = "black", high = "white")
        print(PurCityAge)
      }
      if(input$Age == "All Male"){
        PurCityAge <- BlackFriday %>% filter(Gender == "M") %>% group_by(Age, City_Category) %>% summarise(purchase1 = median(Purchase)) %>% ggplot(aes(x = Age, y = City_Category, fill = purchase1)) + geom_tile() + scale_fill_continuous(low = "black", high = "white")
        print(PurCityAge)
      }
      if(input$Age == "All Female"){
        PurCityAge <- BlackFriday %>% filter(Gender == "F") %>% group_by(Age, City_Category) %>% summarise(purchase1 = median(Purchase)) %>% ggplot(aes(x = Age, y = City_Category, fill = purchase1)) + geom_tile() + scale_fill_continuous(low = "black", high = "white")
        print(PurCityAge)
      }
    }
    if(input$Plot == "Product Category 1 Gender"){
      print(ProdCatGend)
    }
    if(input$Plot == "Price/Category"){
      print(PriceperCat)
    }
  })
  rulesTable <- reactive({
    ar <- rules()
    data.frame(
      lhs = labels(lhs(ar)),
      rhs = labels(rhs(ar)), 
      rules@quality)
  })
  output$predTab <- renderTable({
    rulesTable()
  })
  output$groupedPlot <- renderPlot({
    ar <- rules()
    plot(ar, method = 'grouped')
  })
  output$graphPlot <- renderPlot({
    ar <- rules()
    plot(ar, method = 'graph')
  })
  output$scatterPlot <- renderPlot({
    ar <- rules()
    plot(ar, method = 'scatter')
  })
  output$paracoordPlot <- renderPlot({
    ar <- rules()
    plot(ar, method = 'paracoord')
  })
  output$matrixPlot <- renderPlot({
    ar <- rules()
    plot(ar, method = 'matrix')
  })
}
