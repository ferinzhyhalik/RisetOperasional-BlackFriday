source("data1.R")

server <- function(input, output) {
  reacRule <- reactive({
    rules <- apriori(data = customersProducts, parameter = list(support = 0.008, confidence = 0.8, maxtime = 0)) # maxtime = 0 will allow our algorithim to run until completion with no time limit
  })
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
      if(input$Gender == "Summary All"){
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
      if(input$Gender == "All"){
        PurchaseCityCat<-BlackFriday %>% group_by(Gender,City_Category) %>% count() %>% ggplot(aes(x=as.factor(City_Category),y=n,fill=as.factor(Gender)))+geom_bar(stat="identity",position="dodge")+labs(x="",y="",fill="Gender",title="City Resident")
        print(PurchaseCityCat)
      }
    }
    if(input$Plot == "City Category"){
      if(input$prodcat == "Category 1"){
        if(input$City == "All"){
          PurchaseCityCat<-BlackFriday %>% group_by(Product_Category_1,City_Category) %>% count() %>% ggplot(aes(x=Product_Category_1,y=n,fill=as.factor(Product_Category_1)))+geom_bar(stat="identity",position="dodge")+labs(x="",y="",fill="Gender",title="City Resident")
          print(PurchaseCityCat)
        }
        if(input$City == "Product City A"){
          Product <- filter(BlackFriday, City_Category == "A")
          ProdCatGend<-Product %>% group_by(Product_Category_1,City_Category) %>% count() %>% ggplot(aes(x=Product_Category_1,y=n,fill=as.factor(Product_Category_1)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 0, vjust = 0.5), legend.position = "none")
          print(ProdCatGend)
        }
        if(input$City == "Product City B"){
          Product <- filter(BlackFriday, City_Category == "B")
          ProdCatGend<-Product %>% group_by(Product_Category_1,City_Category) %>% count() %>% ggplot(aes(x=Product_Category_1,y=n,fill=as.factor(Product_Category_1)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 5, vjust = 1), legend.position = "none")
          print(ProdCatGend)
        }
        if(input$City == "Product City C"){
          Product <- filter(BlackFriday, City_Category == "C")
          ProdCatGend<-Product %>% group_by(Product_Category_1,City_Category) %>% count() %>% ggplot(aes(x=Product_Category_1,y=n,fill=as.factor(Product_Category_1)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 5, vjust = 1), legend.position = "none")
          print(ProdCatGend)
        }
      }
      if(input$prodcat == "Category 2"){
        if(input$City == "All"){
          PurchaseCityCat<-BlackFriday %>% group_by(Product_Category_2,City_Category) %>% count() %>% ggplot(aes(x=Product_Category_2,y=n,fill=as.factor(Product_Category_2)))+geom_bar(stat="identity",position="dodge")+labs(x="",y="",fill="Gender",title="City Resident")
          print(PurchaseCityCat)
        }
        if(input$City == "Product City A"){
          Product <- filter(BlackFriday, City_Category == "A")
          ProdCatGend<-Product %>% group_by(Product_Category_2,City_Category) %>% count() %>% ggplot(aes(x=Product_Category_2,y=n,fill=as.factor(Product_Category_2)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 0, vjust = 0.5), legend.position = "none")
          print(ProdCatGend)
        }
        if(input$City == "Product City B"){
          Product <- filter(BlackFriday, City_Category == "B")
          ProdCatGend<-Product %>% group_by(Product_Category_2,City_Category) %>% count() %>% ggplot(aes(x=Product_Category_2,y=n,fill=as.factor(Product_Category_2)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 5, vjust = 1), legend.position = "none")
          print(ProdCatGend)
        }
        if(input$City == "Product City C"){
          Product <- filter(BlackFriday, City_Category == "C")
          ProdCatGend<-Product %>% group_by(Product_Category_2,City_Category) %>% count() %>% ggplot(aes(x=Product_Category_2,y=n,fill=as.factor(Product_Category_2)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 5, vjust = 1), legend.position = "none")
          print(ProdCatGend)
        }
      }
      if(input$prodcat == "Category 3"){
        if(input$City == "All"){
          PurchaseCityCat<-BlackFriday %>% group_by(Product_Category_3,City_Category) %>% count() %>% ggplot(aes(x=Product_Category_3,y=n,fill=as.factor(Product_Category_3)))+geom_bar(stat="identity",position="dodge")+labs(x="",y="",fill="Gender",title="City Resident")
          print(PurchaseCityCat)
        }
        if(input$City == "Product City A"){
          Product <- filter(BlackFriday, City_Category == "A")
          ProdCatGend<-Product %>% group_by(Product_Category_3,City_Category) %>% count() %>% ggplot(aes(x=Product_Category_3,y=n,fill=as.factor(Product_Category_3)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 0, vjust = 0.5), legend.position = "none")
          print(ProdCatGend)
        }
        if(input$City == "Product City B"){
          Product <- filter(BlackFriday, City_Category == "B")
          ProdCatGend<-Product %>% group_by(Product_Category_3,City_Category) %>% count() %>% ggplot(aes(x=Product_Category_3,y=n,fill=as.factor(Product_Category_3)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 5, vjust = 1), legend.position = "none")
          print(ProdCatGend)
        }
        if(input$City == "Product City C"){
          Product <- filter(BlackFriday, City_Category == "C")
          ProdCatGend<-Product %>% group_by(Product_Category_3,City_Category) %>% count() %>% ggplot(aes(x=Product_Category_3,y=n,fill=as.factor(Product_Category_3)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 5, vjust = 1), legend.position = "none")
          print(ProdCatGend)
        }
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
    if(input$Plot == "Product Category Gender"){
      if(input$ProCatGen == "Category 1"){
        ProdCatGend<-BlackFriday %>% group_by(Gender,Product_Category_1) %>% count() %>% ggplot(aes(x=as.factor(Product_Category_1),y=n,fill=as.factor(Gender)))+geom_bar(stat="identity",position="dodge")+labs(x="",y="",fill="gender",title="product category perference in gender")
        print(ProdCatGend)
      }
      if(input$ProCatGen == "Category 2"){
        ProdCatGend<-BlackFriday %>% group_by(Gender,Product_Category_2) %>% count() %>% ggplot(aes(x=as.factor(Product_Category_1),y=n,fill=as.factor(Gender)))+geom_bar(stat="identity",position="dodge")+labs(x="",y="",fill="gender",title="product category perference in gender")
        print(ProdCatGend)
      }
      if(input$ProCatGen == "Category 3"){
        ProdCatGend<-BlackFriday %>% group_by(Gender,Product_Category_3) %>% count() %>% ggplot(aes(x=as.factor(Product_Category_1),y=n,fill=as.factor(Gender)))+geom_bar(stat="identity",position="dodge")+labs(x="",y="",fill="gender",title="product category perference in gender")
        print(ProdCatGend)
      }
    }
    if(input$Plot == "Price"){
      if(input$Price == "Category 1"){
        PriceCat<-BlackFriday %>% ggplot(aes(x=reorder(as.factor(Product_Category_1),Purchase),y=Purchase))+geom_boxplot()+ggtitle("Price per category")
        print(PriceCat)
      }
      if(input$Price == "Category 2"){
        PriceCat<-BlackFriday %>% ggplot(aes(x=reorder(as.factor(Product_Category_2),Purchase),y=Purchase))+geom_boxplot()+ggtitle("Price per category")
        print(PriceCat)
      }
      if(input$Price == "Category 3"){
        PriceCat<-BlackFriday %>% ggplot(aes(x=reorder(as.factor(Product_Category_3),Purchase),y=Purchase))+geom_boxplot()+ggtitle("Price per category")
        print(PriceCat)
      }
    }
    if(input$Plot == "Stay in City"){
      if(input$StayCity == "All"){
        ProdMarStat<-BlackFriday %>% group_by(Stay_In_Current_City_Years,City_Category) %>% count() %>% ggplot(aes(x=as.factor(City_Category),y=n,fill=as.factor(Stay_In_Current_City_Years)))+geom_bar(stat="identity",position="dodge")+labs(x="",y="")
        print(ProdMarStat)
      }
      if(input$StayCity == "City A"){
        Product <- filter(BlackFriday, City_Category == "A")
        ProdMarStat<-Product %>% group_by(Stay_In_Current_City_Years) %>% count() %>% ggplot(aes(x=Stay_In_Current_City_Years,y=n,fill=as.factor(Stay_In_Current_City_Years)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 5, vjust = 1), legend.position = "none")
        print(ProdMarStat)
      }
      if(input$StayCity == "City B"){
        Product <- filter(BlackFriday, City_Category == "B")
        ProdMarStat<-Product %>% group_by(Stay_In_Current_City_Years) %>% count() %>% ggplot(aes(x=Stay_In_Current_City_Years,y=n,fill=as.factor(Stay_In_Current_City_Years)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 5, vjust = 1), legend.position = "none")
        print(ProdMarStat)
      }
      if(input$StayCity == "City C"){
        Product <- filter(BlackFriday, City_Category == "C")
        ProdMarStat<-Product %>% group_by(Stay_In_Current_City_Years) %>% count() %>% ggplot(aes(x=Stay_In_Current_City_Years,y=n,fill=as.factor(Stay_In_Current_City_Years)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x = element_text(angle = 5, vjust = 1), legend.position = "none")
        print(ProdMarStat)
      }
    }
  })
  output$rulesDataTable <- renderPrint({
    ar <- reacRule()
    inspect(sort(ar, by='lift'))
  })
  output$groupedPlot <- renderPlot({
    ar <- reacRule()
    plot(ar, method = 'grouped')
  })
  output$graphPlot <- renderPlot({
    ar <- reacRule()
    plot(ar, method = 'graph')
  })
  output$scatterPlot <- renderPlot({
    ar <- reacRule()
    plot(ar, method = 'scatter')
  })
  output$paracoordPlot <- renderPlot({
    ar <- reacRule()
    plot(ar, method = 'paracoord')
  })
  output$matrixPlot <- renderPlot({
    ar <- reacRule()
    plot(ar, method = 'matrix')
  })
}
