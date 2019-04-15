library(tidyverse)
library(readr)
library(GGally)
library(stringr)

options(scipen=10000)
BlackFriday <- suppressMessages(read_csv("BlackFriday.csv"))
#BlackFriday <- BlackFriday %>% select(-User_ID)
#glimpse(BlackFriday)
#head(BlackFriday)

#BlackFridayPurchase <- BlackFriday %>% select(User_ID, Purchase) %>% group_by(User_ID) %>% distinct()

#BlackFridayTopProd <- BlackFriday %>% group_by(Product_ID) %>% count() %>% arrange(desc(n)) 
#Top_Product <- BlackFriday_20[1:20,] %>% ggplot(aes(x=Product_ID,y = n,fill=Product_ID))+geom_col()+theme(axis.text.x = element_text(angle=90,vjust=0.5),legend.position = "none")

#BlackFridayPurchase = BlackFriday %>% select(User_ID, Purchase) %>% group_by(User_ID) %>% distinct()
#userPurchase <- BlackFridayPurchase %>% ggplot(aes(x=User_ID, y = Purchase, fill = User_ID))+geom_col()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "none")

#BlackFridayAge <- BlackFriday %>% select(User_ID, Age) %>% distinct() %>% count(Age)
#customers_age_vis = ggplot(data = BlackFridayAge) + geom_bar(color = 'black', stat = 'identity', mapping = aes(x = Age, y = n, fill = Age)) + labs(title = 'Age of Customers') + theme(axis.text.x = element_text(size = 10)) + scale_fill_brewer(palette = 'Blues') + theme(legend.position="none")

PurchaseGender<-BlackFriday %>% group_by(Gender) %>% count() %>% ggplot(aes(x=Gender,y=n,fill=Gender))+geom_bar(stat="identity",position="dodge")+labs(x="",y="",fill="gender",title="Purchase by Gender")
#PurchaseCityCat<-BlackFriday %>% ggplot(aes(x=City_Category,y=Purchase,fill=Gender))+geom_boxplot()
PurchaseMarStat<-BlackFriday %>% ggplot(aes(x=as.factor(Marital_Status),y=Purchase,fill=Gender))+geom_boxplot()
#gridExtra::grid.arrange(PurchaseGender, PurchaseCityCat, PurchaseMarStat)

#table(BlackFriday$Age)
#table(BlackFriday$Stay_In_Current_City_Years)

BlackFriday$Stay_In_Current_City_Years<-if_else(BlackFriday$Stay_In_Current_City_Years == '4+','4',BlackFriday$Stay_In_Current_City_Years)
BlackFriday$Stay_In_Current_City_Years<-as.numeric(BlackFriday$Stay_In_Current_City_Years)

BlackFriday<-BlackFriday %>% separate(Age,c("aa","bb")) %>% mutate(aa=as.numeric(aa),bb=as.numeric(bb)) %>% mutate(Age=(aa+bb)/2) %>% select(-c(aa,bb))
BlackFriday$Age<-ifelse(is.na(BlackFriday$Age),55,BlackFriday$Age)
#glimpse(BlackFriday)

table(BlackFriday$Product_Category_1)
ProdCatPref<-BlackFriday %>% group_by(Product_Category_1) %>% count() %>% ggplot(aes(x=reorder(Product_Category_1,n),y=n))+geom_col(aes(fill=as.factor(Product_Category_1)))+labs(x="",y="",title="product category perference")+theme(legend.position="none")
ProdCatGend<-BlackFriday %>% group_by(Gender,Product_Category_1) %>% count() %>% ggplot(aes(x=as.factor(Product_Category_1),y=n,fill=as.factor(Gender)))+geom_bar(stat="identity",position="dodge")+labs(x="",y="",fill="gender",title="product category perference in gender")
PriceperCat<-BlackFriday %>% ggplot(aes(x=reorder(as.factor(Product_Category_1),Purchase),y=Purchase))+geom_boxplot()+ggtitle("Price per category")
#gridExtra::grid.arrange(ProdCatPref, ProdCatGend, PriceperCat)