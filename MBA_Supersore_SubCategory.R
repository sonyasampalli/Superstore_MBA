#install and load package arules
#install.packages("arules")
library(arules)
library(arulesViz)
#install and load tidyverse
install.packages("tidyverse")
library(tidyverse)
#install and load readxml
install.packages("readxml")
library(readxl)
#install and load knitr
install.packages("knitr")
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
install.packages("lubridate")
library(lubridate)
#install and load plyr
#install.packages("plyr")
library(plyr)
library(dplyr)

superstore <- read.csv("C://Users//sony//Desktop//Case Study_TCON//Data- Superstore.csv", sep = ",")
superstore_products<-superstore$Sub.Category
colnames(superstore)
head(superstore)
summary(superstore$Sub.Category)
inspect(superstore$Sub.Category)

#trial to convert items into transactions

new_items<-cbind(superstore$Order.Date,superstore$Customer.ID,superstore$Sub.Category)

head(new_items)

order.date<-as.Date(superstore$Order.Date,format = "%m/%d/%y")

Customer.ID<-as.numeric(as.character(superstore$Customer.ID))
cbind(superstore,order.date)
cbind(superstore$Sub.Category)
glimpse(superstore)
transactiondata<-ddply(superstore,c("Customer.ID","order.date"),function(df1)paste(df1$Sub.Category, collapse = ","))
head(transactiondata)

transactiondata1<-transactiondata[1:2,]
colnames(transactiondata)<-c("order.ID","order.date","Items")
dim(transactiondata)

colnames(transactiondata)

transactiondata$order.ID<-NULL
transactiondata$order.date<-NULL

write.csv(transactiondata,"C:/Users/sony/Desktop/Case Study_TCON/MarketBasket_trans_Scategory.csv",quote=FALSE,row.names=FALSE)

# Data preparation - creating a sparse matrix for transaction data
superstore_tr<-read.transactions("C://Users//sony//Desktop//Case Study_TCON//MarketBasket_trans_Scategory.csv",sep=",")
summary(superstore_tr)

inspect(superstore_tr[1:5])
itemFrequency(superstore_tr[, 1:3])

# Visualizing item support - item frequency plots
itemFrequencyPlot(superstore_tr, support = 0.1)
itemFrequencyPlot(superstore_tr, topN = 15,horiz=FALSE,main='Item frequency, relative')
image(superstore_tr[1:5])
image(sample(superstore_tr, 100))

association.rules <- apriori(superstore_tr, parameter = list(support = 0.001,confidence = 0.75, minlen = 2))
summary(association.rules)

# improving model performance
inspect(sort(association.rules, by = "lift")[1:5])
inspect(association.rules[1:3])

# Taking subsets of association rules of phones
Phonerules <- subset(association.rules, items %in% "Phones")
inspect(Phonerules[1:3])

write(association.rules, file = "C:/Users/sony/Desktop/Case Study_TCON/Presentation_Superstore Data/MBA_Supersore_SubCategory_rules.csv", sep = ",", quote = TRUE, row.names = FALSE)
associationrules_df <- as(association.rules, "data.frame")
str(associationrules_df)

#Plot
top10subRules <- head(Phonerules, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")
