#read transactions
options(digits = 22)
df_transaction <- read.table("Soma_Orders.csv", header = TRUE, sep = ",")
str(df_transaction)
summary(df_transaction)

df_sorted <- df_transaction[order(df_transaction$Order_No),]
library(dplyr)
df_sorted_unique <- distinct(df_sorted)
#convert dataframe to transaction format using ddply; 

if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}

#group all the items that were bought together; by the same customer on the same date
library(plyr)
df_itemList <- ddply(df_sorted_unique, c("Order_No"), function(df1)paste(df1$LOT_NUMBER,collapse = ","))

#remove member number and date
df_itemList$Order_No <- NULL

colnames(df_itemList) <- c("itemList")

#write to csv format
write.csv(df_itemList,"ItemList.csv", quote = FALSE, row.names = TRUE)

#-------------------- association rule mining algorithm : apriori -------------------------#
library(Matrix)
library(arules)
library(grid)
library(arulesViz)

#convert csv file to basket format
txn = read.transactions(file="ItemList.csv", rm.duplicates= FALSE, format="basket",sep=",",cols=1);

# Create an item frequency plot for the top 20 items
itemFrequencyPlot(txn, topN=20, type="absolute")

# Get the rules
rules <- apriori(txn, parameter = list(minlen =2, supp = 0.0001, conf = 0.8))

# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:10])

#basket_rules <- apriori(txn,parameter = list(minlen =2, supp = 0.001, conf = 0.1, target="rules"))
#inspect(basket_rules)

rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])

subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

inspect(rules)
#What are customers likely to buy before buying certain products

#rules<-apriori(data=txn, parameter=list(supp=0.001,conf = 0.08), 
#               appearance = list(default="lhs",rhs=5131512),
#               control = list(verbose=F))
#rules<-sort(rules, decreasing=TRUE,by="confidence")
#inspect(rules[1:5])

#What are customers likely to buy if they purchase certain products
#rules<-apriori(data=txn, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
#               appearance = list(default="rhs",lhs="whole milk"),
#               control = list(verbose=F))
#rules<-sort(rules, decreasing=TRUE,by="confidence")
#inspect(rules[1:5])

df_basket <- as(rules,"data.frame")
df_basket$confidence <- df_basket$confidence * 100
df_basket$support <- df_basket$support * nrow(df_basket)

write.csv(df_basket,"Rules_48.csv",row.names = FALSE)

plot(rules)

set.seed(8000)

plot(rules, method = "grouped", control = list(k = 5))

plot(rules[1:10,], method="graph", control=list(type="items"))

plot(rules[1:10,], method="paracoord",  control=list(alpha=.5, reorder=TRUE))

itemFrequencyPlot(txn, topN = 5)

plot(rules[1:15,],measure=c("support","lift"),shading="confidence",interactive=T)
