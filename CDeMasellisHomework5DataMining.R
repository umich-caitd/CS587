##Question 1

num_rows=10
data=read.csv("C:/Users/Caitlin/Downloads/ROCTable.csv", nrows=num_rows)

sorted_data=data[order(data$Class.Prob), ]

TP=ifelse(sorted_data$X. == "p", 1, 0)
FN=ifelse(sorted_data$X. == "n", 0, 1)
FP=ifelse(sorted_data$X. == "p", 0, 1)
TN=ifelse(sorted_data$X. == "n", 1, 0)

TPR=cumsum(TP)/(cumsum(TP)+cumsum(FN))
FPR=cumsum(FP)/(cumsum(FP)+cumsum(TN))

roc_data=data.frame(Threshold=sorted_data$Class.Prob, TP=TP, FN=FN, FP=FP, TN=TN, TPR=TPR, FPR=FPR)

plot(FPR, TPR, type="b", main="Smoothed ROC Curve", xlab="False Positive Rate", ylab="True Positive Rate")
abline(0, 1, col="red")

print(roc_data)

##install.packages("pander")
library(pander)

pander(roc_data)

##Question 2

setwd("C:/Users/Caitlin/Downloads/")

library(arules)

transactions=read.transactions("transactions.csv", sep = ",", format = "basket")

min_support=2/length(transactions)

frequent_itemsets=apriori(transactions, parameter=list(support=min_support, confidence=0, target="frequent itemsets"))

inspect(frequent_itemsets)

## Question 3 and 4

setwd("C:/Users/Caitlin/Downloads/")

library(arules)

transactions=read.transactions("transactions.csv", sep=",", format="basket")

min_support=2/length(transactions)

frequent_itemsets=apriori(transactions, parameter=list(support=min_support, confidence=0, target="frequent itemsets"))

inspect(frequent_itemsets)

##Question 5

setwd("C:/Users/Caitlin/Downloads/")

library(arules)

transactions=read.transactions("transactions.csv", sep=",", format="basket")

trans_df=as(transactions, "data.frame")

write.csv(trans_df, "transactions_weka.csv", row.names=FALSE)


