install.packages("arules")
install.packages("installr")
library(installr)
install.packages("UsingR")
library(arules)



library(UsingR)

updateR()
library(arules)
as.


data <- list(c("A","F","H","I","J") , c("B", "D","E", "G","I") , c("C","G","H","I"),c("A","F","G","H","J") , c("A", "B","C","D","F","G","H"))
data <- list(c("A"))

dt <- as(data,"transactions")
class(dt)
dt
as(dt, "matrix")

as(dt, "data.frame")


asser_apriori <- apriori(dt)
summary(asser_apriori)

inspect(head(assr_apriori, by="support", n =15 ))

es <- eclat(dt)
summary(es)
inspect(head(es , by = "support" , n= 15))

p1 <- subset(asser_apriori , subset = support >0.5 )
p2 <- subset(es , subset= support >0.5 )


d1 <- dissimilarity(p1 , method = "Euclidean")
d2 <- dissimilarity(p2 , method = "Euclidean")


plot(hclust(d1 , "complete") , main  = "Cluster Dendrogram \n of Association")
plot(hclust(d2 , "complete") , main = " Cluster Dendrogram \n of Frequent Items")

class1 <- hclust(d2 , "complete")$order[1:4]


inspect(p2[class1])



install.packages("arulesViz")
library(arulesViz)


data(Income)


gr_apr <- apriori(Income , parameter = list(support = 0.2 ))


plot(gr_apr , method = "grouped")
상품 A가 먼저나오고 B를 구매를하냐 이문제가 RHS이다 ; 
RHS 는 왼쪽이 맞아서 오른쪽이 뭐냐
동그라미 크기는 support의 크기를 나타낸다. 
농도가 진할수록 lift값이 큰거다