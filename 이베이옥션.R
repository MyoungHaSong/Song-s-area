setwd("C:\\Users\\Administrator\\Desktop\\데마 3차과제")
library(tree)
library(C50)
library(caret)
library(rpart)
library(CART)
library(rpart.plot)
library(CHAID)
library(gmodels)
library(ROCR)
ebay <- read.csv("eBayAuctions.csv", header = T )
names(ebay)
attach(ebay)

# 데이터의 준비와 탐구

str(ebay)
# factor형으로 변환
attach(ebay)
ebay$CategoryD <- as.factor(CategoryD)
ebay$currencyD <- as.factor(currencyD)
ebay$DurationD <- as.factor(DurationD)
ebay$endDayD <- as.factor(endDayD)
ebay$Competitive <- as.factor(Competitive)
summary(ebay)
# 카테고리 몇개 있는지 확인
table(CategoryD)

# 화폐 단위확인
table(currencyD)
# sellerRating
hist(sellerRating,breaks= 20)
#DurationD
table(DurationD)
#ClosePrice
hist(ClosePrice,breaks= 20)
#OpenPrice
hist(OpenPrice,breaks= 20)


= ctrl)

1972*0.7
#데이터를 랜덤하게 섞는다.
ebay_rand <- ebay[order(runif(1972)),]
ebay_train <- ebay_rand[1:1380,]
ebay_test <- ebay_rand[1381:1972,]

table(ebay$Competitive)
1066/(906+1066)
table(ebay_train$Competitive)
751/(629+751)
table(ebay_test$Competitive)
315/(277+315)

#모델 training
#chaid 알고리즘
tree_chaid<- chaid(Competitive ~CategoryD+currencyD+DurationD+endDayD , data = ebay_train , )
plot(tree_chaid)
chaid_predict <- predict(tree_chaid ,ebay_test)

tree_chaid
CrossTable(chaid_predict, ebay_test$Competitive,prop.c = F , prop.r =F,prop.t = F )
(174+195)/592




confusionMatrix(chaid_predict,ebay_test$Competitive)

?count.fields()
class(chaid_predict)

#베깅을 통해서 미래 성능을 알아보기 위해서 베깅을 이용한다.
set.seed(1000)
ctrl <- trainControl(method = "cv", number = 10)
c <- train(Competitive ~CategoryD+currencyD+DurationD+endDayD , data = ebay_train ,method = "CHAID",trControl =ctrl)




#엔트로피를 이용한 의사결정나무 tree
tree_t <- tree(Competitive ~. , data = ebay_train)
plot(tree_t)
text(tree_t)

predict_t <- predict(tree_t , ebay_test, type = "class")


CrossTable(predict_t, ebay_test$Competitive,prop.c = F , prop.r =F,prop.t = F )
(253+248)/592

confusionMatrix(predict_t,ebay_test$Competitive)

tree_prune<- prune.misclass(tree_t)
plot(tree_prune)

tree.tree<- snip.tree(tree_t , nodes = c(2))
predict_t2 <- predict(tree.tree , ebay_test ,type = "class")
table(predict_t2 , ebay_test$Competitive)
confusionMatrix(predict_t2,ebay_test$Competitive)

ctrl <- trainControl(method = "cv", number = 10)
a <- train(Competitive ~ . , data = ebay_train ,method = "ctree",trControl =ctrl)
a
tree_t
plot(tree.tree)
text(tree.tree)
#엔트로피를 이용한 의사결정나무  C5.0
tree_C <- C5.0( Competitive~. ,data = ebay_train , trials =1 )
summary(tree_C)
C_predict <- predict(tree_C, ebay_test)
CrossTable(C_predict, ebay_test$Competitive,prop.c = F , prop.r =F,prop.t = F )
(263+247)/592
install.packages("cwhmisc")
library(cwhmisc)  
library(stringr) 
?train
prune.C <- prune.misclass(tree_C)

plot(a)
confusionMatrix(C_predict,ebay_test$Competitive)
tree_C(271+262)/592
str(tree_C)
plot(tree_C)

kfo
set.seed(1000)
ctrl <- trainControl(method = "cv", number = 10)
a <- train(Competitive ~ . , data = ebay_train ,method = "C5.0",trControl =ctrl)
a









?train
#CART 알고리즘 모델
tree_cart <- rpart(Competitive ~. , data = ebay_train)
plot(tree_cart)
text(tree_cart)

prp(tree_cart , type= 4 , extra = 1 , digits = 3 )

cart_predict <- predict(tree_cart , ebay_test, type = "class")
confusionMatrix(C_predict,ebay_test$Competitive)


train(Competitive ~ . , data = ebay_train ,method = "treebag",trControl =ctrl)
?train
