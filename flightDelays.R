setwd("C:\\Users\\Administrator\\Desktop\\데마 3차과제")
library(tree)
library(C50)
library(caret)
library(rpart)
library(rpart.plot)
library(CHAID)
library(gmodels)
library(ROCR)


fly<- read.csv("FlightDelays.csv")

str(fly)
summary(fly)
dim(fly)

names(fly)
attach(fly)
hist(CRS_DEP_TIME,breaks= 30)
hist(DEP_TIME)


fly$CRS_DEP_TIME<- ifelse(CRS_DEP_TIME >=600 & CRS_DEP_TIME<800 , 1,
       ifelse(CRS_DEP_TIME>= 800 &CRS_DEP_TIME <1000,2,
              ifelse(CRS_DEP_TIME>= 1000 &CRS_DEP_TIME <1200,3,
                     ifelse(CRS_DEP_TIME>= 1200 &CRS_DEP_TIME <1400,4,
                            ifelse(CRS_DEP_TIME>= 1400 &CRS_DEP_TIME <1600,5,
                                   ifelse(CRS_DEP_TIME>= 1600 &CRS_DEP_TIME <1800,6,
                                          ifelse(CRS_DEP_TIME>= 1800 &CRS_DEP_TIME <2000,7,
                                                 ifelse(CRS_DEP_TIME>= 2000 &CRS_DEP_TIME <2200,8,0))))))))


str(fly)

table(CARRIER)

# 데이터 준비
fly$CRS_DEP_TIME<- ifelse(CRS_DEP_TIME >=600 & CRS_DEP_TIME<800 , 1,
                          ifelse(CRS_DEP_TIME>= 800 &CRS_DEP_TIME <1000,2,
                                 ifelse(CRS_DEP_TIME>= 1000 &CRS_DEP_TIME <1200,3,
                                        ifelse(CRS_DEP_TIME>= 1200 &CRS_DEP_TIME <1400,4,
                                               ifelse(CRS_DEP_TIME>= 1400 &CRS_DEP_TIME <1600,5,
                                                      ifelse(CRS_DEP_TIME>= 1600 &CRS_DEP_TIME <1800,6,
                                                             ifelse(CRS_DEP_TIME>= 1800 &CRS_DEP_TIME <2000,7,
                                                                    ifelse(CRS_DEP_TIME>= 2000 &CRS_DEP_TIME <2200,8,0))))))))

str(fly)
attach(fly)
fly$CRS_DEP_TIME <- as.factor(CRS_DEP_TIME)
fly$FL_NUM <- as.factor(FL_NUM)
fly$Weather <- as.factor(Weather)
fly$DAY_WEEK <- as.factor(DAY_WEEK)
fly$DAY_OF_MONTH <-as.factor(DAY_OF_MONTH)

fly$CRS_DEP_TIME
#데이터를 랜덤하게 섞는다.

set.seed(1000)
fly_rand <- fly[order(runif(1972)),]
fly_train <- fly_rand[1:1540,]
fly_test <- fly_rand[1541:2201,]
fly_train<-fly_train[,-12]

fly_train
str(fly_train)
# 모델 훈련 
str(fly_train)
#chaid 알고리즘
tree_chaid<- chaid(Flight.Status ~CARRIER + DEST + FL_DATE + ORIGIN  , data = fly_train , )
tree_chaid
plot(tree_chaid)
chaid_predict <- predict(tree_chaid ,fly_test)

confusionMatrix(chaid_predict,fly_test$Flight.Status)

#tree 함수 (엔트로피지수)
str(fly_train)
fly_train1 <- fly_train[,-7]
tree_t <- tree(Flight.Status ~. , data = fly_train1)
plot(tree_t)
text(tree_t)

predict_t <- predict(tree_t , fly_test, type = "class")


CrossTable(predict_t, fly_test$Flight.Status,prop.c = F , prop.r =F,prop.t = F )
(253+248)/592

confusionMatrix(predict_t, fly_test$Flight.Status)

tree_prune<- prune.misclass(tree.tree)
plot(tree_prune)

tree.tree<- snip.tree(tree_t , nodes = c(3))
plot(tree.tree)
text(tree.tree)
predict_t2 <- predict(tree.tree , fly_test ,type = "class")
table(predict_t2 , fly_test$Flight.Status)
confusionMatrix(predict_t2, fly_test$Flight.Status)

ctrl <- trainControl(method = "cv", number = 10)
a <- train(Competitive ~ . , data = ebay_train ,method = "ctree",trControl =ctrl)
a
tree_t
plot(tree.tree)
text(tree.tree)


#엔트로피를 이용한 의사결정나무  C5.0
tree_C <- C5.0( Flight.Status~. ,data = fly_train , trials =1 )
plot(tree_C)
summary(tree_C)
C_predict <- predict(tree_C, fly_test)
CrossTable(C_predict,fly_test$Flight.Status,prop.c = F , prop.r =F,prop.t = F )
(263+247)/592
install.packages("cwhmisc")
library(cwhmisc)  
library(stringr) 
?train
prune.C <- prune.misclass(tree_C)

plot(a)
confusionMatrix(C_predict,fly_test$Flight.Status)
tree_C(271+262)/592
str(tree_C)
plot(tree_C)

kfo
set.seed(1000)
ctrl <- trainControl(method = "cv", number = 10)
a <- train(Competitive ~ . , data = ebay_train ,method = "C5.0",trControl =ctrl)
a

#CART 알고리즘 모델
tree_cart <- rpart(Flight.Status ~. , data = fly_train)
plot(tree_cart)
text(tree_cart)

prp(tree_cart , type= 4 , extra = 1 , digits = 3 )

cart_predict <- predict(tree_cart , fly_test, type = "class")
confusionMatrix(cart_predict,fly_test$Flight.Status)


train(Competitive ~ . , data = ebay_train ,method = "treebag",trControl =ctrl)
?train
ctrl <- trainControl(method = "cv ", number = 10)
train(Flight.Status~. , data = fly , method = "treebag" , trControl = ctrl)
str(fly)
fly

