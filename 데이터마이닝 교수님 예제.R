summary(iris)
library(Hmisc)
describe(iris) # 데이터의 분포와 결측치의 존재 및 서로다른 unique수를 알려준다.

# 데이터 시각화
plot(iris)
plot(iris$Sepal.Length)
plot(iris$Species)
plot(iris$Species~iris$Sepal.Length,data = iris)
plot(iris$Sepal.Length , col = as.numeric(iris$Species))

library(caret)


featurePlot(iris[, 1:4] , iris$Species , "ellipse")
?featurePlot
# 전처리
cbind(as.data.frame(scale(iris[1:4])), iris$Species)
p <- princomp ( iris [, 1:4] , cor = TRUE )
p
summary(p)
plot(p , type= 'l')
?princomp
all <- factor(c(paste0(LETTERS , "0" ), paste0(LETTERS,"1")))
data <- data.frame(lvl = all , value = rnorm(length(all)))

library(randomForest)

m <- randomForest(value ~ lvl , data = data)

# 2.2 결측값의 처리
iris_na <- iris
iris_na[c(10,20,25,40,32),3] <- NA
iris_na[!complete.cases(iris_na),]

mapply(median , iris_na[1:4] , na.rm = T)
install.packages("DMwR")
library(DMwR)

iris_na[!complete.cases(iris_na), ]
centralImputation(iris_na[1:4])[c(10,20,25,32,33,40,100,123), ]

knnImputation(iris_na[1:4])[c(10,20,25,32,33,40,100,123), ]

#2.3 변수선택
library(caret)
library(mlbench)
data("Soybean")
nearZeroVar(Soybean , saveMetrics = T)

nearZeroVar(Soybean)

mySoybean <- Soybean[,-nearZeroVar(Soybean)]
mySoybean

library(mlbench)
data(Vehicle)
findCorrelation(cor(subset(Vehicle , select= -c(Class))))
myVehicle <- Vehicle[, -c(3,8,11,7,9,2)]
myVehicle


install.packages("FSelector")
library(FSelector)
library(mlbench)
data("Ozone")
(v <- linear.correlation(V4 ~ . , data = subset(Ozone, select = -c(V1,V2,V3))))
#V4와 상관관계가 높은 V18 , V12 , V9 컬럼이 제거됨

v[order(-v) , , drop = F]
# 내림차순 정렬  drop = F 를 설정해서 벡터로 출력되는것을 막음 

chi.squared(Class~. , data = Vehicle)

library(mlbench)
library(rpart)
library(caret)
data("BreastCancer")
m<- rpart(Class ~ . , data = BreastCancer)
m
# rpart 분류분석하는 함수

varImp(m)

predicted <- c(1,0,0,1,1,1,0,0,0,1,1,1)
actual <- c(1,0,0,1,1,0,1,1,0,1,1,1)

xtabs ( ~ predicted + actual)
?xtabs

sum(predicted == actual) / NROW(actual)
confusionMatrix(predicted , actual)

cm <- confusionMatrix(predicted , actual)
str(cm)

# ROC 커브
library(ROCR)

probs <- runif(100)
labels <- as.factor(ifelse(probs > .5 & runif(100) < .4 , "A", "B"))
labels

pred <- prediction(probs , labels)
pred

?prediction

plot(performance(prediction(probs , labels), "tpr", "fpr"))
set.seed(719)
install.packages("cvTools")
library(cvTools)
cv <- cvFolds(NROW(iris) , K= 10 , R = 3 )

validation_idx <- cv$subset[which(cv$which ==1 ) , 1 ]
validation_idx


train <- iris[-validation_idx]
train
validation <- iris[validation_idx, ]

library(foreach)
set.seed(719)

R = 3 
K = 10

cv <- cvFolds(NROW(iris) , K = K , R= R )
cv
foreach(r= 1:R ) %do% {
  foreach( k = 1 :K , .combine = c ) %do% {
    validation_idx <- cv$subsets[which(cv$which==k) , r]
 train <- iris[-validation_idx ,]
 validation <- iris[validation_idx,]
     #preprocessing
    
  #training
    
  #prediction

   #estimating performance
  
  #used rinif for demonstration purpose
    
  return(runif(1))
    }
}
validation
length(iris$Sepal.Length)
length(train$Sepal.Length)
validation_idx

length(validation$Sepal.Length)
library(caret)
parts <- createDataPartition(iris$Species,p= 0.8)
parts
table(iris[parts$Resample1,"Species"])
length(iris$Sepal.Width)
