setwd("C:\\Users\\Administrator\\Desktop\\학교자료\\3학년 1학기\\데이터마이닝\\4월7일")

library("mlbench")
install.packages("tree")
library("tree")

data <- read.csv("BostonHousing.csv",header = T )
data
medv <- data$MEDV

T1 <- tree(medv~., data)
summary(T1)


T1.1 <- snip.tree(T1, node = c(4,5,6,7))


pdf("tree.pdf")

plot(T1,type= "uniform")
T1
plot(T1)


library(mlbench)
library(stats)
library(tree)

Lawn <- read.csv("LawnMower.csv" , header = T)

attach(Lawn)

plot(Income , Lot_Size , xlim = c(20,120) , ylim = c(13,25) , xlab = "소득", ylab= "주택대지크기", type = "p" , pch = substring(Own,1,1))

T1 <- tree(Own~.,Lawn)
plot(T1 , type = "uniform")
text(T1)
mtext("Tree model on Lawn Mower Data", side = 3 , line = 1 , col = 2 , cex = 2 )


s.T1 <- snip.tree(T1 , node = c(4)) # 4번째 노드를 지워버린다 
partition.tree(s.T1)
plot(s.T1)
text(s.T1)


library(MASS)
library(tree)
library(mlbench)
library(stats)
setwd("")
bank <- read.csv("UniversalBank.csv" ,header= T )
names(bank)
partition <- sample(length(bank[,1]) , size= 3500)
train.bank<- bank[partition,]
test.bank <- bank[-partition,]

TRbank.tr = tree(train.bank$Personal.Loan~., train.bank)
summary(TRbank.tr)

TRbank.tr


plot(TRbank.tr)
text(TRbank.tr)


pred <- predict(TRbank.tr , test.bank)
table(round(pred,0) == test.bank[,12])



# CART 모형

library(rpart)

ukm <- read.csv("ukm.csv", header= T )
ukm$X <- NULL
test$X <- NULL

sample.num <- sample(1,0.3*nrow(ukm) , 0.7*nrow(ukm))


train <- ukm[sample.num,]
test <- ukm[-sample.num]

train

if(!require("rpart.plot")){
  install.packages("rpart.plot")
  require("rpart.plot")
}


sample.num <- sample(1:nrow(ukm),0.7*nrow(ukm))


library(rpart)
library(rpart.plot)
ukm <- data.frame(ukm)

train$
names(ukm)
attach(ukm)
ukm.tr.cart <- rpart(UNS ~. , train)
ukm.tr.cart

plot(ukm.tr.cart)
text(ukm.tr.cart)



prp(ukm.tr.cart,type=4, extra = 1, digits = 3 )

names(test)
ukm.tr.cart
yhat <- predict(ukm.tr.cart , newdata = test , type = "class")


ytest <- test[,6]
table(yhat,ytest)


library(tree)


ukm.tr <- tree(UNS ~., data = train, split = "deviance")
ukm.tr

plot(ukm.tr)
text(ukm.tr)

ukm.tr1 <- snip.tree(ukm.tr, nodes =c(10))
plot(ukm.tr1)
text(ukm.tr1, all= T )


ukm.tr2 <- prune.misclass(ukm.tr)
plot(ukm.tr2)


par(pty= "s")
plot(train[,4] , train[,5] , type= "n" , xlab= "LPR" , ylab= "PEG")


text(train[,4] , train[,5] , c("h","l","m","vl")[train[,6]])

partition.tree(ukm.tr,add= T,cex = 1.5)

partition.tree(ukm.tr , add= T , cex = 1.5 )

