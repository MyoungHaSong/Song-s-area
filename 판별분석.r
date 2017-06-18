setwd("C:\\Users\\Administrator\\Desktop\\학교자료\\3학년 1학기\\데이터마이닝\\4월 28")

data <- read.csv("data.csv", header =T )
data
library(MASS)
# 판별분석 모형식
disc.fit <- lda(factor(sex) ~ fsiq+ weight + height , data= data)
# 분류표
pred_sex <- predict(disc.fit , newdata = data[,-1 ])
result <- table(data$sex , pred_sex$class)

# 정분류율
accuracy <- 100*sum(diag(result)) / sum(result)



# 두번째 예제
data <- data.frame(x1,x2,y)

write.csv(data ,file = "data2.csv")

attach(data)

cld <- lda(y~x1+ x2 )
cld
aa<- apply(cld$means%*%cld$scaling,2,mean)
cld$means
cld$scaling
predict(cld)

table(y, predict(cld)$class)

plot(cld,dimen= 1 )

# 판별예측
new <- data.frame(x1 = c(20) , x2 = c(40))
predict(cld,new)

if(!require("caTools")){
  install.packages("caTools")
  require("caTools")
}

xx <- data.frame(x1 = c(x1), x2 = c(x2))
colAUC(xx, y , plotROC = T )

# 판별그래프1
plot(x1 , x2 )