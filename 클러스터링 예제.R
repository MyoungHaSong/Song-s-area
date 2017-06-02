setwd("C:\\Users\\Administrator\\Desktop\\학교자료\\3학년 1학기\\데이터마이닝\\3월 24일")

crime <- data.frame(num=1:16,
                           "Hartford","Honolulu","Houston","Kansas City","Los Angeles",
                           "New Orlean","New Yotk","Portland","Tucson","Washington"),
                               city=c("Atlanta","Boston","Chicago","Dallas","Denver","Detroit",
         murder=c(16.5,4.2,11.6,18.9,6.9,13.0,2.5,3.6,16.8,10.8,9.7,10.3,
                             9.4,5.9,5.1,12.5),
                    rape=c(24.8,13.3,4.7,34.2,41.5,35.7,8.8,12.7,26.6,43.2,51.8,39.7,
                           19.4,23.0,22.9,27.6))
attach(crime)

summary(crime)

D1 <- dist(crime[,3:4]) # 유클리드 거리 구하기 

D2 <- dist(crime[,3:4], method = "manhattan")  # 맨하탄 거리구하기 

hc1 <- hclust(D1^2, method = "single") # 군집화 최단연결법(single) 방법으로 군집화 
                          #hang = 1 여기서 이 행1은 선의 길이를 조정할수 있음. 
plot(hc1 , labels = city , hang = 1 , main = "dendrogram : single " ) #  plot 함수를 통해 댄드로 그램 을 그려보기 
# 수평선을 그어서 밑에 있는 것이 군집의 개수 구평의 개수를 위로 올릴수록 군집의 개수가 많아지고 내릴수록 없을듯. 
# 묶인 군집끼리 타당하게 묶였는지, 왜같이있지 왜묶여있지 라고 이야기가 되어야함.  # 안철수 이야기 ㅋ;

hc2 <- hclust(D1^2 , method = "complete") # 군집화 최장거리 연결법을 통해서 군집화를 구현 

plot(hc2 , labels = city , hang =1 , main ="dendrogram : complete") #  군집화시킨걸 plot을 통해 댄드로그램을 그려서 살펴보기 
# 조금더 깔끔하게 그려진다. 군집을 만들기가 수월해 보인다. 잘묶였는지는 도시들끼리의 특성을 봐야한다. 
x<- crime[,3:4]

c1.num <- 2  # 군집화 할 수를 정함 
hc1.result <- cutree(hc1, k= c1.num)
plot(crime[,3:4], pch = hc1.result , col = hc1.result)
text(crime[,3:4] , labels = city , adj= -0.1 , cex = 0.8 , main = "single") # 최단연결법을 통해 군집화된것 원데이터에서 구분해서 보기 

시카고 하나만떨어지고 나머지가 군집으로 묶임 뭔가이상; ㅋ


hc2.result <- cutree(hc2, k= c1.num)
plot(crime[,3:4] , pch = hc2.result , col= hc2.result)
text(crime[,3:4] , labels = city,  adj = -0.1 , cex = 0.8 ,main = "complete")

hc2.result <- cutree(hc2, k= 3)
plot(crime[,3:4],pch = hc2.result , col= hc2.result)
text(crime[,3:4] , labels= city , adj = -.1 , cex = 0.8 , main = "complete")



iris2 <- iris 
iris2$Species <- NULL


attach(iris2)
kmeans.result <- kmeans(iris2,3) # 군집을 3개로 하라. 
kmeans.result
table(iris$Species , kmeans.result$cluster) # 데이터는 리스트의형태로 들어가있음. 
plot(iris2[c("Sepal.Length" , "Sepal.Width")] , col = kmeans.result$cluster)
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")] , col = 1:3 , pch = 8, cex = 2 )


crime_k <- kmeans(x, centers = 3 )
attributes(crime_k)
?attributes

plot(x , pch= crime_k$cluster , col =crime_k$cluster, main = "K-means clustering")
text(x , labels = city, adj = -0.1 , cex = 0.8)

if(!require(fpc)){
  install.packages("fpc")
  require("fpc")
}

pamk.result <- pamk(iris2)
pamk.result$nc # 생성된 클러스터의 개수를 확인
table(pamk.result$pamobject$clustering, iris$Species)

layout(matrix(c(1,2),1,2))

plot(pamk.result$pamobject)

pam.result <- pam(iris2 , 3 )

library("cluster")
table(pam.result$clustering , iris$Species)
layout(matrix(c(1,2),1,2))
plot(pam.result)
# plotting하는것이 고객과의 커뮤니케이션에는 아주 중요한 역할을 함. 

iris2 <- iris[-5]
ds <- dbscan(iris2, eps = 0.42 , MinPts = 5 )
table(ds$cluster , iris$Species)
plot(ds , iris2 )


set.seed(435)

idx <- sample(1:nrow(iris) , 10)
newData <- iris[idx , -5 ]
newData <- newData + matrix(runif(10*4 , min = 0 , max= 0.2 ) , nrow = 10 , ncol= 4)




















if(!require("mlbench")){
  install.packages("mlbench")
  require("mlbench")
}

if(!require("stats")){
  install.packages("stats")
  require("stats")
}


setwd("C:\\Users\\Administrator\\Desktop\\학교자료\\3학년 1학기\\데이터마이닝\\3월 24일")
STD <- read.csv("Utilities.csv", header =  TRUE)
attach(STD)

par(mfrow= c(1,1))

STD.sales <- STD$Sales
STD.Fuel <- STD$Fuel_Cost

plot(Sales , Fuel_Cost , type = 'p')


plot(scale(Sales) , scale(Fuel_Cost) , xlim = c(-2,3) , ylim=c(-2 , 3 ) , xlab = "매출액", ylab= "연료비" , type = 'p')
text(Sales, Fuel_Cost , labels= as.character(Company) , cex = 0.8 , pos = 2 , col = "red")


par(mfrow = c(2,2 ))
plot(hc <- hclust(dist(STD), method = "single") , hang = -1 )
plot(hc <- hclust(dist(STD), method = "complete") , hang = - 1)
plot(hc <- hclust(dist(STD), method = "average") , hang = - 1)
plot(hc <- hclust(dist(STD), method = "centroid") , hang = - 1)
?hclust

detach(STD)
dev.off()
