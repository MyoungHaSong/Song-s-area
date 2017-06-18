setwd("C:\\Users\\Administrator\\Desktop\\All_Data\\All_Data")

p198 <- read.table("P198.txt" , header=  T )
p198

# ols  적합하기 
regression <- glm(Y ~ X1 + X2 + X3 , data = p198)
regression

library(plyr)

# AK 개체 빼고  ols 하기

which(p198[,1]=="AK")# AK 개체 위치 확인
p198<- p198[-49,]
regression <- glm(Y ~ X1 + X2 + X3 , data = p198)
regression 

# residual 을 원래 데이터 프레임에 넣고 전체 residual 의 제곱합도 넣음
resid <- regression$residuals 
p198$mss <- sum(resid^2) 
p198$resid <- resid
# region 별로 resid 제곱합 계산해서 넣기 
mssj <- ddply(p198, .(Region) ,
              function(p198) {
                data.frame(sigmaj  = sum((p198$resid)^2)
                )}) 
# 각 region 별로 개체 값 구하기 
nj <- c()

for(i in 1:4){
  njj <- length(subset(p198 , Region == i )$Region)
  nj <- rbind(nj, njj )
}

nj # 제대로 구해졌나 확인
# 데이터 프레임 이름 변경 
rownames(nj) <- c(1:4)
colnames(nj) <- "nj"
as.data.frame(nj)


# 구해놓은거 넣기;;
mssj$nj <- nj
mssj$sigmaj <- (mssj$sigmaj)/(nj-1)
mssj$n <- length(p198$Y)
mssj$tss <-p198$mss[1:4]
mssj$sigma <- mssj$tss/mssj$n


mssj
# 만들어 놓은 것을 한 data.frame으로 합치기 
p198akmt <- merge(p198,mssj)
names(p198akmt)

p198akmt

# ols 할것과 weight 에 쓰일 weight 구하기 
p198akmt$cj <- sqrt(p198akmt$sigmaj/p198akmt$sigma)
p198akmt$cY <- p198akmt$Y / p198akmt$cj
p198akmt$ci <- 1/p198akmt$cj
p198akmt$cX1 <- p198akmt$X1 / p198akmt$cj
p198akmt$cX2 <- p198akmt$X2 / p198akmt$cj
p198akmt$cX3 <- p198akmt$X3 / p198akmt$cj

p198akmt$wt <- 1/p198akmt$cj^2
p198akmt

#WLS 적합시키기 
model_WLS <- glm(Y~X1+X2+X3 ,weights =wt , data = p198akmt )
model_WLS
summary(model_WLS)
#OLS 적합시키기 
model_OLS <- lm(cY ~ci+ cX1 +cX2 +cX3 -1  ,data = p198akmt)
model_OLS
summary(model_OLS)