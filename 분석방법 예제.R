##=================================================================
## 1)  자료 탐색/기초 자료 분석
##=================================================================

# Directory 지정
setwd("C:\\?۾???")

# 데이터 읽기
abalone_df <- read.csv("Abalone data.csv", stringsAsFactors = F)

# 작업 공간 저장
save(abalone_df , file=" C:\\??��?۾???\\Abalone data.rdata")

# 데이터 형태 파악
summary(abalone_df)
head(abalone_df)

# 전체 열(column)이름 보기
colnames(abalone_df)

##=================================================================
## 2)  이상치/결측치 분석
##=================================================================

# 특정 행에 결측치 위치 찾기 
which(is.na(abalone_df$Viscera_weight))

# 결측값 데이터 제거하기
abalone_df <- abalone_df[- which(is.na(abalone_df$Viscera_weight)), ]

# 분포 확인 및 이상치 찾기
# - 상자그림그리기
boxplot(abalone_df$Diameter)

# - 하한이상치기준, 1Q, IQR, 3Q, 상한이상치기준
boxplot.stats(abalone_df$Diameter)$stats

# - 데이터 내 이상치 해당 값 찾기

boxplot.stats(abalone_df$Diameter)$out

##=================================================================
## 3)  자료 특성 분석 (1/2)
##=================================================================

# 나이별 데이터 수
table(abalone_df$Age)

# 데이터의 분포를 확인하기 위한 히스토그램
# rings : 나이테
hist(abalone_df$Rings, nclass = 20) 

#  분포가 왼쪽에 치우친 경우 값의 log10을 취하기도 함
# 두 변수의 산점도 그리기
plot(abalone_df$Whole_weight, abalone_df$Diameter)

# 회귀직선을 빨간색 선으로 그려줌
# Y : Diameter(지름),  X : Whole_weight(전체 무게)
abline(lm(abalone_df$Diameter ~ abalone_df$Whole_weight), col = "red")


# 전체 무게와 나이테  간의 상관분석
cor.test(abalone_df$Whole_weight, abalone_df$Rings)
# 상관분석 결과 시각화
#  -  산점도 그리기
pairs(abalone_df[, c("Length", "Diameter", "Shucked_weight", "Rings")])

#  -  상관계수 시각화하기
if(!require("corrplot")) {
  install.packages("corrplot")
  require("corrplot")
}
M <- cor(abalone_df[, c("Length", "Diameter", "Shucked_weight", "Rings")])
corrplot(M, method = "ellipse")


##=================================================================
## 4) 모형 개발 
##=================================================================

##-----------------------------------------------
## 1. ?Ϲ? ???? ????, ???? ȸ?? ?м?
##-----------------------------------------------
# 분석 데이터 생성
train_df <- abalone_df[c(1:3000),] # 전체 데이터의 70%
모형을 평가할때 쓰는것. 

test_df <- abalone_df[c(3001:4174),]  # 전체 데이터의 30%
모형을 검증할때 쓰는것. 


# formula 정의 (y ~ x1 + x2 + x3)
formula <- Rings ~ Length + Diameter + Height + Whole_weight + Shucked_weight + Viscera_weight + Shell_weight

# 다중 회귀 모형 적합
lm_model <- lm(formula, data = train_df)
# 단계적 변수 선택
step(lm_model)
summary(step(lm_model))


# 회귀 모형을 활용한 예측 적용해보기
lm.pred.value <- predict(lm_model, test_df, type = "response")

# 예측 결과 시각화 하기
plot(test_df$Rings, lm.pred.value )
abline(0, 1, col = "red")

##-----------------------------------------------
## 2. 모형 개발 (2/5)
##-----------------------------------------------




hist(abalone_df$Rings, nclass=20)
plot(abalone_df$Diameter ~ abalone_df$Whole_weight)
abline(lm(abalone_df$Diameter ~ abalone_df$Whole_weight), col = "red")

cor.test(abalone_df$Whole_weight, abalone_df$Rings)

boxplot(abalone_df$Diameter)

pairs(abalone_df[, c("Length", "Diameter", "Shucked_weight", "Rings")])

M <- cor(abalone_df[, c("Length", "Diameter", "Shucked_weight", "Rings")])
corrplot(M, method = "ellipse")

formula <- Rings ~ Length + Diameter + Height + Whole_weight + Shucked_weight + Viscera_weight + Shell_weight

lm(formula, data = abalone_df)


formula <- Age ~ Length + Diameter + Height + Whole_weight + Shucked_weight + Viscera_weight + Shell_weight
abalone_df$Age <- as.factor(abalone_df$Age)

X <- as.matrix(subset(abalone_df, select = -c(Sex, Rings, Age)))
tuneobj = tune.svm(formula, data = na.omit(abalone_df), gamma = 10^(-6:-3), cost = 10^(1:3))
summary(tuneobj)$best.parameters 
svm_model <- svm(formula , data = abalone_df, kernel='linear', scale = F,cost = 1000, gamma = 0.001)

# svm_model$coefs[,1] 
# svm_model$index 

w <- t(svm_model$coefs) %*% X[svm_model$index,] 

abalone_df$Age <- as.factor(abalone_df$Age)
my.data <- subset(abalone_df, select = c(Whole_weight, Diameter, Age))[1:100,]

library(e1071)
svm.model <- svm(Age ~ Whole_weight + Diameter, data = my.data, type='C-classification',kernel = 'linear',scale = FALSE)


col_index <- ifelse(my.data$Age == "Adult", 2, 3)
plot(my.data$Whole_weight, my.data$Diameter, col = col_index, pch = 19) ; abline(h = 0,v = 0,lty = 3)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
abline(a = -b/w[1,2], b = -w[1,1]/w[1,2], col = "blue", lty = 3)


glm(Age ~ Whole_weight + Diameter, data = train_df , family = binomial)

logistic_model <- glm(Age ~ Whole_weight + Diameter, data = train_df , family = binomial())
summary(logistic_model)

pred.value <- predict(logistic_model, test_df, type = "response")
pred.value <- ifelse(pred.value > 0.5, "Young", "Adult")

table(test_df$Age, pred.value)

nn_model   <- nnet(formula, data = train_df, size = 1)
pred.value <- predict(nn_model, test_df, type = "class")

test.cl(test_df, predict(nn_model, test_df))



summary(model.gbm)[order(summary(model.gbm), decreasing = T),]

barplot(sort(summary(model.gbm)$rel.inf), horiz = T)
text(2, )

formula <- Rings ~ Length + Diameter + Height + Whole_weight + Shucked_weight + Viscera_weight + Shell_weight

# ?????? ???귀 모형 ??????
lm(formula, data = train_df)
# ???계적 변??? ??????
step(lm(formula, data = train_df), direction = "backward")
summary(step(lm(formula, data = train_df), direction = "backward")) 


install.packages("tsDyn")
library(tsDyn)

?MAPE

mean(abs((test_df$Rings - lm.pred.value)/test_df$Rings), na.rm = T)


mean(abs((test_df$Rings - lm.pred.value))/ test_df$Rings)


#Create X matrix with intercept 
X <- subset(train_df, select = -c(Sex, Age, Rings))
y <- train_df$Rings

#Projection matrix 
P=X%*%solve(t(X)%*%X)%*%t(X) 

#Fitted values 
fv=P%*%y 

#Run canned regression 
reg=lm(y~x) 

#Canned and hand computed fitted values 
cbind(fitted(reg),fv) 

#Are they all equal? 
all.equal(as.vector(as.numeric(fitted(reg))),as.vector(as.numeric(fv))) 
#This already implies that the R-squared is equal 

#Compute R-squared by hand 
R.sq=1-sum((y-fv)^2)/sum((y-mean(y))^2) 



pred.value <- predict(nn_model, test_df)
pred.value <- ifelse(pred.value > 0.5, "Young", "Adult")
# ?????? 결과 보기
table(t_df$Age, pred.value)

y <- train_df$Rings
1 - (sum((y-predict(lm_model))^2)/sum((y-mean(y, na.rm = T))^2))


pred_df <- data.frame(real_value = test_df$Rings, 
                      predict_value = lm.pred.value)

(test_df$Rings, lm.pred.value)

1-sum(apply(pred_df, 1, min)/apply(pred_df, 1, max))/nrow(pred_df)

