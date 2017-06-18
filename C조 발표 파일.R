##### Chapter 10: 모델 성능 평가 -------------------
## 혼돈 매트릭스 만들기

#작업 디렉토리 만들기 . 
setwd("C:\\Users\\Administrator\\Desktop\\보아즈 세션\\R책\\DataSets & Codes\\DataSets & Codes\\chapter 10 모델성능평가")

# 나이브 베이즈에서 사용했던 데이터셋 불러오기 
sms_results <- read.csv("sms_results.csv")
# 데이터가 제데로 들어가있는지 확인 
head(sms_results)

# 실제 분류된 type과 기존에 나이브 베이즈를 이용해서 분류한 값이 다른것을 확인
head(subset(sms_results, actual_type != predict_type))

# 혼돈 매트릭스를 만들기 전에 정분류와 오분류 확인 하기위해 table 함수로 간단하게 표현
table(sms_results$actual_type, sms_results$predict_type)
# xtabs 함수는 수식 인터 페이스를 사용하여 수식에 포함되어 있는것을 교차하여 표로 만든다.
xtabs(~ actual_type + predict_type, sms_results)
# 좀더 자세한결과를 얻기 위해 gmodels 라는 library안에 있는 CrossTable함수를 이용
library(gmodels)
CrossTable(sms_results$actual_type, sms_results$predict_type)
# 각 cell의 가장 위에 있는것이 분류된것임 . 그 밑의 값은 row proportion ,table proportion , chi-square contribution값


# 예측정확도와 오차비율을 계산 
# 예측 정확도 
(154 + 1202) / (154 + 1202 + 5 + 29)

# 개념에서 설명할태니 자세한 설명은 생략한다.;;;
# 왼쪽 위의 셀 과 오른쪽 아래셀 값을 더하고 전체값으로 나눈다.

# 오차 비율
(5 + 29) / (154 + 1202 + 5 + 29)

# 오른쪽위 셀과 왼쪽 아래셀을 더해서 전체값으로 나눈다.
# 오차 비율은  = 1 - 예측 정확도 값으로도 구할수 있다. 
1 - 0.9755396

## 정확도를 넘어 다른 성능 측정.
library(caret)
if(!require("e1071")){
  install.packages("e1071")
  require("e1071")
}

# 위에서 만들었던 혼돈 매트릭스와 다른 혼돈 매트릭스
# 이 함수에서는 긍정의 결과를 반드시 명시해야한다. 따라서 스팸을 걸러내는 것이기 때문에 긍정 값에 spam을 입력한다. 
confusionMatrix(sms_results$predict_type, sms_results$actual_type, positive = "spam")

# Kappa 통계량
# Kappa 통계량은 우연히 정확한 예측을 할 확률을 계산한다.(자세한 설명은 D조가 잘해주겠지 ㅋ;;) 

# Kappa 통계량을 계산하기 위한 과정 
# Kappa =(pr_a -pr_e) /1 - pr_e
# 여기서 pr_a는 분류기와 실제 값 사이에서 pr_a는 실제 (a)와 예측된(e)의 일치도 비율을 말한다. 
# pr_a 계산하기 위해서는 실제 sms값과 예측된 값인 모든 인스턴스의 비율을 더하면된다.
CrossTable(sms_results$actual_type, sms_results$predict_type)
pr_a <- 0.865 + 0.111
pr_a
# 왼쪽 위의 셀과 오른쪽 아래의 셀에서 가장 밑의 값을 더해준다 .

# 카파 통계는 임의적으로 두개가 선택된다는 가정하에 우연으로 예측된 값과 실제값이 일치할 확률인 pr_e로 정확도를 조정한다.
# pr_e를 구하는 과정은 다음과 같다.
# 예측한 결과와 실제결과로 메세지가 스팸일 경우와 햄일 확률의 합.(두상호 배타적 사건의 확률의 합이기 떄문에 곱하고 더하면된다.)

pr_e <- 0.868 * 0.886 + 0.132 * 0.114
pr_e

# 공식에 대입하자. 
k <- (pr_a - pr_e) / (1 - pr_e)
k

if(!require("vcd")){
  install.packages("vcd")
  require("vcd")
}
# R에는 자동적으로 Kappa값을 계산해주는 함수가 있는데 바로 vcd library에 들어있는 Kappa함수
# 실행 결과를 보면 #weighted값으로 되어있는 것이 우리가 찾는 값이다. 
Kappa(table(sms_results$actual_type, sms_results$predict_type))

if(!require("irr")){
  install.packages("irr")
  require("irr")
}
# 또 다른 함수로는 irr 라이브러리에 있는 kappa2함수이다. 
kappa2(sms_results[1:2])

# 민감도와 특이도 .
# 민감도는 정확하게 분류한 긍정의 비율을 측정한다.
# 특이도는 정확하게 분류됐지만 부정예제의 비율계산
# example using SMS classifier

xtabs(~ actual_type + predict_type, sms_results)
# 밑의 두셀의 연산
sens <- 154 / (154 + 29)
sens

spec <- 1202 / (1202 + 5)
spec
# 위의 두셀의 연산 

# caret 패키지에 있는 민감도와 특이도를 계산하는 함수를 살펴보자 .
library(caret)
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")
specificity(sms_results$predict_type, sms_results$actual_type, negative = "ham")

# 민감도가 0.842 -> 스팸메세지를 정확하게 84% 분류했다.
# 특이도가 0.996 -> 99.6%의 스팸 메세지가 아닌 메세지를 올바르게 구별했다. 


# 정밀도와 재현율
xtabs(~ actual_type + predict_type, sms_results)
prec <- 154 / (154 + 5)
prec

# 정밀도는 긍정인 비율에서 참인 긍정의 비율값
# 재현율은 결과가 어떻게 마무리 되는지 측정 (참긍정의 수를 총참의수로 나눠 정의한다.) 민감도와 유사하지만 해석이 다르다.
# 표보면서 잘 계산하면된다 ;;
rec <- 154 / (154 + 29)
rec


# caret library에는 정밀도를 계산하는 함수를 제공한다.
library(caret)
posPredValue(sms_results$predict_type, sms_results$actual_type, positive = "spam")
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")

# F 측정은정밀도와 재현율을 하나로 합하는 모델성능이라한다.
# F 측정은 조합 평균을 이용해서 정확도와 재현율을 조합 .
# 표보고 계산하는건 안어려우니 해보시길.
xtabs(~ actual_type + predict_type, sms_results)
f <- (2 * prec * rec) / (prec + rec)
f

f2 <- (2 * 154) / (2 * 154 + 5 + 29)
f2

## Visualizing Performance Tradeoffs ----
library(ROCR)
pred <- prediction(predictions = sms_results$prob_spam,
                   labels = sms_results$actual_type)

# ROC curves
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve for SMS spam filter", col = "blue", lwd = 2)

# add a reference line to the graph
abline(a = 0, b = 1, lwd = 2, lty = 2)

# calculate AUC
perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
as.numeric(perf.auc@y.values)

# partitioning data
library(caret)
credit <- read.csv("credit.csv")

# Holdout method
# using random IDs
random_ids <- order(runif(1000))
credit_train <- credit[random_ids[1:500],]
credit_validate <- credit[random_ids[501:750], ]
credit_test <- credit[random_ids[751:1000], ]

# using caret function
in_train <- createDataPartition(credit$default, p = 0.75, list = FALSE)
credit_train <- credit[in_train, ]
credit_test <- credit[-in_train, ]

# 10-fold CV
folds <- createFolds(credit$default, k = 10)
str(folds)
credit01_train <- credit[folds$Fold01, ]
credit01_test <- credit[-folds$Fold01, ]

## Automating 10-fold CV for a C5.0 Decision Tree using lapply() ----
library(caret)
library(C50)
library(irr)

set.seed(123)
folds <- createFolds(credit$default, k = 10)

cv_results <- lapply(folds, function(x) {
  credit_train <- credit[x, ]
  credit_test <- credit[-x, ]
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})

str(cv_results)
mean(unlist(cv_results))
