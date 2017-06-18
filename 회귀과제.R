# 회귀 과제 

# 5.8 문제 
library(dummies)

setwd("C:\\Users\\Administrator\\Desktop\\All_Data\\All_Data")

aaa <- read.table("P159.txt", header= T )

aaa$interact <- Sex *Height 
attach(aaa)
names(aaa)

class(aaa$interaction)
# 한번에 다 때려박은 RM 모델
regression_pooled = lm(Weight~ Height)
# 갬마와 델타를 추가해서 넣은 FM 모델 
regression2_FM= glm(Weight ~ Height + Sex + interact)

# 델타 없이 적합 시킨 모델 
regression3_no_interact <- glm(Weight ~ Height + Sex ) 

# 나이를 포함 시킨 모델 
regression4_age <- glm(Weight ~ Height + Sex + Age )

summary(regression4_age)



regression2_FM
regression2$coefficients
regression_pooled$coefficients

summary(regression_pooled)
summary(regression2_FM)
summary(regression3_no_interact)

# vif 를 보기위해 library(car) 를활용하여 vif 를 계산 해서 본다.
library(car)
vif(regression3)
vif(regression2)


# 회귀 5.11 문제 

bbb <- read.table("P011.txt", header = T )
attach(bbb)
names(bbb)

FM <- glm(Price~ Bowl+ Casserole+ Dish  + Tray + Plate + Diam + Time )
bbb
summary(FM)

