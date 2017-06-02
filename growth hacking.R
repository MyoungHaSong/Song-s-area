setwd("C:\\Users\\Administrator\\Desktop\\noom growth hacking team")
aaa <- read.csv("Growth Survey from Web Buyflow (Deprecated).csv" ,header = T )
49704

head(aaa)

handa <- aaa[1:49704,1:4]
write.csv(handa, "handa.csv")

handa <- read.csv("handa.csv", header= T )
handa1 <- handa[1:500, ]

str(handa1)
handa1 <- as.character(handa1$answer)

handa1
# 500개로 하고싶은거 먼저 실험해보기 
result_handa1 <- strsplit(handa1 , split = "\n")

head(result_handa1 , 50)
for(i in 1:49704){ 
  result_handa1_weight1 <- as.numeric(result_handa1[[i]][1])
  result_handa1_weight <- append(result_handa1_weight ,result_handa1_weight1)}
class(result_handa1)
str(result_handa1)
da <- regexpr('심리적',result_handa1 )
da

for(i in 1:500){
  result
}

# 원하는 체중감량 대략적인 분포 
hist(result_handa1_weight,breaks= 30)

)

# 원데이터로 해보기 
handa_ex  <- as.character(handa$answer)

result_handa <- strsplit(handa_ex  , split = "\n")
head(result_handa1 , 50)
str(result_handa)
result_handa_weight <- c()
for(i in 1:49704){ 
  result_handa_weight1 <- as.numeric(result_handa[[i]][1])
  want_weight <- append(want_weight ,result_handa_weight1)}

for(i in 1:49704){ 
  result_handa_weight1 <- as.numeric(result_handa[[i]][1])
  want_weight <- append(want_weight ,result_handa_weight1)}

hist(result_handa_weight , breaks = 30 )
head(handa_ex)
tail(handa_ex)
tail(result_handa , 100)


