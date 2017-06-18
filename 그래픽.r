library(ggplot2)

# 2.3 산점도 그리기
qplot(mtcars$wt, mtcars$mpg)

qplot(wt, mpg , data = mtcars)

ggplot(mtcars , aes(x=wt , y = mpg)) +geom_point()

#2.4 제목과 라벨 추가하기

p <- ggplot(mtcars) + geom_point() + ggtitle("Test ggplot2") + aes(x = wt , y = mpg)


# 2.5 격자 제거하기

q +theme(panel.grid.major = element_blank() , panel.grid.minor = element_blank())



install.packages("googleVis")

require("googleVis")


value1 <- c(60,20,8,7,3,2)

item1 <- c("아메리카노","카페라떼", "카푸치노","카페모카","바닐라라떼","아보카토")

Encoding(item1) <- "utf-8"

df <- data.frame(item1 , value1)


pie2 <- gvisPieChart(df , options = list(is3D = T))
plot(pie2)
print(pie2 , file = "C:\\Users\\Administrator\\Desktop\\커피판매비율.html")


# 3.3 googleVis Example
# Line Chart
library(googleVis)

df <- data.frame(country = c("US","GB","BR"), val1 = c(10,13,14), val2 = c(23,12,32))
df

Line <- gvisLineChart(df)
plot(Line)


# Line Chart with two axis
library(googleVis)

Line2 <- gvisLineChart(df , "country", c("val1","val2"),options = list(series = "[{targetAxisIndex:0},{targetAxisIndex:1}]",vAxes = "[{title:'val1'},{title:'val2'}]"))
plot(Line2)


# Bar Chart
library(googleVis)

df <- data.frame(country = c("US","GB","BR") , val1 = c(10,13,14) , val2 = c(23,12,32))
Bar <- gvisBarChart(df)
plot(Bar)


#column Chart
library(googleVis)

df <- data.frame(country = c("US","GB","BR"), val1 = c(10,13,14),val2 = c(23,12,32))
column <- gvisColumnChart(df)
plot(column)


# combo chart
library(googleVis)

df <- data.frame(country = c("US","GB","BR"), val1 = c(10,13,14),val2 = c(23,12,32))
Combo <- gvisComboChart(df ,xvar="country",yvar = c("val1","val2"), options = list(seriesType ="bars",series='{1:{type:"line"}}'))
plot(Combo)

#Scatter chart

library(googleVis)
df <- data.frame(country =c("US","GB", "BR"), val1 = c(10,13,14), val2 = c(23,12,32))
Scatter <- gvisScatterChart(women,options = list(legend = 'none', lineWidth = 2 , pointSize = 0 , title ="Women",vAxis = "{title:'weight(lbs)'}",hAxis="{title:'height(in)'}",width=300,height = 300))
plot(Scatter)

#Bubble Chart
library(googleVis)
Bubble <- gvisBubbleChart(Fruits , idvar = "Fruit",xvar = "Sales",yvar = "Expenses", colorvar = "Year",sizevar = "Profit", options =list(hAxis ='minValue:75,maxValue:125}'))
plot(Bubble)


# Candlestick Chart
library(googleVis)
Candle <- gvisCandlestickChart(OpenClose , options = list(legend='none'))
plot(Candle)


#Pie Chart

library(googleVis)
Pie <- gvisPieChart(CityPopularity)
plot(Pie)

#Gauge
library(googleVis)

Gauge <- gvisGauge(CityPopularity , options = list(min= 0 , max = 800 , greenFrom = 500 , greenTo = 800 , yellowFrom = 300, yellowTo=500,redFrom = 0 , redTo=300 , width= 400 , height =300))
plot(Gauge)

#
library(googleVis)


install.packages("sp")
library(sp)

China_lev0 <- readRDS("C:\\Users\\Administrator\\Desktop\\china-map\\CHN_adm0.rds")
str(China_lev0)
plot(China_lev)


China_lev1 <- readRDS("C:\\Users\\Administrator\\Desktop\\china-map\\CHN_adm1.rds")
str(China_lev1)
plot(China_lev1)

China_lev2 <- readRDS("C:\\Users\\Administrator\\Desktop\\china-map\\CHN_adm2.rds")
str(China_lev2)
plot(China_lev2)

China_lev3 <- readRDS("C:\\Users\\Administrator\\Desktop\\china-map\\CHN_adm3.rds")
str(China_lev3)
plot(China_lev3)


korea_lev2 <- readRDS("C:\\Users\\Administrator\\Desktop\\china-map\\korea\\KOR_adm2.rds")


head(str(korea_lev2),1)

plot(korea_lev2)

(korea_lev2@data$NL_NAME_2)[100]

ss <- korea_lev2@data$NL_NAME_2==(korea_lev2@data$NL_NAME_2)[100]
suwon <- korea_lev2[korea_lev2@data$NL_NAME_2==korea_lev2@data$NL_NAME_2[100]]
ss<-korea_lev2@data[100,]
korea_lev2@data[95:105,]


seoul <- korea_lev2[korea_lev2$NAME_1 == "Gueonggi-do",]
korea_lev2$``
plot(seoul)
plot(seoul, col = c(1:length(unique(seoul$NAME_2))))1

install.packages("dismo")
library(dismo)
install.packages("rgdal")
library(rgdal)
install.packages("XML")
library(XML)
korea_map <- gmap("South Korea")
head(korea_map)
plot(korea_map)

# 도로 지도 그리기
library(dismo)
library(rgdal)
library(XML)
korea_roadmap <- gmap("South Korea", type = "roadmap")
head(korea_roadmap)
plot(korea_roadmap)

# 위성 지도 그리기 
library(dismo)
library(rgdal)
library(XML)
korea_satellitemap <- gmap("South Korea", type = "satellite")
head(korea_satellitemap)
plot(korea_satellitemap)


# mapproj 패키지와 ggmap 패키지를 이용하여 구글 맵 지도를 생성 할 수 있다. 
install.packages("mapproj")
library(mapproj)
install.packages("ggmap")
library("ggmap")

seoul_map1 <- get_map(location = "seoul", zoom = 11 , maptype = "roadmap")
p.1 <- ggmap(seoul_map1)
p.1 <- p.1 + labs(title = "Seoul map roadmap")

print(p.1)
