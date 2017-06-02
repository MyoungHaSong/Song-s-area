setwd("C:\\Users\\Administrator\\Desktop\\프로젝트\\data")
aaa <- read.csv("08.csv", header = T,stringsAsFactors = F )
a <- aaa[,c("phc_num","age","dong",  "sex", "age_5" , "income", "ora1", "soc16_1" ,"smk3", "soc1", "men2", "men3", "age_5","soc11", "soc1" , "soc16_1")]


a < - na.omit(a)
a_old = subset(a, a$age >= 65)
a_old <- na.omit(a_old)
a_old

#dfr 데이터프레임 기준변수 var에 있다고 가정
names(a)
dfr2 <- split(a , a$phc_num)
for(i in 1:45) {
  nam <- paste0("a",i)
  assign(nam,dfr2[[i]])
}

b = data.frame()
for (i in 1:45) {
  abc <- paste0("a",i)
  b<- rbind(assign(nam,dfr2[[i]]))
  
}



# 오즈비랑 이런거 가져오기
setwd("C:\\Users\\Administrator\\Desktop\\프로젝트")
ozb <- read.csv("자살확률.csv", header = T )
names(ozb)


# 경기도 지도
setwd("C:\\Users\\Administrator\\Desktop\\TL_SCCO_SIG_SHP (1)\\TL_SCCO_SIG_SHP\\201405 시군구경계\\201405 시군구경계")
op <- par(no.readonly=TRUE)
library(maptools)
csrTMcenter <- CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000
                   +ellps=bessel +units=m +no_defs")
SMshp <- readShapePoly("TL_SCCO_SIG",verbose=TRUE,proj4string=csrTMcenter)
SMshp2 <- SMshp[which(substring(SMshp@data[,1],1,2)==41),]
par(mar=c(0,0,0,0))
plot(SMshp2,lwd=0.01)
SMshp2@data
SMshp2$rnorm <- rnorm(44)
names(SMshp2)
spplot(SMshp2 , z="rnorm") #랜덤 색으로 지정
SMshp2$rnorm
SMshp2$"자살충돌률" <- ozb$자살충동률
SMshp2$"우울증" <- ozb$우울증
SMshp2$"자살충동을.느낀다고.예상할.확률" <- ozb$자살충동을.느낀다고.예상할.확률
# 색깔 매핑하느데 빨간색으로 
library(classInt)
library(RColorBrewer)

#구간을 나눠주는데 n= 4로 설정하면 구간으 나누고 style = "quantile" 은 구간을 나누는 기준을 4분위로 사용한다.

pal <- brewer.pal(4 , "Reds")

names(SMshp2)
#
# 자살충동률
brks <- classIntervals(SMshp2$자살충돌률 , n = 4, style = "quantile")$brks 
brks[length(brks)] <- brks[length(brks)] + 1 
spplot(SMshp2 , z= "자살충돌률" , at = brks , col.regions = pal)
#우울증
brks <- classIntervals(SMshp2$우울증 , n = 4, style = "quantile")$brks 
brks[length(brks)] <- brks[length(brks)] + 1 
spplot(SMshp2 , z= "우울증" , at = brks , col.regions = pal) #
#자살충동률을 느낀다고 예상할 확률
brks <- classIntervals(SMshp2$자살충동을.느낀다고.예상할.확률 , n = 4, style = "quantile")$brks 
brks[length(brks)] <- brks[length(brks)] + 1 
spplot(SMshp2 , z= "자살충동을.느낀다고.예상할.확률" , at = brks , col.regions = pal) #




# legend 추가
library(tmap)
qtm(SMshp2 , fill = "rnorm" , fill.style = "quantile", fill.n = 8 , fill.palette = "Reds" )

# 레전드 밖으로 빼고 레전드 값 설정하기 
qtm(SMshp2, fill="rnorm", fill.style="quantile", 
    fill.labels=c("ㅋㅋ","ㅋ", "$ㅋ to $ㅋ1", "ㅋ1 to ㅋ2", "ㅋ2 to ㅋ3"),
    fill.palette="Greens",
    legend.text.size = 0.5,
    layout.legend.position = c("right", "bottom"))

tm_shape(SMshp2) + 
  tm_fill("rnorm", style="quantile",
          labels=c("ㅋ", "$ㅋ to $ㅋㅋ", "ㅋㅋㅋ","ㅋㅋㅋㅋ","ㅋㅋㅋㅋㅋㅋㅋ"),
          palette="Greens")  +
  tm_borders("grey") +
  tm_legend(outside = TRUE, text.size = .8) +
  tm_layout(frame = FALSE)

# 점추가 및 라벨 추가하기
df2 <- data.frame(lon = c(-68.783, -69.6458, -69.7653, -69.9536, 
                          -70.7733, -70.4462, -70.4449, -70.1924, -70.2691), 
                  lat = c(44.8109, 44.5521, 44.3235, 43.913, 
                          43.4399, 43.4741, 43.5104, 44.0975, 43.6651 ), 
                  Name = c("Bangor", "Waterville", "Augusta", "Brunswick", 
                           "Sanford", "Biddeford", "Saco", "Lewiston", "Portland") )


pt <- SpatialPointsDataFrame(coords = df2[,c("lon","lat")], data = df2,
                             proj4string = CRS("+proj=longlat +datum=WGS84"))


tm_shape(SMshp2) + 
  tm_fill("Pop_dens", style="quantile",  palette="Reds",
          title="명하 \n 명하") +
  tm_borders("white") +
  tm_legend(outside = TRUE, text.size = .8) +
  tm_layout(frame = FALSE,
            inner.margins = c(0, 0.25, 0, 0.01)) +
  tm_shape(pt) +
  tm_dots(size=.3, col="yellow", border.col="black") +
  tm_text("SIG_ENG_NM", auto.placement = TRUE, shadow=TRUE, size=0.8)

