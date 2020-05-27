# 제4장기술통계량(Descriptive Statistics)

# R코드 예제: 요약통계치

norm <- rnorm(100,0,1.5) # 정규분포의 랜덤샘플링
summary(norm)


# R코드 예제: 상자그림

chi <- rchisq(1000,df=4) # 카이제곱분포의 랜덤샘플링
boxplot(chi, col="yellow") # 노란색 상자그림


# R코드 예제: 히스토그램과 표본누적함수

library(MASS) # MASS 라이브러리 켜기
par(mfrow=c(2,2)) # 2 by 2로 plot 분할
norm<-rnorm(1000); hist(norm,15,freq=FALSE,main="",col=27);lines(density(norm,width=1,n=1000)) # 정규분포의 히스토그램
truehist(norm,nbins=15,col=27);curve(dnorm(x), col = 2, lty = 2, lwd = 2, add = TRUE) # 정규분포의 히스토그램
plot(ecdf(norm)) # 표본누적함수
quan<-quantile(norm,probs=seq(0,1,0.01))
plot(seq(0,1,0.01),quan,main="inverse cumulative distribution",type="s") # 표본누적함수


# R코드 예제: 산점도 - iris 예제

iris.data <- iris
View(iris.data) # 데이터 spreadsheet-style로 보기
head(iris.data) # 데이터 상단 몇개만 보기
Species <- iris.data$Species # iris.data$Species를 Species라는 변수에 저장
summary(iris.data) # 요약통계치
summary(iris.data$Sepal.Length) # 요약통계치
names(iris.data) # 각 column의 변수명
Species<-factor(iris.data$Species) # Species을 factor로
Sepal.Length<-iris.data$Sepal.Length
plot(Species,Sepal.Length) # Species에 따른 Sepal.Length값을 plotting
Sepal.Width<-iris.data$Sepal.Width
par(mfrow=c(1,2))
hist(Sepal.Width, 15, freq=FALSE, main="", col = "lightblue")
lines(density(Sepal.Width,width=0.6,n=100))
library(MASS)
truehist(Sepal.Width, nbins=15, col = "lightblue")
lines(density(Sepal.Width,n=200))
Petal.Length<-iris.data$Petal.Length
subdata<-data.frame(Sepal.Length, Sepal.Width, Petal.Length) # iris.data의 Sepal.Length, Sepal.Width, Petal.Length 자료만 추출해  subdata에 데이터 프레임 형태로 저장
color <- c("red", "blue", "green")[Species]
cor(subdata) #subdata의 분산-공분산 행렬
pairs(subdata, col = color, pch = 16) # 변수별 산점도, 각 Species를 다른 색으로 표시



# R코드 예제: Q-Q plot

par(mfrow=c(1,2))
randt <- rt(500, df = 5) # t분포 랜덤샘플링
qqnorm(randt) # Q-Q plot
qqline(randt, col = 2) # Q-Q line
qqplot(randt, qt(ppoints(500), df = 5))
qqline(randt, distribution = function(p) qt(p, df = 5),prob = c(0.25, 0.75),col = 2)
