# 제6장 신뢰구간(Confidence Intervals)


# R 예제: 모분산을 아는 경우의 모평균의 신뢰구간

library(MASS)
height <- na.omit(survey$Height) # survey$Height내의 NA값 제거한 것을 height라는 변수에 저장
n = length(height) # height 데이터 개수
sigma = 9.48 # 표본표준편차
se = sigma/sqrt(n); se # 표준오차
E=qnorm(.975)*se; E # 신뢰 계수
sample.mean = mean(height) # 표본평균 
sample.mean+c(-E,E) # 신뢰구간

install.packages("TeachingDemos") # 패키지 "TeachingDemos" 설치
library("TeachingDemos")
z.test(height, sd=sigma) # z 테스트


# R 예제: 모분산을 모르는 경우의 모평균의 신뢰구간

library(MASS) 
height = na.omit(survey$Height)
n = length(height) 
sd = sd(height) # 표본표준편차
se = sd/sqrt(n); se # 표본오차
E = qt(.975, df=n-1)*se; E # margin of error 

sample.mean = mean(height) # 표본평균
sample.mean+c(-E,E) # 신뢰구간

t.test(height) # t 테스트


# R 예제: 카이제곱 분포

plot(function(x) dchisq(x, df = 2), 0, 27, ylim = c(0, 0.5),main="Chi-squared-Density",ylab="f") # 자유도 2인 카이제곱 분포
plot(function(x) dchisq(x, df = 5), 0, 27, col="blue",add=TRUE) # 자유도 5인 카이제곱 분포
plot(function(x) dchisq(x, df = 10), 0, 27, col="green",add=TRUE) # 자유도 10인 카이제곱 분포
text(1.5,0.4,"df=2");text(4,0.18,"df=5");text(9,0.14,"df=10") # 원하는 위치에 텍스트 삽입


# R 예제: 모분산 신뢰구간-1

X1 <- rnorm(50, mean = 0, sd = 2) # 평균 0, 표준편차 2인 정규 분포를 따르는 확률변수 X1 랜덤 샘플링 50개
X2 <- rnorm(30, mean = 1, sd = 1) # 평균 1, 표준편차 1인 정규 분포를 따르는 확률변수 X1 랜덤 샘플링 30개
var.test(X1,X2) # X1과 X2가 같은 분산을 갖는가?

# R 예제: 모분산 신뢰구간-2

sigma.X1 <- sd(X1); sigma.X2 <- sd(X2) # X1,X2의 표본표준편차
n.X1 <- length(X1); n.X2 <- length(X2) # 표본크기
F0 <- sigma.X1^2/sigma.X2^2; F0 # F0값
a <- .05 # alpha 값
f.rhalf_a <- qf(1-a/2,n.X2-1,n.X1-1) # 신뢰계수
f.lhalf_a <- qf(a/2,n.X2-1,n.X1-1) # 신뢰계수
CI0 <- c(f.lhalf_a,f.rhalf_a) # 신뢰계수
CI <- F0*CI0; CI # 신뢰구간
pvalue <- 2*(1-pf(F0,n.X1-1,n.X2-1)); pvalue # p값

# R 예제: 모비율 신뢰구간 - 1

library(MASS)
gender = na.omit(survey$Sex)
n = length(gender) # gender 데이터 개수
n.Female = sum(gender == "Female") # gender 데이터 중 Female 값인 데이터의 개수
pbar = n.Female/n; pbar # 표본비율
se = sqrt(pbar*(1-pbar)/n); se # 표준오차
E = qnorm(.975)*se; E # margin of error 
pbar+c(-E,E) # 신뢰구간
size=0.25/se^2

# R 예제: 모비율 신뢰구간 - 2

prop.test(n.Female, n)

binom.test(n.Female, n)