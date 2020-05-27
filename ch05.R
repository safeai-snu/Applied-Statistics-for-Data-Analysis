# 제5장 모수추정법(Parameter Estimation)

# R코드 예제: 중심극한정리

par(mfrow=c(2,2)) # 2 by 2로 plot 분할
hist(sample(1:6,replace=TRUE,100000),breaks=0.5:6.5,main="",xlab="one die") # 1~6의 눈 100000개 샘플링 후 히스토그램
X1<-sample(1:6,replace=TRUE,100000) # 확률변수 X1: 주사위에 적힌 숫자, 100000개 샘플링
X2<-sample(1:6,replace=TRUE,100000) # 확률변수 X2: 주사위에 적힌 숫자, 100000개 샘플링
hist(X1+X2,breaks=1.5:12.5,main="", xlab="two dice") # 확률변수 X1+X2의 확률밀도함수 히스토그램
X3<-sample(1:6,replace=TRUE,100000) # 확률변수 X3: 주사위에 적힌 숫자, 100000개 샘플링
hist(X1+X2+X3,breaks=2.5:18.5,main="", xlab="three dice")
X4<-sample(1:6,replace=TRUE,10000)
X5<-sample(1:6,replace=TRUE,10000)
hist(X1+X2+X3+X4+X5,breaks=4.5:30.5,main="", xlab="five dice")
mean(X1+X2+X3+X4+X5) # 확률변수 X1+X2+X3+X4+X5의 평균
sd(X1+X2+X3+X4+X5) # 확률변수 X1+X2+X3+X4+X5의 표준편차
lines(seq(1,30,0.1),dnorm(seq(1,30,0.1),17.5937,3.837668)*100000) # 평균 17.5937, 3.837668인 정규분포의 확률밀도함수

