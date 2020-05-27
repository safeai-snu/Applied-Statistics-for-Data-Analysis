# 제3장 확률분포 (Probability Distributions) 


# R코드 예제: 매개변수 값에 따른 이항분포

par(mfrow=c(3,2)) # 3 by 2로 plot 분할
barplot(dbinom(0:5,5,0.5),names=as.character(0:5),xlab="x",ylab="f(x)", main="n=5, p=0.5") # n=5, p=0.5 인 이항분포의 막대그래프
barplot(dbinom(0:20,20,0.2),names=as.character(0:20),xlab="x",ylab="f(x)", main="n=20, p=0.2")
barplot(dbinom(0:10,10,0.5),names=as.character(0:10),xlab="x",ylab="f(x)", main="n=10, p=0.5")
barplot(dbinom(0:20,20,0.5),names=as.character(0:20),xlab="x",ylab="f(x)", main="n=20, p=0.5")
barplot(dbinom(0:20,20,0.5),names=as.character(0:20),xlab="x",ylab="f(x)", main="n=20, p=0.5")
barplot(dbinom(0:20,20,0.8),names=as.character(0:20),xlab="x",ylab="f(x)", main="n=20, p=0.8")


# R코드 예제: 정규분포

curve(dnorm(x,0,0.5),-5,5,ylab="f(x)") # 평균 0, 표준편차 0.5인 정규분포
curve(dnorm(x,0,1),-5,5,col="red",add=TRUE) # 평균 0, 표준편차 1인 정규분포, 빨간 선
curve(dnorm(x,0,2),-5,5,col="blue",add=TRUE) # 평균 0, 표준편차 2인 정규분포, 파란 선
legend("topright", inset=.05, cex = 1, lty=c(1,1), lwd=c(2,2), c("sd=0.5","sd=1","sd=2"), 
       col=c("black", "red","blue")) # plot 오른쪽 상단에 범례 추가


# R 예제 : t- 분포가 표준정규분포에 수렴     # 자유도 1인 t 분포    
plot(function(x) dt(x, df = 1), -5, 5, ylim = c(0, 0.42),main="t-Density",ylab="f")     
plot(function(x) dt(x, df = 10), -5, 5, col="blue",add=TRUE) # 자유도 10인 t 분포 
plot(function(x) dt(x, df = 30), -5, 5, col="green",add=TRUE) # 자유도 30인 t 분포   
plot(function(x) dnorm(x), -5, 5,  col="red",add=TRUE) # 정규 분포    
text(0,0.3,"df=1");text(0,0.35,"df=10"); text(0,0.4,"df=30");text(1.2,0.4,"normal");        
arrows(1.1,0.39,0.6,0.35) # 원하는 위치에 화살표 그림 삽입   


