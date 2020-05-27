# 제2장 확률변수와 베이즈정리 (Random Variables and Bayes Theorem) 

# R코드 예제: 확률밀도함수, 누적분포함수

curve(dnorm(x),-4,4)  # 정규분포의 확률밀도함수 -4부터 4까지 범위
curve(pnorm(x),-4,4)  # 정규분포의 누적분포함수 -4부터 4까지 범위


# R코드 예제: 상관관계

install.packages("mvtnorm");library(mvtnorm) # 패키지 mvtnorm 설치 및 라이브러리 켜기
par(mfrow=c(2,2)) # 2 by 2로 plot 분할
rho=0.6;cov <- matrix(c(1,rho,rho,1), ncol=2) # 2변수 분산-공분산 행렬
S=rmvnorm(n=300, mean=c(0,0), sigma=cov);plot(S, main="correlation=0.6")  # 2차원 정규결합확률분포의 상관 값에 따른 산점도 plot
rho=-0.6;cov <- matrix(c(1,rho,rho,1), ncol=2);
S=rmvnorm(n=300, mean=c(0,0), sigma=cov);plot(S, main="correlation=-0.6")
rho=0.9;cov <- matrix(c(1,rho,rho,1), ncol=2);
S=rmvnorm(n=300, mean=c(0,0), sigma=cov);plot(S, main="correlation=0.9")
rho=0.15;cov <- matrix(c(1,rho,rho,1), ncol=2);
S=rmvnorm(n=300, mean=c(0,0), sigma=cov);plot(S, main="correlation=0.15")