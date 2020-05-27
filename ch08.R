# 제8 장 귀무가설 유의성 검정

# R 예제: 오른쪽 단측검정 z-검정

sample.mean = 65 # 표본평균
mu0 = 60 # hypothesized value
sigma = 9.5 # 표본표준편차
n0 = 15 # 표본 크기
se = sigma/sqrt(n0)
z= (sample.mean-mu0)/(sigma/sqrt(n0));  z # 검정통계량

alpha = .05 
z.alpha = qnorm(1-alpha); z.alpha # 임계값 

pvalue = 1-pnorm(z); pvalue  # 우측 꼬리 p값


# R 예제: 2종 오류(오른쪽 단측검정)


q.norm = qnorm(alpha, mean=mu0, sd=se, lower.tail=FALSE); q.norm
mu.a = 62 # 모평균의 참값
p.62 = pnorm(q.norm, mean=mu.a, sd=se); p.62 # mu.a = 62 일때 2종 오류 확률
mu.a = 64 # 모평균의 참값
p.64 = pnorm(q.norm, mean=mu.a, sd=se); p.64 # mu.a = 64 일때 2종 오류 확률
mu.a = 68 # 모평균의 참값
p.68 = pnorm(q.norm, mean=mu.a, sd=se); p.68 # mu.a = 68 일때 2종 오류 확률
mu.a = 70 # 모평균의 참값 
p.70 = pnorm(q.norm, mean=mu.a, sd=se); p.70 # mu.a = 70 일때 2종 오류 확률


# R 예제: 검정력(오른쪽 단측검정)

r_beta=function(x,n,alpha,sigma){
  se = sigma/sqrt(n) # 표준오차
  q=qnorm(alpha, mean=mu0, sd=se, lower.tail=FALSE);
  beta = pnorm(q, mean=x, sd=se)
  return(beta)
} # 검정력
mu0 = 60 # hypothesized value 
sigma = 9.5 # 표본표준편차 
n = c(1,2, 5, 10, 15, 20, 30, 40, 50, 100) # 표본크기
alpha = .05 # 유의 수준 
# 운영특성곡선 beta 의 그래프
curve(r_beta(x,15,alpha,sigma),60,70,main="OC curve", xlab="True mean",ylab="Probability of type II error: beta")
x=seq(60, 70)
for (k in 1:10)
  { lines(x,r_beta(x,n[k],alpha,sigma), lty=3); 
  text(70-4*k/5,r_beta(70-4*k/5, n[k], alpha,sigma), labels=paste("n=",n[k])) }

curve(1-r_beta(x,n[k],alpha,sigma),60,70,xlab="True mean",ylab="Power") # 검정력 함수 1-beta 의 그래프


# R 예제: 표본수(오른쪽 단측 검정)

alpha=0.05; z.alpha=qnorm(alpha, lower.tail=FALSE);z.alpha # alpha = 0.05 이고
beta=0.1; z.beta=qnorm(beta, lower.tail=FALSE);z.beta # beta = 0.1 이고
mu.a = 62;mu0=60;sigma=9.5 # 모평균의 참값이 62 일때
n_size=(z.alpha+z.beta)^2*sigma^2/(mu.a-mu0)^2;n_size # 표본수


# R 예제: 양측검정 z-검정

alpha = .05 
z.half.alpha = qnorm(1-alpha/2) 
c(-z.half.alpha, z.half.alpha) 
z=(sample.mean-mu0)/(sigma/sqrt(n0)); z # 검정통계량
pval = 2*(1-pnorm(abs(z))); pval  # 양측 p값

# R 예제: 2종 오류 (양측검정)

mu0 = 60 # hypothesized value 
sigma = 9.5 # 표본표준편차
n = 15  # 표본크기
se = sigma/sqrt(n); se # 표준오차
alpha = .05 # 신뢰수준
acc = c(alpha/2, 1-alpha/2) 
CI0 = qnorm(acc, mean=mu0, sd=se); CI0 
mu.a = 58 # 모평균의 참값
p = pnorm(CI0, mean=mu.a, sd=se); p
beta=diff(p); beta # p[2]-p[1]: 2종 오류
mu.a = 65 # 모평균의 참값
p = pnorm(CI0, mean=mu.a, sd=se); p
beta=diff(p); beta # p[2]-p[1]: 2종 오류

# R 예제: 2종 오류와 검정력 (양측검정)

t_beta=function(x,n,alpha,sigma){
  se = sigma/sqrt(n) # 표준오차
  acc = c(alpha/2, 1-alpha/2)
  q=qnorm(acc, mean=mu0, sd=se)
  beta=pnorm(q[2], mean=x, sd=se)-pnorm(q[1], mean=x, sd=se) # p[2]-p[1]: 2종 오류
  return(beta)
} # 검정력
mu0 = 60 # hypothesized value 
sigma = 9.5 # 표본표준편차 
n = c(1,2, 5, 10, 15, 20, 30, 40, 50, 100) # 표본크기
alpha = .05 # 유의 수준 

# 운영특성곡선 beta 의 그래프
curve(t_beta(x,15,alpha,sigma),50,70,main="OC curve", xlab="True mean",ylab="Probability of type II error: beta")
x=seq(50, 70)
for (k in 1:10)
{ lines(x,t_beta(x,n[k],alpha,sigma), lty=3); 
  text(71-4*k/5,t_beta(71-4*k/5, n[k], alpha,sigma), labels=paste("n=",n[k])) }

curve(1-t_beta(x,n[k],alpha,sigma),50,70,xlab="True mean",ylab="Power") # 검정력 함수 1-beta 의 그래프

# R 예제: 오른쪽 단측검정 t-검정

sample.mean = 635 # 표본평균
mu0 = 620 # hypothesized value 
sd = 21 # 표본표준편차
n = 15 # 표본크기
se = (sd/sqrt(n)); se # 표준오차
t = (sample.mean-mu0)/se; t # 검정통계량
alpha = .05
t.alpha = qt(1-alpha, df=n-1); t.alpha # 임계값
pvalue = 1-pt(t, df=n-1); pvalue # 단측 p값 

mu.a = 620+10 # 모평균의 참값
beta0 = pt(t.alpha,df = n-1,(mu.a-mu0)/se); beta0 # 2종 오류 확률 beta 값
power=1-beta0; power  # power

n=seq(15,50); se = (sd/sqrt(n))
beta_c = pt(t.alpha,df = n-1,(mu.a-mu0)/se)
plot(n, beta_c, type="l", xlab="sample size",ylab="Probability of type II error: beta")
abline(h=0.1, lty=2, lwd=2)

# R 예제: 양측검정 t-검정

alpha = .05 
t.half.alpha = qt(1-alpha/2, df=n-1)  
c(-t.half.alpha, t.half.alpha) 
pvalue = 2*(1-pt(abs(t), df=n-1)); pvalue  # 양측 p

# R 예제: 2종 오류(양측검정)

mu0 = 60 # hypothesized value 
sd = 9.5 # 표본표준편차 
n = 15 # 표본크기
se = sd/sqrt(n); se # 표준오차 
alpha = .05 # 유의수준 
acc = c(alpha/2, 1-alpha/2) 
CI0 = mu0+qt(acc,df=n-1)*se; CI0 

mu.a = 62 # 모평균의 참값
p = pt((CI0-mu.a)/se, df=n-1); p
beta=diff(p); beta # p[2]-p[1]: 2종 오류

mu.a = 65 # 모평균의 참값
p = pt((CI0-mu.a)/se, df=n-1); p
beta=diff(p); beta # p[2]-p[1]: 2종 오류



# R 예제: 두 모집단 오른쪽 단측 z-검정

sample.mean.X1 = 1.3104; sample.mean.X2 = 1.2388; # 표본평균
var.X1 = 0.00997 ; var.X2 = 0.01231 # 표본분산
n.X1 = 59 ; n.X2 = 38 # 표본크기
se = sqrt(var.X1/n.X1+var.X2/n.X2)
z = (sample.mean.X1 - sample.mean.X2)/se # 검정통계량
alpha = .05
z.alpha = qnorm(1-alpha); z.alpha # 임계값
pvalue = 1-pnorm(z); pvalue # 단측 p값

alpha = .05; z.alpha = qnorm(alpha, lower.tail = FALSE); z.alpha
beta = 0.1 ; z.beta = qnorm(beta, lower.tail = FALSE); z.beta 
n.size=(z.alpha+z.beta)^2*(var.X1+var.X2)/(sample.mean.X1-sample.mean.X2)^2;n.size # 표본수

CI.upper = sample.mean.X1-sample.mean.X2 + z.alpha*se; CI.upper # 상계 신뢰구간

alpha = .05; z.half.alpha = qnorm(1-alpha/2); z.half.alpha
CI0 = c(-z.half.alpha, z.half.alpha); CI0
CI = sample.mean.X1-sample.mean.X2 + CI0*se; CI # 양측 신뢰구간


# R 예제: 두 모집단 양측 t-검정

# Data 

mtcars
head(mtcars)
mtcars$mpg # 1974년도 미국 자동차들의 가스 마일리지 데이터
mtcars$am # 각 모델들의 변속 종류 (0 = 자동, 1 = 수동)
L = mtcars$am == 0 
mpg_auto = mtcars[L,]$mpg 
mpg_auto # 변속 종류가 자동인 모델들의 가스 마일리지 데이터
mpg_manual = mtcars[!L,]$mpg 
mpg_manual # 변속 종류가 수동인 모델들의 가스 마일리지 데이터
t.test(mpg_auto, mpg_manual, var.equal=TRUE) # t 검정 
sample.mean.auto = mean(mpg_auto); sample.mean.manual = mean(mpg_manual) # 표본평균
delta0 = 0 # hypothesized difference value 
sigma.auto = sd(mpg_auto); sigma.manual = sd(mpg_manual) # 표본표준편차 
n.auto=length(mpg_auto); n.manual=length(mpg_manual) # 표본크기 
delta.m=sample.mean.auto-sample.mean.manual
s.pooled=((n.auto-1)*sigma.auto^2+(n.manual-1)*sigma.manual^2)/(n.auto+n.manual-2) # pooled estimator of variance
df=n.auto+n.manual-2; df # 자유도
t= (delta.m-delta0)/sqrt(s.pooled*(1/n.auto+1/n.manual));  t   # 검정통계량 

alpha = .05 
t.half.alpha = qt(1-alpha/2,df); t.half.alpha
CI0 = c(-t.half.alpha, t.half.alpha)
CI = delta.m + CI0*sqrt(s.pooled*(1/n.auto+1/n.manual)); CI # 양측 신뢰구간
pvalue = 2*(1-pt(abs(t),df));  pvalue # 양측 p 값 



# R 예제: Paired t-검정

score <- read.csv("C:/DataNsoftware/courses_SNU/산업공학통계/Lecture Notes/new_R_codes/score.csv")

View(score)
head(score)
t.test(score$Midterm, score$Final, paired=TRUE) # Paired t-검정 

# R 예제: Paired t-검정 구현 코드

diff = (score$Midterm)-(score$Final); diff.mean=mean(diff); diff.mean # 차이의 표본평균
delta0 = 0 # hypothesized difference value
n=length(diff) # 표본의 크기
se = sd(diff)/sqrt(n)  # 차이의 표본평균의 표준오차
t= diff.mean/se;  t # 검정통계량 
alpha = .05 
t.half.alpha = qt(1-alpha/2,df=n-1) 
CI0=c(-t.half.alpha, t.half.alpha) 
CI=diff.mean+CI0*se; CI  # 양측 신뢰구간
pvalue = 2*(1-pt(abs(t),df=n-1)); pvalue

# R 예제: 비율 정확검정 신뢰구간
binom.test(100, 100, alternative = "greater")

# R 예제: 모비율 검정 Lower Tail Test of Population Proportion

sample.prop = 6 # 표본비율 
p0 = .04 # hypothesized value 
n = 300 # 표본 크기
se0=sqrt(p0*(1-p0)/n) # 표준오차 
z = (sample.prop/n-p0)/se0; z # 검정통계량

alpha = .05; z.alpha = qnorm(1-alpha); -z.alpha #임계값
pvalue = pnorm(z);  pvalue # 단측 p-값 

prop.test(sample.prop, n, p0, alt="less", correct=FALSE) 

# R 예제: 2종 오류와 표본수

q = p0+qnorm(alpha, 0, 1)*se0; q
p.a=.025; # 모비율의 참값
se = sqrt(p.a*(1-p.a)/n)
p.beta = pnorm(q, mean=p.a, sd=se, lower.tail = FALSE); p.beta  #2종 오류
beta = 0.1; z.beta = qnorm(beta, lower.tail = FALSE); z.beta
n.size=(z.alpha*sqrt(p0*(1-p0))+z.beta*sqrt(p.a*(1-p.a)))^2/(p.a-p0)^2;n.size # n의 크기




# R 예제: 이항분포의 두 모비율 차이에 대한 z-검정

M = matrix(c(63,37,237,163),2)
M
prop.test(M, correct = FALSE)
n.X1 = sum(M[1,]); p.X1 = M[1,1]/n.X1; p.X1 # 유학파 중 임원 이상의 비율
n.X2 = sum(M[2,]); p.X2 = M[2,1]/n.X2; p.X2 # 국내파 중 임원 이상의 비율
pooled = (M[1,1]+M[2,1])/(n.X1+n.X2) # 임원 이상의 비율
se = sqrt(pooled*(1-pooled)*(1/n.X1+1/n.X2)) # 표준오차
z = (p.X1 - p.X2)/se; z # 검정통계량

alpha = 0.05
z.half.alpha = qnorm(1-alpha/2)
CI0 = c(-z.half.alpha, z.half.alpha); CI0
CI = p.X1-p.X2 + CI0*se; CI # 양측 신뢰구
pvalue = 2*(1-pnorm(abs(z))); pvalue # 양측 p-값 


# R 예제: 단일 모분산 검정

sigma0 = 15 # hypothesized value 
sd = 12.78 # 표본표준편차
n = 30 # 표본 크기
chi= (n-1)*sd^2/sigma0^2;  chi # 검정통계량

alpha = .1 
chi.alpha = qchisq(alpha, df=n-1); chi.alpha # 임계값 
pvalue = pchisq(chi, df=n-1); pvalue # 단측 p값 




# R 예제: 두 모분산 차이에 대한 F-검정

X1 <- rnorm(50, mean = 0, sd = 2)
X2 <- rnorm(30, mean = 1, sd = 1)
var.test(X1, X2) # X1과 X2가 같은 분산을 갖는가?

sigma.X1 = sd(X1); sigma.X2 = sd(X2) # 표본표준편차 
sigma.X1; sigma.X2
n.X1=length(X1); n.X2=length(X2); n.X1-1; n.X2-1
F0=sigma.X1^2/sigma.X2^2; F0

alpha = .05 
f.rhalf.alpha = qf(1-alpha/2,n.X2-1,n.X1-1) 
f.lhalf.alpha = qf(alpha/2,n.X2-1,n.X1-1)
CI0=c(f.lhalf.alpha, f.rhalf.alpha); CI0 
CI=F0*CI0; CI  # 양측 신뢰구간
pvalue = 2*(1-pf(F0,n.X1-1,n.X2-1)); pvalue  # 양측 p-값


