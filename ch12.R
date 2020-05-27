# 제 12장 다중회귀분석 

# R 예제: 회귀분석

Credit.data <- read.csv("Credit.csv")
head(Credit.data)
pairs(~Balance+Income+Limit+Rating+Cards+Age+Education, data=Credit.data)

reg.fit= lm(Balance ~ ., data=Credit.data) # 다중회귀분석 
summary(reg.fit) # 회귀분석 요약 
coefficients(reg.fit) # 모델의 계수
confint(reg.fit, level=0.95) # 모델 매개변수의 신뢰구간 
fitted(reg.fit) # 예측값
residuals(reg.fit) # 잔차
Cov=vcov(reg.fit); Cov # 분산-공분산 행렬 
cov2cor(Cov)   # 상관관계 행렬
influence(reg.fit) # regression diagnostics

anova(reg.fit) # 분산분석


# R 예제: 범주형 없는 회귀분석

head(mtcars)
reg.fit= lm(mpg ~ drat, data=mtcars) # 회귀분석 
summary(reg.fit)  # 회귀분석 요약

# R 예제: 범주형 있는 회귀분석

reg.fit= lm(mpg ~ drat+vs, data=mtcars) # 회귀분석 
summary(reg.fit)  # 회귀분석 요약


# R 예제: 교호작용 회귀분석

sales.data <- read.csv("sales.csv", header=TRUE)
head(sales.data)
boxplot(Sales ~ AdExp+Price, data=sales.data)


# R 예제: 교호작용 없는 회귀분석

reg.fit <- lm(Sales ~ Price + AdExp, data=sales.data) # 교호작용 없는 회귀분석 
summary(reg.fit)  # 회귀분석 요약

# R 예제: 교호작용 있는 회귀분석

reg.fit2 <- lm(Sales ~ Price*AdExp, data=sales.data) # 교호작용 있는 회귀분석 
summary(reg.fit2)  # 회귀분석 요약


# R 예제: 모형선택

install.packages('ISLR') # ISLR 패키지 설치
library(ISLR) # 라이브러리 켜기
head(Hitters)
fix(Hitters) # 데이터 편집 
names(Hitters) # Hitters 데이터의 각 컬럼의 names
dim(Hitters) # Hitters 데이터의 차원
sum(is.na(Hitters$Salary)) # Hitters$Salary에서 NA값이 몇개인지 세기
Hitters =na.omit(Hitters) # NA값이 포함된 행을 지운 후 이것을 Hitters로 저장 
dim(Hitters) # 322개의 행에서 263개의 행이 됨
install.packages('leaps') # leaps 패키지 설치
library (leaps) # leaps 라이브러리 켜기
reg.fit.full=regsubsets(Salary ~ .,Hitters) # 회귀분석
summary(reg.fit.full) # 회귀분석 요약
reg.fit.full=regsubsets(Salary ~.,data=Hitters,nvmax =19) # 모형선택함수 
reg.sum = summary(reg.fit.full)
names(reg.sum)
reg.sum$rsq # 각 모형의 r-square 값 
par(mfrow =c(2,2))
plot(reg.sum$rss ,xlab=" Number of Variables ",ylab=" RSS",type="l") #  각 모형의 Residual sum of squares
plot(reg.sum$adjr2 ,xlab =" Number of Variables ",ylab=" Adjusted RSq",type="l") # Adjusted r-squared
max = which.max(reg.sum$adjr2) # Adjusted r-squared의 최대값의 위치 
points(max, reg.sum$adjr2[max], col ="red",cex =2, pch =20) # Adjusted r-squared 가 최대가 되는 지점 표시 
plot(reg.sum$cp ,xlab =" Number of Variables ",ylab="Cp",type="l") # Mallows'Cp
min = which.min(reg.sum$cp) # Mallows'Cp 의 최소값의 위치
points(min, reg.sum$cp[min], col ="red",cex =2, pch =20) # Mallows'Cp 의 최소가 되는 지점 표시  
plot(reg.sum$bic ,xlab=" Number of Variables ",ylab=" BIC", type="l") # Schwartz's information criterion, BIC
min2 = which.min (reg.sum$bic) # BIC의 최소값의 위치
points(min2, reg.sum$bic[min2], col =" red",cex =2, pch =20) # BIC가 최소가 되는 지점 표시

plot(reg.fit.full ,scale ="r2") 
plot(reg.fit.full ,scale ="adjr2")
plot(reg.fit.full ,scale ="Cp")
plot(reg.fit.full ,scale ="bic")

coef(reg.fit.full ,6) # 모형의 계수

reg.fit.forward=regsubsets(Salary ~.,data=Hitters ,nvmax =19,method ="forward")  # 모형선택함수 - 전진선택법
summary(reg.fit.forward)
reg.fit.backward=regsubsets(Salary ~.,data=Hitters ,nvmax =19, method ="backward")  # 모형선택함수 - 후진제거법
summary(reg.fit.backward)
coef(reg.fit.full ,7) # 모형의 계수
coef(reg.fit.forward ,7) # 모형의 계수
coef(reg.fit.backward ,7) # 모형의 계수

# R 예제: 교차 타당성(CV)

set.seed (1) # reproducible random result를 원하는 경우 시점을 고정

train= sample(c(TRUE,FALSE), nrow(Hitters), rep=TRUE) # TRUE or FALSE을 nrow(Hitters)개 샘플링 for training set
test = (!train) # 샘플링 된 값들 중 TRUE 는 FALSE로, FALSE는 TRUE로 바꾼다 for test set
reg.fit.best=regsubsets (Salary ~., data=Hitters [train ,],nvmax =19) # training set 으로 모델선택


predict.regsubsets = function (obj ,new.data ,id ,...){ # 함수가 obj와 new.data, id를 받음
  formula=as.formula (obj$call [[2]])
  matrix=model.matrix (formula ,new.data ) # 모델 매트릭스
  coef =coef(obj ,id=id) # obj 모델의 계수
  x.vars =names (coef ) # 선택된 변수
  matrix[,x.vars ]%*%coef # (선택된 변수에 해당하는 모델 매트릭스%*%계수) 값
} 


val.err =rep(NA ,19) # val.err 초기화

for(i in 1:19){
  predict=predict.regsubsets(reg.fit.best,Hitters[test,], id=i)
  val.err [i]= mean((Hitters$Salary[test]-predict)^2)
} # val.err의 i 번째 원소를 i번째 error 값으로 저장 

val.err # for loop이 실행된 후의 val.err

min = which.min (val.err) # error가 최소가 되는 위치
coef(reg.fit.best ,min) # error가 최소가 되는 reg.fit.best 모델의 계수


# K-Fold cross-validation 

k=10 # k 지정
set.seed (1) # reproducible random result를 원하는 경우 시점을 고정
k.folds= sample (1:k,nrow(Hitters),replace =TRUE) # Hitters 자료를 k개로 셋으로 분할
cv.err =matrix(NA ,k,19, dimnames =list(NULL , paste (1:19))) # 초기화
for(j in 1:k){
  fit.best =regsubsets(Salary ~.,data=Hitters [k.folds !=j,], nvmax =19) # k-1 개로 model fitting
  for(i in 1:19) {
    pred=predict.regsubsets(fit.best ,Hitters [k.folds ==j,], id=i) # 나머지 1개 set으로 predict
    cv.err [j,i]=mean( (Hitters$Salary[k.folds ==j]-pred)^2) # err의 평균값을 cv.err[j,i]에 저장
  }
}
mean.cv.err = apply(cv.err ,2, mean) # cv.err 매트릭스의 컬럼별 평균을 mean.cv.err에 저장
mean.cv.err
par(mfrow =c(1,1)) # plot 화면을 1 by 1 로
plot(mean.cv.err ,type="b") # err mean 플라팅
reg.fit.best=regsubsets (Salary ~ .,data=Hitters , nvmax =19) # 모델 선택 함수
coef(reg.fit.best ,11) # reg.fit.best 모델의 계수


# R 예제: 다중공선성

edu.data <- read.csv("coleman.csv")
View(edu.data)
reg.y = lm(formula = Test ~., data = edu.data) # 선형회귀모델 (Test~.)
summary(reg.y)  # 회귀분석 요약
mother.y = lm(Mother ~.-Test, data = edu.data) # 회귀분석 (Mother ~. -Test) VIF 계산을 위해 필요함
VIF = 1/(1-summary(mother.y)$r.squared); VIF # VIF
pairs(edu.data) # Mother 변수와 Wcol, SES 변수가 큰 상관관계를 갖는 것 확인
round(cor(edu.data),2)  # 상관관계 행렬



# R 예제: 영향력이 큰 관측치, 이상치 있는 경우

install.packages('car')
library(car)
scatterplot(repwt ~ weight|sex, smooth=FALSE, labels=rownames(Davis), data=Davis) # 성별, weight에 대한 repwt 산점도
scatterplot(weight ~ repwt|sex, smooth=FALSE, labels=rownames(Davis), data=Davis) # 성별, repwt에 대한 weight 산점도
sex.factor <- factor(Davis$sex, levels=c("M","F")) # Male=0; Female=1 로 변환시킨 뒤 sex.factor에 저장
reg <- lm(repwt ~ weight*sex.factor, data=Davis) # 회귀분석
summary(reg) # 회귀분석 요약
reg2.before <- lm(weight ~ repwt*sex.factor, data=Davis) # 회귀분석
summary(reg2.before) # 회귀분석 요약

# R 예제: 이상치 제거한 경우

fix(Davis) # 데이터 편집 
Davis[11:13,] # 편집 되었는지 확인
reg.fix <- update(reg) # 회귀분석 수정
summary(reg.fix) # 회귀분석 요약

reg2.after <- lm(weight ~ repwt*sex.factor, data=Davis) # 회귀분석
summary(reg2.after) # 회귀분석 요약

# R 예제: 이상치 판정

# fix(Davis) # 데이터 편집
reg <- lm(repwt ~ weight*sex.factor, data=Davis)
plot(hatvalues(reg))# hat-값
hat.m = mean(hatvalues(reg))
abline(h=c(2,3)*hat.m)  # hat-값의 평균의 두배, 세배인 지점
max(hatvalues(reg)); which.max(hatvalues(reg)) # hat-값의 최대값, 그리고 그 위치
outlierTest(reg) # Bonferroni 이상치 테스트
qqPlot(reg, simulate=TRUE) # QQ-plot
reg.inf = influence.measures(reg) # influential cases 감지
summary(reg.inf)
layout(matrix(c(1,2,3,4),2,2)) # plot 화면 분할
plot(reg)


# R 예제: 회귀분석진단(Regression Diagnostics)


# 원 회귀모형 (Original fit) vs 로그변환된 회귀모형(Log transformed fit)
library(car)
SLID.data <- read.csv("SLID.csv")
head(SLID.data)
reg.fit <- lm(compositeHourlyWages ~ sex + age + yearsEducation, data=SLID.data) # 회귀분석
reg.log.fit <- lm(log2(compositeHourlyWages) ~ sex + age + yearsEducation, data=SLID.data) # log변환된 회귀분석
summary(reg.fit) # 회귀분석 요약
layout(matrix(c(1,2,3,4),2,2)) # plot 화면 분할
plot(reg.fit)
summary(reg.log.fit) # 로그 회귀분석 요약
layout(matrix(c(1,2,3,4),2,2)) # plot 화면 분할
plot(reg.log.fit)

# 이상치 판정
outlierTest(reg.fit) # Bonferonni p-값 for most extreme obs
# 영향력 큰 관측값 판정 - Influential Observations added variable plots 
avPlots(reg.fit)
# Cook의 D 플랏; D 값 > 4/(n-p-1)  인지 확인
cut.off <- 4/((nrow(SLID.data)-length(reg.fit$coefficients)-2)) 
plot(reg.fit, which=4, cook.levels=cut.off)
# Influence Plot 
influencePlot(reg.fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# 정규분포성 판정 
qqPlot(reg.fit, main="QQ Plot")
# 표준화된 잔차의 분포
library(MASS)
stud.residuals <- studres(reg.fit) # 표준화된 잔차 
hist(stud.residuals, freq=FALSE, 
     main="Distribution of Studentized Residuals") # 표준화된 잔차의 분포 히스토그램
fit.x<-seq(min(stud.residuals),max(stud.residuals),length=40) 
fit.y<-dnorm(fit.x) 
lines(fit.x, fit.y) # 정규분포
# 이분산성 판정
ncvTest(reg.fit)
ncvTest(reg.log.fit)
# plot 표준화된 잔차 vs. fitted values 
spreadLevelPlot(reg.fit)
spreadLevelPlot(reg.log.fit)
# 다중공선성 판정
vif(reg.fit) # VIF - variance inflation factors 
sqrt(vif(reg.fit)) > 2 # 문제가 있는가?
vif(reg.log.fit) # VIF - variance inflation factors 
sqrt(vif(reg.log.fit)) > 2 # 문제가 있는가?
# 비선형성 판정 
crPlots(reg.fit)
crPlots(reg.log.fit)
# Ceres plots 
ceresPlots(reg.fit)
ceresPlots(reg.log.fit)
# 자기상관성 판정
durbinWatsonTest(reg.fit)
durbinWatsonTest(reg.log.fit)
