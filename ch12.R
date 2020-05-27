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
