# C제11장 단순회귀분석


# R 예제: 단순회귀분석(학생시험성적)

score <- read.csv("score.csv"); 
y = score$Final; x = score$Midterm;
head(score)
reg.y = lm(y~x) # 회귀분석
summary(reg.y) # 회귀분석 요약

anova(reg.y) # 분산분석


# R 예제: 단순회귀분석 faithful

head(faithful)
x=faithful$waiting; y=faithful$eruptions
reg_y= lm(y ~ x) # 회귀분석
plot(x,y)
abline(reg_y)
summary(reg_y) # 회귀분석 요약
anova(reg_y) # 분산분석

coef = summary(reg_y)$coefficients; coef  # 추정된 회귀식의 파라미터
n=length(y); n
alpha = .05;
t.half.alpha = qt(1-alpha/2, df=n-2);t.half.alpha
CI0 = c(-t.half.alpha, t.half.alpha) 
coef[1,1]+CI0*coef[1,2] # 신뢰구간 for b0
coef[2,1]+CI0*coef[2,2] # 신뢰구간 for b1

new.X1 = data.frame(x=85)
predict(reg_y, new.X1, interval="confidence") # 평균 반응치에서의 신뢰구간
predict(reg_y, new.X1, interval="predict")   # 예측구간

new.X2 <- data.frame(x = seq(10, 50, 1))
predict(lm(y ~ x), new.X2, se.fit = TRUE)
pred.w.p.lim <- predict(lm(y ~ x), new.X2, interval = "prediction")
pred.w.c.lim <- predict(lm(y ~ x), new.X2, interval = "confidence")
matplot(new.X2$x, cbind(pred.w.c.lim, pred.w.p.lim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")



# R 예제: 회귀분석(Ivy league grade)

Ivy.grade.data <- read.csv("Ivy_grade.csv")
head(Ivy.grade.data)
x=Ivy.grade.data$Year; y=Ivy.grade.data$Grade;
reg_y=lm(y~x) # 회귀분석
summary(reg_y) # 회귀분석 요약
anova(reg_y) # 분산분석

# R 예제: 회귀분석진단


opar<-par(mfrow = c(2,2))
plot(reg_y, las=1) # Residuals vs Fitted, Normal Q-Q, Scale-Location, Residuals vs Leverage 플라팅
par(opar) # 이전 파라미터 세팅으로 복구


# R 예제: 변수 변환


elec.data = read.csv("Electricity.csv")
head(elec.data)
x=elec.data$Usage;y=elec.data$Demand
plot(x,y)  # 플라팅
reg_y = lm(y ~ x) # 회귀분석 
summary(reg_y) # 회귀분석 요약
anova(reg_y) # 분산분석

sqrt.y = sqrt(y) # 분산을 안정화 시키기 위한 Transform
reg_y2 = lm(sqrt.y ~ x) # 회귀분석
summary(reg_y2) # 회귀분석 요약
anova(reg_y2) # 분산분석 
opar <- par(mfrow = c(1,2))
plot(reg_y, which=1);plot(reg_y2, which=1) # Residuals vs Fitted 플라티
par(opar) # 이전 파라미터 세팅으로 복구


# R 예제: 상관계수 검정

X1 = c(34,37,38,20,24,79,202,236,233,214)
X2 = c(3301, 3821, 3867, 5181, 6544, 7965, 9181, 11523, 10662, 9988)
cor.test(X1,X2) # 상관계수 검정


# R 예제: 상관성 검정
head(USJudgeRatings) # "USJudgeRatings" 데이터 윗 부분 보기
require(graphics) # 패키지 실행 및 부착
pairs(USJudgeRatings) # 각 변수별 산점도 플라팅
cor.test(~ CONT + INTG, data = USJudgeRatings) # 상관성 검정

