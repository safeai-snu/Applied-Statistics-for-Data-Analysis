# 제9장 실험계획법과 분산분석 

# R 예제: 일원 분산분석 

# data <- read.csv("Pomitech.csv") 
data <- read.csv("assemble.csv") 
head(data)
y= c(as.matrix(data))
factor.data=c("method1", "method2", "method3") # 요인
k=3;n=7;
method=gl(k, n, n*k, factor(factor.data));method # 요인 생성
anova.fit = aov(y~method)  # 분산분석
summary(anova.fit)  # 요약표
boxplot(y~method,ylab="number")

# R 예제: 신뢰구간
install.packages("multcomp")
library(multcomp)
anova.fit = aov(y~method-1) # intercept 제거
glht(anova.fit) # 선형 가설 생성
plot(glht(anova.fit)) # 신뢰구간 플라팅


# R 예제: 진단그림
layout(matrix(c(1,2,3,4),2,2)) # 레이아웃 분할
plot(anova.fit) # 진단 그림 플라팅
bartlett.test(y~method) # Bartlett 테스트 - 등분산성 검정
fligner.test(y~method) # fligner 테스트 - 등분산성 검정


# R 예제: 다중검정에서 본페로니 방법 Tukey의 정직유의 차이방법 (HSD) 
pairwise.t.test(y,method, p.adj = "bonf")
TukeyHSD(anova.fit) # 정직유의차이방법 - Multiple Comparisons: Tukey Honestly Significant Differences
plot(TukeyHSD(anova.fit))



# R 예제: 임의 효과 모형
# data <- read.csv("Textile.csv")
# attach(dataframe)
# y<-c(L1,L2,L3,L4)
# loom<-factor(rep(1:4,c(4,4,4,4)))
# aov.rfit = aov(y~Error(loom)) 
# summary(aov.rfit)  #show the summary table
# print(model.tables(aov.rfit,"means"),digits=3)
# boxplot(y~loom,names=c("L1","L2","L3","L4"))

data <- read.csv("spinner.csv")
data
y<-c(data$L1,data$L2,data$L3,data$L4) # 직기
machine.data<-factor(rep(1:4,c(5,5,5,5))) # 요인
boxplot(y~machine.data,names=c("L1","L2","L3","L4")) # 별 상자그림
aov.rfit = aov(y~Error(machine.data)) 
summary(aov.rfit)
anova.fit = aov(y~machine.data)  # 분산분석
summary(anova.fit)  # 요약표
print(model.tables(anova.fit,"means"),digits=3) # Grand mean


# R 예제: 임의 블록 설계
data <- read.csv("Fabric.csv")
data
y= c(t(as.matrix(data)))
factor.data=c("C1", "C2", "C3", "C4") # 요인
k=4;n=5;
chem.data=gl(k, 1, n*k, factor(factor.data));chem.data
block = gl(n, k, k*n); block # 요인 블로킹
anova = aov(y ~ block + chem.data ) # 블록 주도 분산분석 
summary(anova) # 요약표 
layout(matrix(c(1,2,3,4),2,2)) # 레이아웃 분할
plot(anova) # 분산분석 결과 플라팅

anova = aov(y ~ chem.data + block) # 요인주도 분산분석 
summary(anova) # 요약표 
layout(matrix(c(1,2,3,4),2,2)) # 레이아웃 분할
plot(anova) # 분산분석 결과 플라팅

TukeyHSD(anova) # Tukey의 정직유의 차이방법 (HSD) 
plot(TukeyHSD(anova)) # 신뢰구간
anova = aov(y ~ chem.data -1) # intercept 제거
glht(anova) # 선형 가설 생성
plot(glht(anova)) # 플라팅

# R 예제: 이원분산분석

data <- read.csv("TEPS.csv")
y= c(t(as.matrix(data))); y
factor1 = c("one-month", "six-month", "one-year")   # 1st 요인수준 
factor2 = c("business", "engineering", "sciences") # 2nd 요인수준
n1 = length(factor1) # 첫번째 요인의 개수 
n2 = length(factor2) # 두번째 요인의 개수 
n = 3 # 처리당 관찰 표본수
college.data = gl(n1, 1, n*n1*n2, factor(factor1)); college.data # factor 생성
program.data = gl(n2, n*n1, n*n1*n2, factor(factor2)); program.data # factor 생성
anova = aov(y ~ college.data*program.data) # 분산분석 - 교작용 포함 
summary(anova) # 요약표 
interaction.plot(college.data, program.data, y)
interaction.plot(program.data, college.data, y)

#다른 데이터
data <- read.csv("GMAT.csv")
y= c(t(as.matrix(data[,-1]))); y
factor1 = c("business", "engineering", "arts and sciences") # 1st 요인수준 
factor2 = c("three-hour", "one-day", "10-week") # 2nd 요인수준
n1 = length(factor1) # 첫번째 요인의 개수 
n2 = length(factor2) # 두번째 요인의 개수 
n = 2 # 처리당 관찰 표본수
college.data = gl(n1, 1, n*n1*n2, factor(factor1)); college.data # factor 생성
prgrm.data = gl(n2, n*n1, n*n1*n2, factor(factor2)); prgrm.data # factor 생성
anova = aov(y ~ college.data*prgrm.data) # 분산분석 - 교작용 포함 
summary(anova) # 요약표 

