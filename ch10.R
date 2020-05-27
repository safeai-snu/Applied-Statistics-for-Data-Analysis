# 제10장 범주형 자료 및 비모수 통계

# R 예제: 다항분포의 적합도 검정1 (Goodness of Fit)
library(MASS)
levels(survey$Smoke) 
smk.frq = table(survey$Smoke); smk.frq # smoke 데이터의 빈도수
smk.prb = c(.045, .795, .085, .075) # smoke 데이터의 확률분포
chisq.test(smk.frq, p=smk.prb) # 적합도 검정 - 카이제곱 검정


# R 예제: 다항분포의 적합도 검정2 (Goodness of Fit)
matrix <- as.table(rbind(c(762, 327, 468), c(484, 239, 477))) # 데이터 행렬 생성
dimnames(matrix) <- list(gender = c("F", "M"),party = c("Democrat","Independent", "Republican"))
matrix
(Xsq <- chisq.test(matrix)) # 검정 결과 요약 출력
Xsq$observed   # 관측치
Xsq$expected   # 예측치

# R 예제: 부호 검정
score <- read.csv("score.csv")
diff = sign(score$Midterm-score$Final); t=sum(diff==0);
median=sum(diff>0); median # 중앙값
n=length(diff)-t; n # 표본크기
binom.test(median, n) # Exact binomial test

# R 예제: Wilcox Signed - Rank 검정
score <- read.csv("score.csv")
wilcox.test(score$Midterm, score$Final, paired = TRUE)

# R 예제: Mann - Whitney - Wilcoxon
logic = mtcars$am == 0
mpg.data.auto = mtcars[logic,]$mpg; mpg.data.auto # 자동 변속 경우의 마일리지
mpg.data.manual = mtcars[!logic,]$mpg ; mpg.data.manual # 수동 변속 경우의 마일리지
wilcox.test(mpg.data.auto, mpg.data.manual) # Wilcox 검정
wilcox.test(mpg ~ am, data=mtcars) # Wilcox 검정

# R 예제: Kruskal - Willis 검정
head(airquality)
require(graphics) # 패키지 실행 및 부착
boxplot(Ozone~Month, data = airquality) # Month변수에 따른 Ozone 변수에 대한 상자그림 플라팅
kruskal.test(Ozone ~ Month, data = airquality) # kruskal 검정

