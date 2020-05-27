#Scores for our 50 patients (Score)
Score =c(0.01, 0.03, 0.09, 0.10, 0.10, 0.11, 0.11, 0.13, 0.20, 0.20, 0.21, 0.21, 0.24, 0.26, 0.26, 0.28, 0.29, 0.32, 0.32, 0.36, 0.38, 0.39, 0.41, 0.41, 0.43, 0.47, 0.49, 0.52, 0.52, 0.53, 0.53, 0.59, 0.60, 0.61, 0.69, 0.69, 0.73, 0.74, 0.75, 0.75, 0.79, 0.80, 0.80, 0.82, 0.85, 0.88, 0.89, 0.89, 0.93, 0.98)
# Corona: 1=yes, 0=no)
Corona=c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,1,0,0,0,1,1,1,1,1,0,0,1,1,1,0,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1)
data = data.frame(cbind(Score, Corona))

#Corona (whether it's correct or not) using our 8 thresholds.
data$yes10 = ifelse(data$Score > 0.1, 1, 0)
data$yes20 = ifelse(data$Score > 0.2, 1, 0)
data$yes30 = ifelse(data$Score > 0.3, 1, 0)
data$yes40 = ifelse(data$Score > 0.4, 1, 0)
data$yes50 = ifelse(data$Score > 0.5, 1, 0)
data$yes60 = ifelse(data$Score > 0.6, 1, 0)
data$yes70 = ifelse(data$Score > 0.7, 1, 0)
data$yes80 = ifelse(data$Score > 0.8, 1, 0)

sensitivity <-c(sum(data$yes10 == 1 & data$Corona == 1)/sum(data$yes10 ==1),
                sum(data$yes20 ==1 & data$Corona == 1)/sum(data$yes20 ==1),
                sum(data$yes30 ==1 & data$Corona == 1)/sum(data$yes30 ==1),
                sum(data$yes40 ==1 & data$Corona == 1)/sum(data$yes40 ==1),
                sum(data$yes50 ==1 & data$Corona == 1)/sum(data$yes50 ==1),
                sum(data$yes60 ==1 & data$Corona == 1)/sum(data$yes60 ==1),
                sum(data$yes70 ==1 & data$Corona == 1)/sum(data$yes70 ==1),
                sum(data$yes80 ==1 & data$Corona == 1)/sum(data$yes80 ==1))   

specificity <-c(sum(data$yes10 == 0 & data$Corona == 0)/sum(data$yes10 ==0),
                sum(data$yes20 ==0 & data$Corona == 0)/sum(data$yes20 ==0),
                sum(data$yes30 ==0 & data$Corona == 0)/sum(data$yes30 ==0),
                sum(data$yes40 ==0 & data$Corona == 0)/sum(data$yes40 ==0),
                sum(data$yes50 ==0 & data$Corona == 0)/sum(data$yes50 ==0),
                sum(data$yes60 ==0 & data$Corona == 0)/sum(data$yes60 ==0),
                sum(data$yes70 ==0 & data$Corona == 0)/sum(data$yes70 ==0),
                sum(data$yes80 ==0 & data$Corona == 0)/sum(data$yes80 ==0))

TruePositiveRate = sensitivity
FalsePositiveRate = 1 - specificity
ROCCurve = data.frame(cbind(FalsePositiveRate, TruePositiveRate))
plot(ROCCurve, main="ROC Curve for Corona Test")
lines(ROCCurve, col="red")
