quality = read.csv("quality.csv")

str(quality)
table(quality$PoorCare)

# baseline accuracy
base.acc = 98 / 131

library(caTools)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)

qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = "binomial")
summary(QualityLog)

predictTrain = predict(QualityLog, type = "response")
summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean)

QualityLog2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = "binomial")
summary(QualityLog2)

table(qualityTrain$PoorCare, predictTrain > 0.5)
sensitivity = 10 / (10+15)
specificity = 70 / (70+4)

library(ROCR)
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))

predictTest = predict(QualityLog, type="response", newdata = qualityTest)

# computing the test set AUC
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
