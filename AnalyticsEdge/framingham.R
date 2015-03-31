framingham = read.csv("framingham.csv")
str(framingham)

library(caTools)
set.seed(1000)
# 50% to 80% of the data to the training set
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

training = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)

framinghamLog = glm(TenYearCHD~., data = training, family = binomial)
predictTest = predict(framinghamLog, type = "response", newdata = test)
table(test$TenYearCHD, predictTest >  0.5)

library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
