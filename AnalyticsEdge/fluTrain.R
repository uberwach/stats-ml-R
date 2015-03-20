fluTrain = read.csv("FluTrain.csv")
str(fluTrain)

maxIndex = which.max(fluTrain$ILI)
fluTrain[maxIndex,]

maxQueries = which.max(fluTrain$Queries)
fluTrain[maxQueries,]

hist(fluTrain$ILI)

plot(log(fluTrain$ILI) ~ fluTrain$Queries)

fluTrend1 = lm(log(ILI) ~ Queries, data = fluTrain)
summary(fluTrend1)

cor(log(fluTrain$ILI), fluTrain$Queries)

fluTest = read.csv("FluTest.csv")

predict.test = exp(predict(fluTrend1, newdata=fluTest))
str(predict.test)
ind = which(fluTest$Week == "2012-03-11 - 2012-03-17")
predicted = predict.test[ind]
observed = fluTest$ILI[ind]
rel_error = (observed - predicted) / observed
rel_error

SSE = sum((predict.test - fluTest$ILI)^2)
RMSE = sqrt(SSE / length(predict.test))
RMSE

library(zoo)
ILILag2 = lag(zoo(fluTrain$ILI), -2, na.pad=TRUE)
fluTrain$ILILag2 = coredata(ILILag2)

sum(is.na(fluTrain$ILILag2))
plot(log(fluTrain$ILILag2) ~ log(fluTrain$ILI))

flu.model2 = lm(log(ILI) ~ log(ILILag2) + Queries, data = fluTrain)
summary(flu.model2)

fluTest$ILILag2 = coredata(lag(zoo(fluTest$ILI), -2, na.pad = TRUE))
sum(is.na(fluTest$ILILag2))

fluTest$ILILag2[1] = fluTrain$ILI[nrow(fluTrain) - 1]
fluTest$ILILag2[2] = fluTrain$ILI[nrow(fluTrain)]
fluTest$ILILag2[1]
fluTest$ILILag2[2]

predictions2 = exp(predict(flu.model2, newdata = fluTest))
SSE = sum((predictions2 - fluTest$ILI)^2)
RMSE = sqrt(SSE / length(predictions2))
RMSE


# look up ARIMA models