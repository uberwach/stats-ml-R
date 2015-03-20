pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
str(pisaTrain)

tapply(pisaTrain$readingScore, pisaTrain$male, mean)

# gives the column that contain NA values
sapply(pisaTrain, function(x) !all(!is.na(x)))

# filter them out
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
nrow(pisaTrain)
nrow(pisaTest)

# replace categorical variable with dummy variables, choose White as baseline.
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

pisa.lm = lm(readingScore ~ ., data = pisaTrain)
summary(pisa.lm)

prediction.train = predict(pisa.lm, pisaTrain)
RSS = sum(pisa.lm$residuals^2)
RMSE = sqrt(RSS / nrow(pisaTrain))
RMSE

prediction.test = predict(pisa.lm, newdata = pisaTest)
summary(prediction.test)

SSE = sum((prediction - pisaTest$readingScore)^2)
RMSE = sqrt(SSE / nrow(pisaTest))
RMSE

SST = sum( (pisaTest$readingScore - mean(pisaTrain$readingScore))^2)
R2 = 1 - SSE/SST
