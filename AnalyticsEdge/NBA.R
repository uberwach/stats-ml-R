NBA = read.csv("NBA_train.csv")
str(NBA)
table(NBA$W, NBA$Playoffs)
NBA$PTSdiff = NBA$PTS - NBA$oppPTS
plot(NBA$PTSDiff, NBA$W)

WinsReg = lm(W ~ PTSdiff, data = NBA)
summary(WinsReg)

PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)
summary(PointsReg)

PointsReg$residuals
SSE = sum(PointsReg$residuals^2)
SSE
RMSE = sqrt(SSE / nrow(NBA))
RMSE

PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA)
summary(PointsReg2)

PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB  + STL, data = NBA)
SSE = sum(PointsReg4$residuals^2)
SSE
RMSE = sqrt(SSE / nrow(NBA))
RMSE

NBA_test = read.csv("NBA_test.csv")
PointsPredictions = predict(PointsReg4, newdata = NBA_test)
SSE = sum((PointsPredictions - NBA_test$PTS)^2)
SST = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R_test = 1 - SSE/SST
R_test
