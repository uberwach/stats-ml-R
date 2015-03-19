baseball = read.csv("baseball.csv")
str(baseball)

moneyball = subset(baseball, Year < 2002)
str(moneyball)
# RD = run difference = runs scored - runs allowed
moneyball$RD = moneyball$RS - moneyball$RA
plot(moneyball$RD, moneyball$W)

# build a model by that variable
WinsReg = lm(W ~ RD, data = moneyball)
summary(WinsReg)
predict(WinsReg, data.frame(RD = c(99))) 

RunsReg = lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data = moneyball)
summary(RunsReg)

predict(RunsReg, data.frame(OBP = c(0.311), SLG = c(0.405)))

OpponentReg = lm(RA ~ OOBP + OSLG, data = moneyball)
summary(OpponentReg)

predict(OpponentReg, data.frame(OOBP = c(0.297), OSLG = c(0.370)))
