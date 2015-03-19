wine = read.csv("wine.csv")

str(wine)
summary(wine)

# create a linear model
model1 = lm(Price ~ AGST, data = wine)
summary(model1)

model1$residuals
SSE = sum(model1$residuals^2)
SSE

# build a multilinear model
model2 = lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)

SSE = sum(model2$residuals^2)
SSE

# question
model3 = lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(model3)

# compute correlation

cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)

model4 = lm(Price ~ HarvestRain + WinterRain + Age + AGST, data = wine)
wineTest = read.csv("wine_test.csv")
str(wineTest)

predictTest = predict(model4, newdata = wineTest)
predictTest

SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
R2 = 1 - SSE/SST
R2
