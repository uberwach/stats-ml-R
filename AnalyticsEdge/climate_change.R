climate = read.csv("climate_change.csv")
str(climate)

training = subset(climate, Year <= 2006)
test = subset(climate, Year > 2006)

climate.lm = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = training) 
summary(climate.lm)

cor(training)

climate.reduced = lm(Temp ~ N2O + TSI + Aerosols + MEI, data = training)
summary(climate.reduced)

climate.step = step(climate.lm)
summary(climate.step)

climate.pred = predict(climate.step, test)
RSS = sum( (climate.pred - test$Temp)^2)
RST = sum( (test$Temp - mean(training$Temp))^2)
R2 = 1 - RSS/RST
R2
