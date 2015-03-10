# An Analytical Detective
mvt = read.csv("mvtWeek1.csv")

# Problem 1
nrow(mvt)
names(mvt)
max(mvt$ID)
min(mvt$Beat)
table(mvt$Arrest)
nrow(subset(mvt, LocationDescription == 'ALLEY'))

# Problem 2
head(mvt$Date)
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
median(DateConvert)

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
table(mvt$Month)
table(mvt$Weekday)
table(subset(mvt, Arrest)$Month)

# Problem 3
hist(mvt$Date, breaks = 100)
boxplot(Date ~ Arrest, data = mvt)
table(subset(mvt, Year == 2001)$Arrest)
table(subset(mvt, Year == 2007)$Arrest)
table(subset(mvt, Year == 2012)$Arrest)

# Problem 4
sort(table(mvt$LocationDescription))
Top5 = subset(mvt, is.element(LocationDescription, c("STREET", "ALLEY","PARKING LOT/GARAGE(NON.RESID.)", "GAS STATION", "DRIVEWAY - RESIDENTIAL")))
nrow(Top5)
Top5$LocationDescription = factor(Top5$LocationDescription)
tapply(as.numeric(Top5$Arrest),Top5$LocationDescription, mean)
GasStation = subset(Top5, LocationDescription == "GAS STATION")
table(GasStation$Weekday)
table(subset(Top5, LocationDescription == "DRIVEWAY - RESIDENTIAL")$Weekday)
