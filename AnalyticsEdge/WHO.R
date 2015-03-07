WHO = read.csv("WHO.csv")

names(WHO)
summary(WHO$Over60)

minIndex = which.min(WHO$Over60)
WHO$Country[minIndex]

maxIndex = which.max(WHO$LiteracyRate)
WHO$Country[maxIndex]

outliers = subset(WHO, GNI > 100000 & FertilityRate > 2.5)
nrows(outliers)
outliers[c("Country", "GNI", "FertilityRate")]
hist(outliers)

tapply(WHO$ChildMortality, WHO$Region, mean)