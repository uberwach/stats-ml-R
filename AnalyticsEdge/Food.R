USDA = read.csv("USDA.csv")

str(USDA)
summary(USDA)

findMaxByColumn = function(data, column) {
  index = which.max(as.matrix(data[column]))
  data[index, ]
}

# Highest Calorie?
findMaxByColumn(USDA, "Calories")

# Get High Sodium foods
HighSodium = subset(USDA, Sodium > 10000)
nrow(HighSodium)
HighSodium$Description


# Find caviar
CaviarIndex = match("CAVIAR", USDA$Description)
USDA[CaviarIndex, ]
sd(USDA$Sodium, na.rm = TRUE)

# Adding variables
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=TRUE))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=TRUE))