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
