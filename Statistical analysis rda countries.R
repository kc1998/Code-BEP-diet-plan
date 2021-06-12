rm(list = ls())
library(ggplot2)
library(readxl)
install.packages("ggstatsplot")
library(ggstatsplot)
install.packages("tidyverse")
library(tidyverse)

rda_countries <- read_excel("RDA_Countries.xlsx")
rda_countries <- as.data.frame(rda_countries)

summary(rda_countries)

# Make countries the index of rda_countries
rownames(rda_countries) <- rda_countries$Country
rda_countries$Country <- NULL # delete column countries

rda_countries

class(rda_countries$`Energy (kcal)`)
sapply(rda_countries, class)
mean(rda_countries$`Energy (kcal)`)

# Change every nutrient column from character to numeric if needed, except for the FA columns and Country,
# Because those values mostly contain < signs.
rda_countries$`Carbohydrate (g)` <- as.numeric(rda_countries$`Carbohydrate (g)`)
rda_countries$`Protein (g)` <- as.numeric(rda_countries$`Protein (g)`)
rda_countries$`Fiber (g)` <- as.numeric(rda_countries$`Fiber (g)`)
rda_countries$`Vitamin B5 Pantothenic acid (mg)` <- as.numeric(rda_countries$`Vitamin B5 Pantothenic acid (mg)`)
rda_countries$`Chloride (mg)` <- as.numeric(rda_countries$`Chloride (mg)`)
rda_countries$`Sodium (mg)` <- as.numeric(rda_countries$`Sodium (mg)`)

summary(rda_countries)

summary(rda_countries$`Energy (kcal)`)
summary(rda_countries$`Vitamin B6 (mg)`)
median(rda_countries$`Energy (kcal)`)
boxplot(rda_countries$`Carbohydrate (g)`, ylab = "Energy (kcal)", las = 2)
boxplot(rda_countries$`Vitamin B6 (mg)`, ylab = "Energy (kcal)")

plot(rda_countries$`Energy (kcal)`)
text(rda_countries$`Energy (kcal)`,labels = rda_countries$Country)
text(rda_countries$`Energy (kcal)`,labels = rownames(rda_countries))
bxp_protein <- boxplot(rda_countries$`Protein (g)`)
text(bxp_protein$group,
     bxp_protein$out,
     rownames(rda_countries)[which(rda_countries == bxp_protein$out, arr.ind = TRUE)[, 1]],
     pos = 4) # right of the dot


# create mean for every column except fat and plot it in boxplot
par(mfrow=c(2,2))
boxplot(rda_countries$`Energy (kcal)`, ylab = "Energy (kcal)", las = 2)
boxplot(rda_countries$`Carbohydrate (g)`, ylab = "Carbohydrate (g)", las = 2)
boxplot(rda_countries$`Protein (g)`, ylab = "Protein (g)", las = 2)
boxplot(rda_countries$`Fiber (g)`, ylab = "Fiber (g)", las = 2)

par(mfrow = c(1,1))
boxplot(rda_countries$`Water (l)`, ylab = "Water (l)", las = 2)

par(mfrow = c(2,3))
boxplot(rda_countries$`Vitamin A (µg)`, ylab = "Vitamin A (µg)", las = 2)
boxplot(rda_countries$`Vitamin B1 Thiamin (mg)`, ylab = "Vitamin B1 Thiamin (mg)", las = 2)
boxplot(rda_countries$`Vitamin B2 Riboflavine (mg)`, ylab = "Vitamin B2 Riboflavine (mg)", las = 2)
boxplot(rda_countries$`Vitamin B3 Niacin (mg)`, ylab = "Vitamin B3 Niacin (mg)", las = 2)
boxplot(rda_countries$`Vitamin B5 Pantothenic acid (mg)`, ylab = "Vitamin B5 Pantothenic acid (mg)", las = 2)
boxplot(rda_countries$`Vitamin B6 (mg)`, ylab = "Vitamin B6 (mg)", las = 2)

boxplot(rda_countries$`Vitamin B9 Folate (µg)`, ylab = "Vitamin B9 Folate (µg)", las = 2)
boxplot(rda_countries$`Vitamin B12 (µg)`, ylab = "Vitamin B12 (µg)", las = 2)
boxplot(rda_countries$`Vitamin C (mg)`, ylab = "Vitamin C (mg)", las = 2)
boxplot(rda_countries$`Vitamin D (µg)`, ylab = "Vitamin D (µg)", las = 2)
boxplot(rda_countries$`Vitamin E (mg)`, ylab = "Vitamin E (mg)", las = 2)
boxplot(rda_countries$`Vitamin K1/K2 (µg)`, ylab = "Vitamin K (µg)", las = 2)

boxplot(rda_countries$`Calcium (mg)`, ylab = "Calcium (mg)", las = 2)
boxplot(rda_countries$`Chloride (mg)`, ylab = "Chloride (mg)", las = 2)
boxplot(rda_countries$`Copper (µg)`, ylab = "Copper (µg)", las = 2)
boxplot(rda_countries$`Iron (mg)`, ylab = "Iron (mg)", las = 2)
boxplot(rda_countries$`Iodine (µg)`, ylab = "Iodine (µg)", las = 2)
boxplot(rda_countries$`Magnesium (mg)`, ylab = "Magnesium (mg)", las = 2)

boxplot(rda_countries$`Manganese (mg)`, ylab = "Manganese (mg)", las = 2)
boxplot(rda_countries$`Phosphorus (mg)`, ylab = "Phosphorus (mg)", las = 2)
boxplot(rda_countries$`Potassium (mg)`, ylab = "Potassium (mg)", las = 2)
boxplot(rda_countries$`Selenium (µg)`, ylab = "Selenium (µg)", las = 2)
boxplot(rda_countries$`Sodium (mg)`, ylab = "Sodium (mg)", las = 2)
boxplot(rda_countries$`Zinc (mg)`, ylab = "Zinc (mg)", las = 2)
par(mfrow = c(1,1))

minerals <- rda_countries[22:33]
summary(rda_countries$`Iodine (µg)`)
boxplot(minerals)

# Calculate average rda for each nutrient and ignore NAs
average_rda <- colMeans(rda_countries[sapply(rda_countries, is.numeric)], na.rm = TRUE)
average_rda


colnames(rda_countries)
# Since most countries recommend < 10% of energy gained from 
# saturated FA, set the average to < 10%.
rda_countries$`FA saturated (%)`
# Half of the countries does not have data about rda for MUFA,
# so it is hard to give an average. Seeing that every country is within
# a range of 10 < 20, this range will be used as the % value for MUFA per day.
rda_countries$`FA mono (%) MUFA`
# There are a lot of lower and upper bounds for PUFA. Take the
# average of these bounds and sum the averages with the constant value.
# The average is then 7.06%. Another possibility is to take the range 3-12%,
# then every country's rda is satisfied.
rda_countries$`FA poly (%) PUFA`

n <- rownames(rda_countries)
trans_rda_countries <- as.data.frame(t(rda_countries[, -1]))
colnames(rda_countries) <- n
trans_rda_countries
str(trans_rda_countries)

write.table(trans_rda_countries, "transposed_rda_data.txt", sep = ",", quote = FALSE, row.names = TRUE)

# normalize, standardize rda data and create line chart
# to look for common values