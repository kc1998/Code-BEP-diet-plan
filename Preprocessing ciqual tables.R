rm(list = ls())

# Old Directory: "C:/Users/s166927/Documents"
library("readxl")
library("tidyverse")
install.packages("missForest")
library(missForest)
getwd()

data <- read_excel("Table Ciqual 2020_ENG_2020 07 07.xls")

colnames(data)

# first determine if row contains <
# secondly, if value == 1, change the number


# delete commas
# need to combine traces and < 
# hyphens are coerced into being NA

clean_this_columns <- data[10:76] # 10: 76 Select columns that need to be cleaned
clean_this_columns <- as.data.frame(clean_this_columns) # Make dataframe of 'clean_this_columns'
clean_this_columns

for (i in 1:length(clean_this_columns)){ # Loop over all columns
  true_false <- sapply("< ", grepl, clean_this_columns[,i]) # Create a column that stores whether or not value contains '<' sign using TRUE and FALSE
  clean_this_columns[ , ncol(clean_this_columns) + 1] <- true_false # Add the column to 'clean_this_columns'
  colnames(clean_this_columns)[ncol(clean_this_columns)] <- paste0("True", i) # Rename column name to true[i] for every created column
}
# Now clean_this_columns consists of 67 column components and 67 true columns
# The first component column relates to the first true column, the second component column relates to the second true column, etc.

clean_this_columns # Printing clean_this_columns shows that every new column name is '<'
colnames(clean_this_columns) # However, printing colnames shows that every new column is named True1 - True67

# changes from character to factor
# First delete '<', ',' and 'traces' and then convert columns to numeric.
# Converting to numeric first makes sure that cells containing '<', ',' and 'traces' get NA value
clean_this_columns <- as.data.frame(lapply(clean_this_columns, function(y) gsub("< ", "", y))) # Delete all '<' from the columns and convert back to data frame
clean_this_columns <- as.data.frame(lapply(clean_this_columns, function(y) gsub(",", ".", y))) # Convert commas to points
clean_this_columns <- as.data.frame(lapply(clean_this_columns, function(y) gsub("traces", "0", y))) # Convert traces to 0
clean_this_columns[1:67] <- as.data.frame(lapply(clean_this_columns[1:67], function(x) as.numeric(as.character(x)))) # Convert first 67 columns from character to numeric
# The line above produces warning messages, namely that all the hyphens and empty cells are coerced into NAs

clean_this_columns[44:48] %>% slice(1:10)
clean_this_columns[111:115] %>% slice(1:10)

for (i in (length(clean_this_columns)/2)+1:length(clean_this_columns)){ # Start at column true1 and end at column true67
  for (j in 1:nrow(clean_this_columns)){ # Go over every row in each column from outer for loop
    if (clean_this_columns[j, i] == "TRUE"){ # If value in a column is 'TRUE', then it corresponds to a cell which had a '<' sign
      clean_this_columns[j, i-67] = clean_this_columns[j,i-67]/2 # Since the component column and true column are related,
      # stay in the same row and go back 67 columns, which is the difference between each related column, and divide the value in that cell by 2
    }
  }
}# Divide by 2, because want to take the average of every < sign due to variability.
# Running the for loop gives an error, but it doesn't seem that anything went wrong

# check if it works (delete before submitting)
clean_this_columns[44:48] %>% slice(1:10)
clean_this_columns[15] %>% slice(1:30)

# calculate vitamin A and calculate vitamin K before imputing data
# Remember to delete column names retinol, vitamin k1/k2 from subset

# Select columns vitamin K1 and K2
vitamin_K <- clean_this_columns[, c("Vitamin.K1..µg.100g.", "Vitamin.K2..µg.100g.")]
# If an entire row of vitamin_K only contains NA's, then sum of this row will be NA, otherwise sum the rows
vitamin_K <- ifelse(apply(is.na(vitamin_K),1,all),NA,rowSums(vitamin_K,na.rm=TRUE))
vitamin_K
# vitamin A is retinol + 1/12*beta-carotene
vitamin_A <- clean_this_columns[, c("Retinol..µg.100g.", "Beta.carotene..µg.100g.")]
# Divide beta-carotene by 12 to convert it to retinol
vitamin_A$Beta.carotene..µg.100g. <- vitamin_A$Beta.carotene..µg.100g./12
# Same method is used when calculating vitamin K
vitamin_A <- ifelse(apply(is.na(vitamin_A),1,all),NA,rowSums(vitamin_A,na.rm=TRUE))

# Set vitamin A and K to their respective columns
clean_this_columns$Vitamin.A..µg.100g. = vitamin_A
clean_this_columns$Vitamin.K..µg.100g. = vitamin_K

# missing values
# change one value to ice cream
# First select all necessary nutrients before imputation

# add sugar and fibers, cholesterol and salt
nutrients <- clean_this_columns[, c("Energy..Regulation.EU.No.1169.2011..kcal.100g.", "Water..g.100g.", "Protein..g.100g.", 
                                    "Carbohydrate..g.100g.", "Fat..g.100g.", "FA.saturated..g.100g.", "FA.mono..g.100g.", "FA.poly..g.100g.",
                                    "Calcium..mg.100g.", "Chloride..mg.100g.", "Copper..mg.100g.", "Iron..mg.100g.", "Iodine..µg.100g.",
                                    "Magnesium..mg.100g.", "Manganese..mg.100g.", "Phosphorus..mg.100g.", "Potassium..mg.100g.",
                                    "Selenium..µg.100g.", "Sodium..mg.100g.", "Zinc..mg.100g.", "Vitamin.A..µg.100g.", "Vitamin.D..µg.100g.",
                                    "Vitamin.E..mg.100g.", "Vitamin.K..µg.100g.", "Vitamin.C..mg.100g.", "Vitamin.B1.or.Thiamin..mg.100g.", 
                                    "Vitamin.B2.or.Riboflavin..mg.100g.", "Vitamin.B3.or.Niacin..mg.100g.", "Vitamin.B5.or.Pantothenic.acid..mg.100g.",
                                    "Vitamin.B6..mg.100g.","Vitamin.B9.or.Folate..µg.100g.", "Vitamin.B12..µg.100g.", "Fibres..g.100g.",
                                    "Salt..g.100g.", "Sugars..g.100g.", "Cholesterol..mg.100g.")]
nutrients

# Select food name and food (sub)(sub)group and convert it to a dataframe
alim_group <- data %>% select(4,5,6,8)
alim_group <- as.data.frame(alim_group)
alim_group %>% slice(0:10)

# Group dataframes together
alim_group_nutrients <- cbind(alim_group, nutrients, stringsAsFactors = FALSE)
alim_group_nutrients %>% slice(0:10)

dish_group <- alim_group_nutrients[,2]
dish_group <- as.data.frame(dish_group)
unique_dishes <- unique(dish_group)
unique_dishes
nrow(unique_dishes)
length(alim_group_nutrients)

# Create a table with the current nutrient values, such that
# after applying missForest, the imputed values can be added to this table
imputed_table <- alim_group_nutrients
imputed_table[23:30, 5:10]

soups <- alim_group_nutrients[23:68,5:40]
soup.imp <- missForest(soups)
imputed_table[23:68, 5:40] <- soup.imp$ximp

mixed_salads <- alim_group_nutrients[2:22,5:40]
mixed_salads.imp <- missForest(mixed_salads)
imputed_table[2:22, 5:40] <- mixed_salads.imp$ximp

dishes <- alim_group_nutrients[69:227,5:40]
dishes.imp <- missForest(dishes)
imputed_table[69:227, 5:40] <- dishes.imp$ximp

pizzas_crepes_pies <- alim_group_nutrients[228:274,5:40]
pizzas_crepes_pies.imp <- missForest(pizzas_crepes_pies)
imputed_table[228:274, 5:40] <- pizzas_crepes_pies.imp$ximp

sandwiches <- alim_group_nutrients[275:314,5:40]
sandwiches.imp <- missForest(sandwiches)
imputed_table[275:314, 5:40] <- sandwiches.imp$ximp

savoury_pastries <- alim_group_nutrients[315:338,5:40]
savoury_pastries.imp <- missForest(savoury_pastries)
imputed_table[315:338, 5:40] <- savoury_pastries.imp$ximp

vegetables <- alim_group_nutrients[339:641,5:40]
vegetables.imp <- missForest(vegetables)
imputed_table[339:641, 5:40] <- vegetables.imp$ximp

potatoes_tubers <- alim_group_nutrients[642:692,5:40]
potatoes_tubers.imp <- missForest(potatoes_tubers)
imputed_table[642:692, 5:40] <- potatoes_tubers.imp$ximp

legumes <- alim_group_nutrients[693:730,5:40]
legumes.imp <- missForest(legumes)
imputed_table[693:730, 5:40] <- legumes.imp$ximp

fruits <- alim_group_nutrients[731:900,5:40]
fruits.imp <- missForest(fruits)
imputed_table[731:900, 5:40] <- fruits.imp$ximp

nuts_seeds <- alim_group_nutrients[901:952,5:40]
nuts_seeds.imp <- missForest(nuts_seeds)
imputed_table[901:952, 5:40] <- nuts_seeds.imp$ximp

pasta_rice_grains <- alim_group_nutrients[953:1023,5:40]
pasta_rice_grains.imp <- missForest(pasta_rice_grains)
imputed_table[953:1023, 5:40] <- pasta_rice_grains.imp$ximp

breads_and_similar <- alim_group_nutrients[1024:1079,5:40]
breads_and_similar.imp <- missForest(breads_and_similar)
imputed_table[1024:1079, 5:40] <- breads_and_similar.imp$ximp

savoury_biscuits <- alim_group_nutrients[1080:1097,5:40]
savoury_biscuits.imp <- missForest(savoury_biscuits)
imputed_table[1080:1097, 5:40] <- savoury_biscuits.imp$ximp

# the following foods do not belong to a food group
blank <- alim_group_nutrients[1098:1141,5:40]
blank.imp <- missForest(blank)
imputed_table[1098:1141, 5:40] <- blank.imp$ximp

cooked_meat <- alim_group_nutrients[1142:1274,5:40]
cooked_meat.imp <- missForest(cooked_meat)
imputed_table[1142:1274, 5:40] <- cooked_meat.imp$ximp

raw_meat <- alim_group_nutrients[1275:1440,5:40]
raw_meat.imp <- missForest(raw_meat)
imputed_table[1275:1436, 5:40] <- raw_meat.imp$ximp

delicatessen_meat_and_similar <- alim_group_nutrients[1437:1609,5:40]
delicatessen_meat_and_similar.imp <- missForest(delicatessen_meat_and_similar)
imputed_table[1437:1609, 5:40] <- delicatessen_meat_and_similar.imp$ximp

other_meat_products <- alim_group_nutrients[1610:1625,5:40]
other_meat_products.imp <- missForest(other_meat_products)
imputed_table[1610:1625, 5:40] <- other_meat_products.imp$ximp

cooked_fish <- alim_group_nutrients[1626:1688,5:40]
cooked_fish.imp <- missForest(cooked_fish)
imputed_table[1626:1688, 5:40] <- cooked_fish.imp$ximp

raw_fish <- alim_group_nutrients[1689:1794,5:40]
raw_fish.imp <- missForest(raw_fish)
imputed_table[1689:1794, 5:40] <- raw_fish.imp$ximp

cooked_seafood <- alim_group_nutrients[1795:1818,5:40]
cooked_seafood.imp <- missForest(cooked_seafood)
imputed_table[1795:1818, 5:40] <- cooked_seafood.imp$ximp

raw_seafood <- alim_group_nutrients[1819:1843,5:40]
raw_seafood.imp <- missForest(raw_seafood)
imputed_table[1819:1843, 5:40] <- raw_seafood.imp$ximp

fish_products <- alim_group_nutrients[1844:1899,5:40]
fish_products.imp <- missForest(fish_products)
imputed_table[1844:1899, 5:40] <- fish_products.imp$ximp

eggs <- alim_group_nutrients[1900:1923,5:40]
eggs.imp <- missForest(eggs)
imputed_table[1900:1923, 5:40] <- eggs.imp$ximp

meat_substitute <- alim_group_nutrients[1924:1929,5:40]
meat_substitute.imp <- missForest(meat_substitute)
imputed_table[1924:1929, 5:40] <- meat_substitute.imp$ximp

milk <- alim_group_nutrients[1930:1949,5:40]
milk.imp <- missForest(milk)
imputed_table[1930:1949, 5:40] <- milk.imp$ximp

dairy_products_and_similar <- alim_group_nutrients[1950:2086,5:40]
dairy_products_and_similar.imp <- missForest(dairy_products_and_similar)
imputed_table[1950:2086, 5:40] <- dairy_products_and_similar.imp$ximp

cheese_and_similar <- alim_group_nutrients[2087:2221,5:40]
cheese_and_similar.imp <- missForest(cheese_and_similar)
imputed_table[2087:2221, 5:40] <- cheese_and_similar.imp$ximp

cream_and_similar <- alim_group_nutrients[2222:2230,5:40]
cream_and_similar.imp <- missForest(cream_and_similar)
imputed_table[2222:2230, 5:40] <- cream_and_similar.imp$ximp

water <- alim_group_nutrients[2231:2320,5:40]
water.imp <- missForest(water)
imputed_table[2231:2320, 5:40] <- water.imp$ximp

non_alcoholic_beverages <- alim_group_nutrients[2321:2476,5:40]
non_alcoholic_beverages.imp <- missForest(non_alcoholic_beverages)
imputed_table[2321:2476, 5:40] <- non_alcoholic_beverages.imp$ximp

alcoholic_beverages <- alim_group_nutrients[2477:2525,5:40]
alcoholic_beverages.imp <- missForest(alcoholic_beverages)
imputed_table[2477:2525, 5:40] <- alcoholic_beverages.imp$ximp

# something goes wrong here
sugars_and_honey <- alim_group_nutrients[2526:2540,5:40]
sugars_and_honey.imp <- missForest(sugars_and_honey)
imputed_table[2526:2540, 5:40] <- sugars_and_honey.imp$ximp

chocolate_products <- alim_group_nutrients[2541:2571,5:40]
chocolate_products.imp <- missForest(chocolate_products)
imputed_table[2541:2571, 5:40] <- chocolate_products.imp$ximp

non_chocolate_confectionery <- alim_group_nutrients[2572:2588,5:40]
non_chocolate_confectionery.imp <- missForest(non_chocolate_confectionery)
imputed_table[2572:2588, 5:40] <- non_chocolate_confectionery.imp$ximp

jam <- alim_group_nutrients[2589:2602,5:40]
jam.imp <- missForest(jam)
imputed_table[2589:2602, 5:40] <- jam.imp$ximp

viennese_pastries <- alim_group_nutrients[2603:2625,5:40]
viennese_pastries.imp <- missForest(viennese_pastries)
imputed_table[2603:2625, 5:40] <- viennese_pastries.imp$ximp

sweet_biscuits <- alim_group_nutrients[2626:2690,5:40]
sweet_biscuits.imp <- missForest(sweet_biscuits)
imputed_table[2626:2690, 5:40] <- sweet_biscuits.imp$ximp

breakfast_cereals <- alim_group_nutrients[2691:2738,5:40]
breakfast_cereals.imp <- missForest(breakfast_cereals)
imputed_table[2691:2738, 5:40] <- breakfast_cereals.imp$ximp

cereal_bars <- alim_group_nutrients[2739:2746,5:40]
cereal_bars.imp <- missForest(cereal_bars)
imputed_table[2739:2746, 5:40] <- cereal_bars.imp$ximp

cakes_and_pastry <- alim_group_nutrients[2747:2831,5:40]
cakes_and_pastry.imp <- missForest(cakes_and_pastry)
imputed_table[2747:2831, 5:40] <- cakes_and_pastry.imp$ximp

# combine ice cream and sorbet and frozen_desserts
ice_products <- alim_group_nutrients[2832:2860,5:40]
ice_products.imp <- missForest(ice_products)
imputed_table[2832:2860, 5:40] <- ice_products.imp$ximp

#sorbet <- alim_group_nutrients[2844:2848,5:40]
#sorbet.imp <- missForest(sorbet)
#imputed_table[2844:2848, 5:40] <- sorbet.imp$ximp

#frozen_desserts <- alim_group_nutrients[2849:2860,5:40]
#frozen_desserts.imp <- missForest(frozen_desserts)
#imputed_table[2849:2860, 5:40] <- frozen_desserts.imp$ximp

butters <- alim_group_nutrients[2861:2873,5:40]
butters.imp <- missForest(butters)
imputed_table[2861:2873, 5:40] <- butters.imp$ximp

vegetable_oils <- alim_group_nutrients[2874:2905,5:40]
vegetable_oils.imp <- missForest(vegetable_oils)
imputed_table[2874:2905, 5:40] <- vegetable_oils.imp$ximp

margarines <- alim_group_nutrients[2906:2924,5:40]
margarines.imp <- missForest(margarines)
imputed_table[2906:2924, 5:40] <- margarines.imp$ximp

fish_oils <- alim_group_nutrients[2925:2928,5:40]
fish_oils.imp <- missForest(fish_oils)
imputed_table[2925:2928, 5:40] <- fish_oils.imp$ximp

other_fats <- alim_group_nutrients[2929:2935,5:40]
other_fats.imp <- missForest(other_fats)
imputed_table[2929:2935, 5:40] <- other_fats.imp$ximp

sauces <- alim_group_nutrients[2936:3010,5:40]
sauces.imp <- missForest(sauces)
imputed_table[2936:3010, 5:40] <- sauces.imp$ximp

condiments <- alim_group_nutrients[3011:3027,5:40]
condiments.imp <- missForest(condiments)
imputed_table[3011:3027, 5:40] <- condiments.imp$ximp

cooking_aids <- alim_group_nutrients[3028:3039,5:40]
cooking_aids.imp <- missForest(cooking_aids)
imputed_table[3028:3039, 5:40] <- cooking_aids.imp$ximp

salts <- alim_group_nutrients[3040:3045,5:40]
salts.imp <- missForest(salts)
imputed_table[3040:3045, 5:40] <- salts.imp$ximp

spices <- alim_group_nutrients[3046:3070,5:40]
spices.imp <- missForest(spices)
imputed_table[3046:3070, 5:40] <- spices.imp$ximp

herbs <- alim_group_nutrients[3071:3098,5:40]
herbs.imp <- missForest(herbs)
imputed_table[3071:3098, 5:40] <- herbs.imp$ximp

seaweed <- alim_group_nutrients[3099:3115,5:40]
seaweed.imp <- missForest(seaweed)
imputed_table[3099:3115, 5:40] <- seaweed.imp$ximp

foods_for_particular_nutritional_uses <- alim_group_nutrients[3116:3120,5:40]
foods_for_particular_nutritional_uses.imp <- missForest(foods_for_particular_nutritional_uses)
imputed_table[3116:3120, 5:40] <- foods_for_particular_nutritional_uses.imp$ximp

miscellaneous_ingredients <- alim_group_nutrients[3121:3147,5:40]
miscellaneous_ingredients.imp <- missForest(miscellaneous_ingredients)
imputed_table[3121:3147, 5:40] <- miscellaneous_ingredients.imp$ximp

baby_food <- alim_group_nutrients[3148:3186, 5:40]
baby_food.imp <- missForest(baby_food)
baby_food.imp$ximp
imputed_table[3148:3186, 5:40] <- baby_food.imp$ximp

imputed_table[1:10,]

imputed_table[3180:3186,]
imputed_table[2525:2545,]
imputed_table[2505:2525,]
colnames(imputed_table)
row(imputed_table)
imputed_table[3186,]
class(imputed_table)

imputed_table

numeric_table <- imputed_table[,5:40] # Select all numeric columns
numeric_table[numeric_table < 0] = 0 # Set all negative values to 0
# replace imputed table with numeric table, which has no more negative values
imputed_table[,5:40] <- numeric_table
imputed_table

install.packages("writexl")
library(writexl)

# Create an excel or csv file based on the imputed_table
write_xlsx(imputed_table, "C:\\Users\\s166927\\Documents\\Imputed_Table.xlsx")
write_csv(imputed_table, "C:\\Users\\s166927\\Documents\\Imputed_Table.csv")

