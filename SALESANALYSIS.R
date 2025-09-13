# DATA LOADING
setwd("C:/Users/Dell") 
superstore <- read.csv("saless.csv") 
View(superstore)

details <- summary(superstore) 
print(details)

details <- str(superstore) 
print(details)

heads <- head(superstore) 
print(heads)

tails <- tail(superstore) 
print(tails)

dimension <- dim(superstore) 
print(dimension)


#DATA CLEANING
as = sum(is.na(superstore)) 
print(as)

print(is.na(superstore))

#HANDLING MISSING VALUES
# Replace missing Sales with mean
superstore$Sales <- ifelse(is.na(superstore$Sales), 
                           ave(superstore$Sales, FUN = function(x) mean(x, na.rm = TRUE)), 
                           superstore$Sales)

# Replace missing Discount with mean
superstore$Discount <- ifelse(is.na(superstore$Discount), 
                              ave(superstore$Discount, FUN = function(x) mean(x, na.rm = TRUE)), 
                              superstore$Discount)

#ROUNDING OFF SALES AND PROFITS
superstore$Sales  <- as.numeric(format(round(superstore$Sales, 0))) 
superstore$Profit <- as.numeric(format(round(superstore$Profit, 0))) 

#DATA VISUALIZTION
# 1. Histogram of Sales
hist(superstore$Sales, 
     main = "Sales Distribution", 
     col = "skyblue", 
     xlab = "Sales", 
     ylab = "Frequency", 
     border = "black")

# 2. Scatterplot (Sales vs Profit with Regression Line)
plot(superstore$Sales, superstore$Profit, 
     main = "Sales vs Profit with Regression Line", 
     col = "blue", pch = 16, 
     xlab = "Sales", ylab = "Profit")
abline(lm(Profit ~ Sales, data=superstore), col = "red")

# 3. Region-wise Sales
barplot(tapply(superstore$Sales, superstore$Region, sum), 
        main = "Region-wise Sales", 
        col = "orange", 
        xlab = "Region", 
        ylab = "Total Sales")

# 4. Profit by Discount Level
boxplot(Profit ~ Discount, data = superstore, 
        main = "Profit by Discount Level", 
        xlab = "Discount", 
        ylab = "Profit", 
        col = "lightgreen")

#CORRELATION ANALYSIS
cor(superstore$Sales, superstore$Profit, use = "complete.obs") 
cor(superstore$Discount, superstore$Profit, use = "complete.obs") 

#REGRESSION MODEL
model <- lm(Profit ~ Sales + Discount, data=superstore) 
summary(model) 



