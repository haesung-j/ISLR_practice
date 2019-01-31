#### Stock Market Data
library(ISLR)
names(Smarket)
dim(Smarket)              # 1250 9
summary(Smarket)          # Direction is qulitative, ow are quantitative.

# check correlation without Direction(qulitative)
cor(Smarket[, -9])        # Only Year,Volumn have substantial correlation
attach(Smarket)
plot(Year, Volume)
