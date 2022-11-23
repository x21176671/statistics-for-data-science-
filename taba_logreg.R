# stats CA assignment
# multiple linear regression analysis
# Peter McEnroe
library(ggplot2)
library(ggthemes)
library(scales)
library(Amelia)
library(dplyr)
library(leaps)
library(MASS)
library(caret)
library(car)
library(haven)
library(ggplot2)
library(gridExtra)
library(regclass)
library(AICcmodavg)
library(xlsx)

setwd("college/nci/semester1/statistics_for_data_science/TABA") 
def <- read.csv("Default.csv")
# PART B LOGISTIC REGRESSION 

#tidy up variable names
names(def)[names(def) == "ï..gender"] <- "gender"
attach(def)
#find missing attributes
sapply(def,function(x) sum(is.na(x)))
# cast variables as factors where required
def[def$gender ==0,]$gender <- 'Male' 
def[def$gender ==1,]$gender <- 'Female' # convert variable from 0/1 to nominal responses 
def$gender <- as.factor(def$gender)
def[def$retire ==0,]$retire <- 'Not Retired' 
def[def$retire ==1,]$retire <- 'Retired' # convert variable from 0/1 to nominal responses 
def$retire <- as.factor(def$retire)
def[def$marital ==0,]$marital <- 'Unmarried' 
def[def$marital ==1,]$marital <- 'Married' # convert variable from 0/1 to nominal responses 
def$marital <- as.factor(def$marital)
def[def$homeown ==0,]$homeown <- 'Renter' 
def[def$homeown ==1,]$homeown <- 'Homeowner' # convert variable from 0/1 to nominal responses 
def$homeown <- as.factor(def$homeown)
def[def$default ==0,]$default <- 'No default on record' 
def[def$default ==1,]$default <- 'default on record' # convert variable from 0/1 to nominal responses 
def$default <- as.factor(def$default)

# Prelim understanding of variables in dataset 
#visualise w/ boxplots/hists/barcharts
# histograms 
par(mfrow = c(2, 5)) # Create a 2 x 5 plotting matrix
hist(def$gender, xlab = "gender", main = "", col = 'red')  # M/F, Factor
hist(def$age, xlab = "age", main = "", col = 'brown', freq = T) # int age of individual, continuous
hist(def$ed, freq = T, xlab = "years educated", main = "", col = 'cyan')  # int, yrs educated. continuous
hist(def$retire, xlab = "retired", main = "", col = 'aquamarine', freq = T) #Retired/Not Retired, factore
hist(def$income, xlab = "income ('000 ???)", main = "", col = 'blue', freq = T) # household income in thousands. continuous
hist(def$creddebt, xlab = "cred debt in '000 ???", main = "", col = 'green', freq = T) # credit debt in thousands, continuous
hist(def$othdebt, xlab = "other debt in ", main = "", col = 'purple', freq = T)  # other debt in thousands, continuous
hist(log(def$income), xlab = "income ('000 ???)", main = "", col = 'blue', freq = T) # household income in thousands. continuous
hist(log(def$creddebt), xlab = "cred debt in '000 ???", main = "", col = 'green', freq = T) # credit debt in thousands, continuous
hist(log(def$othdebt), xlab = "other debt in ", main = "", col = 'purple', freq = T)  # other debt in thousands, continuous
# boxplots 
par(mfrow = c(2, 5)) # Create a 2 x 5 plotting matrix
plot(def$gender, xlab = "gender", main = "", col = 'red', ylab ="")
boxplot(def$age, xlab = "age", main = "", col = 'brown', freq = T)
boxplot(def$ed, freq = T, xlab = "years educated", main = "", col = 'cyan')
plot(def$retire, xlab = "retired", main = "", col = 'aquamarine', ylab ="")
boxplot(log(def$income), xlab = "log(income ['000 ???])", main = "", col = 'blue', freq = T)
boxplot(log(def$creddebt), xlab = "log(cred debt ['000 ???])", main = "", col = 'green', freq = T)
boxplot(log(def$othdebt), xlab = "log(other debt ['000 ???])", main = "", col = 'purple', freq = T)
plot(def$marital, xlab = "marital status", main = "", col = 'grey', ylab ="")
plot(def$homeown, xlab = "homeownership", main = "", col = 'pink', ylab ="")
plot(def$default, xlab = "default", main = "", col = 'orange', ylab ="")


# build log. reg. model + describe steps taken, rationale for rejecting intermediate models etc 

#split dataset for train and test 80:20
testsize <- floor(0.8*nrow(def))
set.seed(15042022) #set seed to split dataset to keep the same random sample 
picked <- sample(seq_len(nrow(def)), size = testsize )
train <- def[picked,] # set to create model with (random 80% of the default set)
test <- def[-picked,] # set to test model with (other 20%)
attach(train)
#find outliers in income & debts
salary_outliers <- boxplot.stats(def$income)$out  #prints all salaries outside the IQR outlier criterion
cd_outliers <- boxplot.stats(def$creddebt)$out  #prints all salaries outside the IQR outlier criterion
od_outliers <- boxplot.stats(def$othdebt)$out  #prints all salaries outside the IQR outlier criterion
# this might be fine tbh, no harm to check though ^

#fit linear model with all IV first 
fit0 <- glm(default ~ age+gender+ed+retire+income+creddebt+othdebt+marital+homeown, data = train, family = binomial) # 77.1
# gender, retire, and marital all not significant
fit1 <- glm(default ~ age+ed+income+creddebt+othdebt+homeown, data = train, family = binomial) # 77.3% only 77% accurate and high deviance 
# try again with transforms to debts and income  
fit2 <- glm(default ~ age+ed+sqrt(income)+sqrt(creddebt)+sqrt(othdebt)+homeown, data = train, family = binomial) #78.12% #stil shite w/ log, sqrt 1% better  
summary(fit1)
# remove homeown, lowest odds ratio in fit 0 and 1 
fit3 <- glm(default ~ age+ed+creddebt+income+othdebt, data = train, family = binomial) # 77.48%  # no improvement, removing income as well as this makes it 1% worse 
#model with p < 2e-16
fit4 <- glm(default ~ age+creddebt+income, data = train, family = binomial) # 76.65% accurate :(



# evaluate model 
actual <- train$default
predicted <- round(fitted(fit3))
xt <- xtabs(~ actual + predicted)
xt
confusion3 <- confusionMatrix(xt)
confusion3
accuracy <- (xt[1,1]+xt[2,2])/sum(xt)
accuracy
# odds ratio
exp(coef(fit3))



#check for multicollinearity 
#correlation matrix ### checking variables for multicoliniarity  
library(ggcorrplot)
#Data needs to be all numeric
corr <- round(cor(train, method = 'pearson'), digits = 2)
head(corr[, 1:6])
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

# summarise parameters of final model 



#verify assumptions 

# odds ratio 

# confusion matrix & measures of model fit 
 




