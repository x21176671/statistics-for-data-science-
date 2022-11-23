# stats CA assignment
# multiple linear regression analysis NO OUTLIERS 
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
names(def)[names(def) == "ï..gender"] <- "gender"

# PART B LOGISTIC REGRESSION 
def_num <- data.frame(def) # numeric dataframe to compute correlation plot later

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
#remove income, and debt variable outliers  428 TOTAL OUTLIERS
income_outliers <- boxplot.stats(def$income)$out  #prints all creddebts outside the IQR outlier criterion
creddebt_outliers <- boxplot.stats(def$creddebt)$out  #prints all creddebts outside the IQR outlier criterion
othdebt_outliers <- boxplot.stats(def$othdebt)$out  #prints all othdebts outside the IQR outlier criterion
def_no_indy_out <- def[def$income<min(income_outliers),] # 209 outliers
def_no_indy_out <- def_no_indy_out[def_no_indy_out$creddebt<min(creddebt_outliers),] # 278 additional outliers
def_no_indy_out <- def_no_indy_out[def_no_indy_out$othdebt<min(othdebt_outliers),] # 221  outliers

#split dataset for train and test 80:20
testsize <- floor(0.8*nrow(def_no_indy_out))
set.seed(15042022) #set seed to split dataset to keep the same random sample 
picked <- sample(seq_len(nrow(def_no_indy_out)), size = testsize )
train <- def_no_indy_out[picked,] # set to create model with (random 80% of the default set)
test <- def_no_indy_out[-picked,] # set to test model with (other 20%)
attach(train)

#fit linear model with all IV first 
fit0 <- glm(default ~ age+gender+ed+retire+income+creddebt+othdebt+marital+homeown, data = train, family = binomial) # 78.3
# gender, retire, and marital all not significant
fit1 <- glm(default ~ age+ed+income+creddebt+othdebt+homeown, data = train, family = binomial) # 78.08% only 77% accurate and high deviance 
# try again with transforms to debts and income  
fit2 <- glm(default ~ age+ed+sqrt(income)+sqrt(creddebt)+sqrt(othdebt)+homeown, data = train, family = binomial) #77.81% #stil shite w/ log, sqrt 1% better  
summary(fit1)
# remove homeown, lowest odds ratio in fit 0 and 1 
fit3 <- glm(default ~ age+ed+creddebt+income+othdebt, data = train, family = binomial) # 77.81%  # no improvement, removing income as well as this makes it 1% worse 

#try again with looser outlier removal
income_mean <- mean(def$income)
income_IQR <- IQR(def$income)
income_outliers <- def[def$income > income_mean + 1.5*income_IQR,]$income  #prints all creddebts outside the IQR outlier criterion
creddebt_mean <- mean(def$creddebt)
creddebt_IQR <- IQR(def$creddebt)
creddebt_outliers <- def[def$creddebt > creddebt_mean + 1.5*creddebt_IQR,]$creddebt  #prints all creddebts outside the IQR outlier criterion
othdebt_mean <- mean(def$othdebt)
othdebt_IQR <- IQR(def$othdebt)
othdebt_outliers <- def[def$othdebt > othdebt_mean + 1.5*othdebt_IQR,]$othdebt  #prints all creddebts outside the IQR outlier criterion
def_no_indy_out <- def[def$income<min(income_outliers),] # 209 outliers
def_no_indy_out <- def_no_indy_out[def_no_indy_out$creddebt<min(creddebt_outliers),] # 278 additional outliers
def_no_indy_out <- def_no_indy_out[def_no_indy_out$othdebt<min(othdebt_outliers),] # 221  outliers

#split dataset for train and test 80:20
testsize <- floor(0.8*nrow(def_no_indy_out))
set.seed(15042022) #set seed to split dataset to keep the same random sample 
picked <- sample(seq_len(nrow(def_no_indy_out)), size = testsize )
train <- def_no_indy_out[picked,] # set to create model with (random 80% of the default set)
test <- def_no_indy_out[-picked,] # set to test model with (other 20%)
attach(train)

#fit linear model with all IV first 
fit0 <- glm(default ~ age+gender+ed+retire+income+creddebt+othdebt+marital+homeown, data = train, family = binomial) # 78.3
# gender, retire, and marital all not significant
fit1 <- glm(default ~ age+ed+income+creddebt+othdebt+homeown, data = train, family = binomial) # 78.08% only 77% accurate and high deviance 
fit2 <- glm(default ~ age+ed+creddebt+othdebt+income, data = train, family = binomial) # 78.08% only 77% accurate and high deviance 


# evaluate model 
actual <- train$default
predicted <- round(fitted(fit2))
xt <- xtabs(~ actual + predicted)
xt
confusion1 <- confusionMatrix(xt)
confusion1
accuracy <- (xt[1,1]+xt[2,2])/sum(xt)
accuracy
# odds ratio
exp(coef(fit3))



#check for multicollinearity 
# correlation plot
library(ggcorrplot)
#Data needs to be all numeric
corr <- round(cor(def_num, method = 'pearson'), digits = 2)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)
#redo w/o default variable 
def_num_sub1 <- subset(def_num, select = c("age", "ed", "income", "creddebt", "othdebt", "homeown", "gender", "retire", "marital"))
corr <- round(cor(def_num_sub1, method = 'pearson'), digits = 2)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)
# VIF 
vif0 <- VIF(fit0)
vif1 <- VIF(fit1)
vif1

# summarise parameters of final model 



#verify assumptions 

# odds ratio 

# confusion matrix & measures of model fit 





