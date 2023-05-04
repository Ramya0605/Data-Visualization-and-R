getwd()      #check working directory

setwd("specify path here")    #set working directory 

#############################################################################33
# Data preparation process

#importing the data
S_data <- read.csv("D:/Data science/ASDV/Assignment/Task 2/School 
                   salary_data.csv", header= TRUE)

#viewing dimension of data
dim(S_data)

#displaying names of the columns
names(S_data)

#structure of data
str(S_data)

#displaying top 5 data
head(S_data)

#displaying last 5 data
tail(S_data)

#calculating number of rows
nrow(S_data)

###########################################################################3
#checking missing values
is.na(S_data)

#find missing value in separate column wise
colSums(is.na(S_data))

#to give overall column result
sum(is.na(S_data))

#if there is any missing data,then gives False
complete.cases(S_data)

#removes missing data and display remaining values
data = na.omit(S_data)

#outliers for primary

IQR(S_data$Experienced)

IQR(S_data$Well.Experienced)

IQR(S_data$Beginner)

#Outliers for Secondary

IQR(S_data$US.Experienced)

IQR(S_data$US.Well.Experienced)

IQR(S_data$US.Beginner)

#######################################################################
# Task 4.1 - Descriptive Analysis
#find summary-------------------------------------------------------------------
summary(data)


#using Hist function to check the data in histogram
hist(data$Time)

#Primary teacher salary in 10 years
hist(data$Experienced,col=data("Green"))

#Primary teacher salary in 15 years
hist(data$Well.Experienced, col=data("Red"))

#Primary teacher's starting salary 
hist(data$Beginner, col=data("Blue"))

#Primary teacher's top salary
hist(data$Top.scale, col=data("cyan"))

#########################################################################
install.packages('e1071', dependencies=TRUE)
library(e1071)
#skewness
skewness(data$Experienced, na.rm = FALSE)
skewness(data$Well.Experienced, na.rm = FALSE)
skewness(data$Beginner, na.rm = FALSE)
skewness(data$Top.scale, na.rm = FALSE)
skewness(data$US.Experienced, na.rm = FALSE)
skewness(data$US.Well.Experienced, na.rm = FALSE)
skewness(data$US.Beginner, na.rm = FALSE)
skewness(data$US.Top.of.scale, na.rm = FALSE)

#############################################################################3
#standard deviation
sd(data$Time , na.rm = FALSE)
sd(data$Experienced, na.rm = FALSE)
sd(data$Well.Experienced, na.rm = FALSE)
sd(data$Beginner, na.rm = FALSE)
sd(data$Top.scale, na.rm = FALSE)
sd(data$US.Experienced, na.rm = FALSE)
sd(data$US.Well.Experienced, na.rm = FALSE)
sd(data$US.Beginner, na.rm = FALSE)
sd(data$US.Top.of.scale, na.rm = FALSE)

####################################################################################3
install.packages("moments")
library(moments)
#kurtosis
kurtosis(data$Time)
kurtosis(data$Experienced)
kurtosis(data$Well.Experienced)
kurtosis(data$Beginner)
kurtosis(data$Top.scale)
kurtosis(data$US.Experienced)
kurtosis(data$US.Well.Experienced)
kurtosis(data$US.Beginner)
kurtosis(data$US.Top.of.scale)

##############################################################################3
# Task 4.2 - Correlation Analysis
  
install.packages("tidyverse") 
library(tidyverse)
install.packages("corrplot")
library(corrplot)

head(S_data)

used_variables<-S_data[,(c(4,5,6,7,8,9,10,11))]
used_variables

cor(used_variables)

m=cor(used_variables)

corrplot(m, method="ellipse")
corrplot(m, method="pie")


plot(used_variables)
boxplot(used_variables)

install.packages("ggstatsplot")
library(ggstatsplot)
no_outliers <- as.data.frame(sapply(used_variables, 
                                    function(used_variables) 
                                      (abs(used_variables-mean(used_variables))
                                       /sd(used_variables))))

no_outliers1 <- no_outliers[!rowSums(no_outliers>3), ]

corrplot(cor(no_outliers1), order="AOE" ,method ="pie", type = "lower")


install.packages("GGally")
library(GGally)

GGally::ggpairs(used_variables, columns=1:4)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(used_variables[1:4],histogram=TRUE, method="Pearson")

####################################################################################

#Task 4.3 - Regression analysis

#loading the data
newdata <- select(used_variables,1:4)

glimpse(newdata)

install.packages("GGally")
library(GGally)
ggpairs(newdata)

#Define the objective

model<-lm(Top.scale ~., data=newdata)

summary(model)

corrplot(cor(newdata))

#Performing linear regression analysis

model1<-lm(Top.scale~Experienced, data=newdata)
summary(model1)

model2<-lm(Top.scale~Well.Experienced, data=newdata)
summary(model2)

model3<-lm(Top.scale ~ Well.Experienced + Experienced, data=newdata)
summary(model3)

install.packages("broom")
library(broom)
glance(model)
glance(model1)
glance(model2)
glance(model3)

plot(model3,1)
plot(model3,2)
plot(model3,3)


###################################################################################
#Task 4.4 - Time series Analysis

#Convert to time series format
tseries<-ts(S_data$Well.Experienced,start=2010,end = 2019, frequency=12)
tseries
plot(tseries)


install.packages("TTR")
library(TTR)
install.packages("forecast")
library(forecast)

f = forecast(tseries)
plot(f)

# Prediction of 28 months
m_ets <- ets(tseries)
f_ets <- forecast(m_ets, h = 28)
plot(f_ets)


hw = HoltWinters(tseries)
f_ets = predict(hw, n.ahead = 28, prediction.interval = T, level = 0.95)
plot(hw, f_ets)


aa = auto.arima(tseries)
f_ets = forecast(aa, h = 28)
plot(f_ets)



#####################################################################################
# Task 4.5 - Hypothesis testing

install.packages("datarium")
install.packages("qqplotr")
install.packages("ggplot2")
library(ggplot2)
library(datarium)
library(qqplotr)

# Q-Q plot
ggplot(mapping = aes(sample=newdata$Top.scale)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Time") + ylab("Experienced")


random_sample2 <- rexp(500)
ggplot(mapping = aes(sample = random_sample2)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Time") + ylab("Experienced")

# One sample T-Test

qplot(data$Experienced, geom="histogram")
t.test(data$Experienced, mu=45000)
t.test(data$Experienced, mu=45000, alternative="less")
t.test(data$Experienced, mu=45000, alternative="greater")

# Two sample T-Test

t.test(data$Experienced, data$US.Experienced, alternative="less")
t.test(data$Experienced, data$US.Experienced, alternative="greater")

#CHI-SQUARE

chisq <- chisq.test(data$Country.Name, data$Country.Code)
chisq

#Apply chisquare test to find difference for 
chisq.test(data$Country.Name, data$Country.Code)


#create table for both the columns
table(data$Country.Name, data$Country.Code)

chisq.test(as.table(rbind(data$Experienced, data$US.Experienced)))

#########################################################################


