############################################################################################################################################
##################################################     Final term - Cash holding      ###################################################### 
############################################################################################################################################


###################################################     1. Create Datasets      ############################################################ 

# Ignore the warning messages
options(warn=-1)

# Import library
library(tidyverse)
library(readxl)
library(readr)
library(ggplot2)   # for plotting
library(forcats)   # for handling factors
library(scales)    # for axis scale formatting
library(xts)       # xts conclude index and core data (DataFrame have a timeseries columns)
library(tseries)   # adf.test
library(lmtest)    # coeftest
library(stats)     # Box.test
library(forecast)
library(PerformanceAnalytics)

# Import data from excel file.
df = read_excel("K194141745.xlsx")
View(df)

# Checking Na values.
sum(is.na(df))

###################################################    2. Calculated the control variable    #################################################

covid_period = c(df$MSN[which(df$MSN == 'Q1/2020'):which(df$MSN == 'Q1/2022')])

# Calculate control varibles
data = df %>% transmute(Quarter = MSN,
                        CashHolding = round(Cash/TotalAssets,6),                                   
                        Leverage = round(TotalDebt/TotalAssets,6),                                 
                        CAE = round(FixAssets/TotalAssets,6),                                      
                        Firmsizes = log(TotalAssets),
                        Covid19 = ifelse(MSN %in% covid_period,1,0)
)

# Checking Na values.
sum(is.na(data))

# Descriptive statistics data
summary(data)
View(data)

# Export data to csv
# write.csv(data, file = "C:/Users/ASUS/Desktop/Study/Year_3/HK_6/R_Program/Final/Code/MSN.csv",row.names=FALSE, na="NA")

#######################################################    3. Descriptive statistics    ################################################################

#### Period Q4/2009 - Q4/2019
before= data %>% slice(1:which(Quarter == 'Q4/2019'))

# Descriptive statistics before Q1/2020
before_statis = before %>% summarise(variables = c('Cash Holding','Leverage', 'Capital expenditure','Firmsizes'),
                                     obs = nrow(before),
                                     min = c(min(CashHolding),min(Leverage),min(CAE),min(Firmsizes)),
                                     mean = c(mean(CashHolding),mean(Leverage),mean(CAE),mean(Firmsizes)),
                                     median = c(median(CashHolding),median(Leverage),median(CAE),median(Firmsizes)),
                                     std = c(sd(CashHolding),sd(Leverage),sd(CAE),sd(Firmsizes)),
                                     max = c(max(CashHolding),max(Leverage), max(CAE), max(Firmsizes))
)
before_statis = data.frame(before_statis)
before_statis

#### Period Q1/2020 - now
after = data %>% slice(which(data$Quarter == 'Q1/2020'): nrow(data))

# Descriptive statistics after Q1/2020
after_statis = after %>% summarise(variables = c('Cash Holding','Leverage', 'Capital expenditure','Firmsizes'),
                                   obs = nrow(after),
                                   min = c(min(CashHolding),min(Leverage),min(CAE),min(Firmsizes)),
                                   mean = c(mean(CashHolding),mean(Leverage),mean(CAE),mean(Firmsizes)),
                                   median = c(median(CashHolding),median(Leverage),median(CAE),median(Firmsizes)),
                                   std = c(sd(CashHolding),sd(Leverage),sd(CAE),sd(Firmsizes)),
                                   max = c(max(CashHolding),max(Leverage), max(CAE), max(Firmsizes))
)
after_statis = data.frame(after_statis)
after_statis

###########################################################     4. Visualization     #####################################################################  

# Histogram of cash holding
data %>% ggplot(aes(CashHolding)) +
  geom_histogram(binwidth = 0.03,fill = 'coral', color = 'white') + 
  labs(title = "Histogram of Cash Holding",
       x = "Cash Holding",
       y = "Count")+
  scale_x_continuous(labels = comma)+
  scale_y_continuous()


# Box plot of cash holding  
boxplot(data$CashHolding,
        main = "Box plot of Masan Cash holding",
        col = "orange",
        border = "brown",
        xlab= "Cash holding",
        horizontal = TRUE
)
###########################    5. Perform multiple regression to determine the significant determinants of the variable of assigned topic.   ################## 

# Scatter for checking the linear characteristic in the data sets.
par(mfrow=c(1,3))
plot(CashHolding ~ Leverage, data=data, col = "blue", lwd = 2)
plot(CashHolding ~ CAE, data=data, col = "chartreuse4",lwd = 2)
plot(CashHolding ~ Firmsizes, data=data, col = "red",lwd = 2)


##### 5.1. Multiple Linear Regression with the usual individual variables (model 1) 
cash.holding.lm<-lm(CashHolding ~  Leverage + Firmsizes + CAE, data = data)
summary(cash.holding.lm)

# Visualization 
par(mfrow=c(2,2))
plot(cash.holding.lm)

# Check normality
shapiro.test(resid(cash.holding.lm)) # Null hypothesis is normality

# Check homoskedasticity
bptest(cash.holding.lm) #Null hypothesis is homoskedasticity


##### 5.2. Multiple Linear Regression with the usual individual variables and the interaction 
# between Covid-19 dummy variable (model 2)
data$Covid19 = factor(data$Covid19)
cash.holding.lm.covid<-lm(CashHolding ~  Leverage + Firmsizes + CAE + Covid19 , data = data)
summary(cash.holding.lm.covid)

# Visualization 
par(mfrow=c(2,2))
plot(cash.holding.lm.covid)

# Check normality
shapiro.test(resid(cash.holding.lm.covid))# Null hypothesis is normality

# Check homoskedasticity
bptest(cash.holding.lm.covid) #Null hypothesis is homoskedasticity

##### 5.3. Prediction
pred.model = predict(cash.holding.lm, data[,3:5])

pred.table =data.frame(Quanter = data$Quarter,
                       CashHolding.Real = data$CashHolding,
                      CashHolding.Pred = round(pred.model,6))
pred.table

accuracy(pred.table$CashHolding.Real,pred.table$CashHolding.Pred)

##############################################       6. ARIMA        ##############################################

# Check class
class(data$Quarter)

# Convert "character" to "Date" data
date.time = seq(as.Date("2009/12/01"), by = "quarter", length.out = nrow(data)) 
date.time

# Create a xts Dataframe
msn.cash = xts(data[,2],date.time)

View(msn.cash)

par(mfrow=c(1,1))
plot(msn.cash$CashHolding)

##### Cash Holding stationary check 

# ACF/PACF
par(mfrow=c(1,2))
acf(msn.cash$CashHolding,main='ACF for CashHolding',lag.max = 24)
pacf(msn.cash$CashHolding,main='PACF for CashHolding',lag.max = 24)

# ADF test and KPSS test
adf.test(msn.cash$CashHolding) 
kpss.test(msn.cash$CashHolding)


# Caculate the percent change of cash holding.
msn.cash$diff = diff(msn.cash$CashHolding)
lag.cash = msn.cash$CashHolding[-nrow(data)]

index(lag.cash) = seq(as.Date("2010/03/01"), by = "quarter", length.out = nrow(data)-1) 
msn.cash$lag = lag.cash

msn.cash = na.omit(msn.cash)
msn.cash$pct.change = msn.cash$diff/lag.cash

##### The percent change of cash holding stationary check 

# ACF/PACF
par(mfrow=c(1,2))
acf(msn.cash$pct.change,main='ACF for percent change of CashHolding',lag.max = 24)
pacf(msn.cash$pct.change,main='PACF for percent change of CashHolding',lag.max = 24)

# ADF test and KPSS test
adf.test(msn.cash$pct.change) 
kpss.test(msn.cash$pct.change)

# Building Auto-ARIMA model
auto=auto.arima(msn.cash$pct.change,seasonal=F,trace = T, ic='aic')
auto
coeftest(auto)

# Prediction
term = 4
fcastauto=forecast(auto,h=term)
fcastauto # predicted values for 4 terms (Q1/2022 - Q4/2022)

# Visualization the prediction
par(mfrow=c(1,1))
plot(fcastauto)

#train set
accuracy(fcastauto)

############################################################################################################################################
######################################################           The End         ########################################################### 
############################################################################################################################################
