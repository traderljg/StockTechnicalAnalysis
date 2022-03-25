library(quantmod)
library(plotly)
library(tidyverse)
library(tidyquant)
library(dplyr)
library(lubridate)
library(forcats)
library(PerformanceAnalytics)


start <- as.Date("2018-01-12")      #Starting Date For Stock Data 
end <- as.Date("2022-01-01")        #Ending Date For Stock Data

#Download Symbols
getSymbols("spy", src = "yahoo",from = start,to = end)

#Get dates
dates = index(SPY)
#Convert from Xts to Dataframe
SPY = as.data.frame(SPY)
#Add column with dates to Dataframe
SPY = cbind(date = dates, SPY)

#Calculate return Col
SPY = SPY %>%
  mutate(return = SPY.Close - SPY.Open)

############################################################

#Coin Flip Strat

#This strategies works by flipping coin a set number of times.
#If it is heads we buy long at the open, if it is tails we go golf
#We sell at the close of every day. Then repeat the process the next day

#Coin_Fun = function(){

#Flip a 2 sided coin 1008 times
C_Flips = rbinom(1000, 1, .5)

#Add colum for flips
SPY = SPY %>%
  add_column(C_Flips = C_Flips)

#Filter for all flips that were heads
C_Flip_Strat = SPY %>%
  filter(C_Flips == 1)

#Extra variable of just heads column for returns
Heads_Returns = C_Flip_Strat %>%
  select(return)


x = C_Flip_Strat$date
y = C_Flip_Strat$return
capital = 1000



#Convert to Xts in case Quant mod package is needed
Flip_Strat = as.xts(C_Flip_Strat)



#Determine commission by multiplying the open/the price you bought at by 5%
Commish = C_Flip_Strat$SPY.Open * .05
#Subtract the commission from the return
Ret_Comm = y - Commish

#Take capital divide by the buy price, round to even number of shares
shares = round(capital / C_Flip_Strat$SPY.Open, digits = 0)
#Multiply the number of shares by the profit or loss in returns then add them up
equity = cumsum(shares * Heads_Returns)+capital



#Determine the x and y coordinates for the labels
annotations <- data.frame(
  x = c(
    round(min(C_Flip_Strat$return), 2),
    round((mean(y) - sd(y)), 2) - 2.5,
    round(mean(C_Flip_Strat$return), 2),
    round((mean(y) + sd(y)), 2) + 2,
    round(max(C_Flip_Strat$return), 2)
  ),
  y = c(4, 50, 58, 50, 5),
  label = c("Min:", "Std1:", "Mean:", "Std1:", "Max:")
)

#Color green if returns > 0 else color red
Clr_Returns = ifelse(y >= 0, "green1", "#00FF00")

#Plot histogram of Non commission based returns for Coin Flip Strat, No position
ggplot(C_Flip_Strat, aes(x = y)) +
  geom_histogram(aes(fill = Clr_Returns), bins = 75) +
  geom_vline(aes(xintercept = mean(y)), color = "#000000", size = 1.25) +
  geom_vline( aes(xintercept = mean(y) + sd(y)),
    color = "#000000",size = 1,linetype = "dashed") +
  geom_vline( aes(xintercept = mean(y) - sd(y)),
    color = "#000000", size = 1,linetype = "dashed") +
  ylim(c(0, 60)) +
  geom_text(data = annotations,aes(x = x,y = y,label = paste(label, x)),
    size = 5, fontface = "bold") + 
    labs(  title = "Histogram of Random Coin Flips",
    caption = "Source: Gapminder dataset", x = "Returns", y = "Time")


#Plot of commission based returns for Coin Flip Strat, No position
ggplot(C_Flip_Strat, aes(x = x, y = Ret_Comm, colour = Ret_Comm > 0)) +
  scale_colour_manual(name = 'Return > 0', values = setNames(c('green', 'red'),
  c(T, F))) + xlab('Time') + ylab('Returns w Commission') +
  geom_point() + geom_hline(yintercept = 0, size = 1) +
  geom_smooth(method = "lm", colour = "black", size = 2) +ylim(c(-50, 10))

#Single plot of one 1 coin flip with 1000 starting capital
plot(equity$return,type = "l",  col = "black",
  main = "Coin Flip Strat Equity Graph (Capital = $1000)",
  xlab = "Days", ylab = "Value")
abline(h = capital, col = "black", lwd = 3)
abline(h = max(equity$return), col = "green", lwd = 1)
abline(h = min(equity$return), col = "red", lwd = 1)
legend("right", legend=c("Equity High", "Equity Low"),
       col=c("green", "red"), lty=1:2, cex=0.8)




RandTests = cbind(equity)
start <- as.Date("2020-01-01")                     
end <- as.Date("2020-06-01")   
getSymbols("spy", src = "yahoo",from = start,to = end)


for (i in 1:75){
  capital = 1000
  dates = index(SPY)
  SPY2 = as.data.frame(SPY)
  SPY2 = cbind(date = dates, SPY2)
  SPY2 = SPY2 %>%
    mutate(return = SPY.Close - SPY.Open)
  
  C_Flips = data.frame(rbinom(103, 1, .5))
  
  
  
  SPY2 = SPY2 %>%
    add_column(Flips = C_Flips)
  
  C_Flip_Strat = SPY2 %>%
    filter(Flips == 1)
  
  
  shares = round(capital / C_Flip_Strat$SPY.Open, digits = 0)
  equity_test = cumsum(shares * C_Flip_Strat$return)+capital
 
  RandTests = qpcR:::cbind.na(RandTests,equity_test)
 
}
RandTests <- subset (RandTests, select = -return)
equityHigh = max(RandTests, na.rm = TRUE)
equityLow = min(RandTests, na.rm = TRUE)
#Mean of all tests
test_mean = mean(colMeans(RandTests, na.rm = TRUE))


RandTests = qpcR:::cbind.na(date = dates, RandTests)
RandTests =  RandTests %>% drop_na()






a = .15

ggplot(RandTests, aes(x = date))+
  geom_line(aes(y = equity_test.1), alpha = a)+
  geom_line(aes(y = equity_test.2), alpha = a)+
  geom_line(aes(y = equity_test.3), alpha = a)+
  geom_line(aes(y = equity_test.4), alpha = a)+
  geom_line(aes(y = equity_test.5), alpha = a)+
  geom_line(aes(y = equity_test.6), alpha = a)+
  geom_line(aes(y = equity_test.7), alpha = a)+
  geom_line(aes(y = equity_test.8), alpha = a)+
  geom_line(aes(y = equity_test.9), alpha = a)+
  geom_line(aes(y = equity_test.10), alpha = a)+
  geom_line(aes(y = equity_test.11), alpha = a)+
  geom_line(aes(y = equity_test.12), alpha = a)+
  geom_line(aes(y = equity_test.13), alpha = a)+
  geom_line(aes(y = equity_test.14), alpha = a)+
  geom_line(aes(y = equity_test.15), alpha = a)+
  geom_line(aes(y = equity_test.16), alpha = a)+
  geom_line(aes(y = equity_test.17), alpha = a)+
  geom_line(aes(y = equity_test.18), alpha = a)+
  geom_line(aes(y = equity_test.19), alpha = a)+
  geom_line(aes(y = equity_test.20), alpha = a)+
  geom_line(aes(y = equity_test.21), alpha = a)+
  geom_line(aes(y = equity_test.22), alpha = a)+
  geom_line(aes(y = equity_test.23), alpha = a)+
  geom_line(aes(y = equity_test.24), alpha = a)+
  geom_line(aes(y = equity_test.25), alpha = a)+
  geom_line(aes(y = equity_test.26), alpha = a)+
  geom_line(aes(y = equity_test.27), alpha = a)+
  geom_line(aes(y = equity_test.28), alpha = a)+
  geom_line(aes(y = equity_test.29), alpha = a)+
  geom_line(aes(y = equity_test.30), alpha = a)+
  geom_line(aes(y = equity_test.31), alpha = a)+
  geom_line(aes(y = equity_test.32), alpha = a)+
  geom_line(aes(y = equity_test.33), alpha = a)+
  geom_line(aes(y = equity_test.34), alpha = a)+
  geom_line(aes(y = equity_test.35), alpha = a)+
  geom_line(aes(y = equity_test.36), alpha = a)+
  geom_line(aes(y = equity_test.37), alpha = a)+
  geom_line(aes(y = equity_test.38), alpha = a)+
  geom_line(aes(y = equity_test.39), alpha = a)+
  geom_line(aes(y = equity_test.40), alpha = a)+
  geom_line(aes(y = equity_test.40), alpha = a)+
  geom_line(aes(y = equity_test.41), alpha = a)+
  geom_line(aes(y = equity_test.42), alpha = a)+
  geom_line(aes(y = equity_test.43), alpha = a)+
  geom_line(aes(y = equity_test.44), alpha = a)+
  geom_line(aes(y = equity_test.45), alpha = a)+
  geom_line(aes(y = equity_test.46), alpha = a)+
  geom_line(aes(y = equity_test.47), alpha = a)+
  geom_line(aes(y = equity_test.48), alpha = a)+
  geom_line(aes(y = equity_test.49), alpha = a)+
  geom_line(aes(y = equity_test.50), alpha = a)+
  geom_hline(yintercept = capital, size =1.5, color ="black", linetype = "dashed")+
  geom_hline(yintercept = equityHigh, size =1, color ="green", linetype = "solid")+
  geom_hline(yintercept = test_mean, size =.8, color ="black", linetype = "dashed")+
  geom_hline(yintercept = equityLow, size =1, color ="red", linetype = "solid")
  
  
  
  
  
  

  



#Talking points For slides
# I found working with dates and random amount of rows from the coin flip challenging
# as it created jagged arrays filled with N/A values
# This made running mean, min maxfunctions difficult as well, 
# as dates are treated differently.#Also I found it challenging deal 
#with variables already stored in the enviroment.
#Often times testing solutions in the console would show errors
#because the dataframe was still in system




#Monte Carlo simualation, zero sum game then add commssions etc

#rm(list = ls())

######################################################################
#Buy and Hold Strat
stock_list = ("SPY")
getSymbols(stock_list,warnings = FALSE)
Stock = to.yearly(SPY)

#Convert from xts to dataframe
df1 = data.frame(Date = index(Stock), coredata(Stock))

df1 = df1 %>%
  add_column(InvPeriod = seq(NROW(df1)))




df2 = df1%>%
  filter(InvPeriod == 1 | InvPeriod == 4 | InvPeriod == 7 | InvPeriod == 10 
         | InvPeriod == 13 )%>%
  select(Date, SPY.Open, SPY.Adjusted, InvPeriod)

#3 year returns
capital = 1000
shares = round(capital / df2$SPY.Open, digits = 0)

shares = data.frame(shares)

Returns2 = 0

#Calulates return for a buy and hold every 3 years, buy at open of first
#year sell at adjusted close 3 years later
for (i in 2:(NROW(df2)+2)) {
  Returns2 =  rbind.data.frame(Returns2, (df2[i,3] - df2[i-1,2]))
}

c1 = colnames(Returns2)

Returns2 = rename(Returns2, BH_Returns = c1 )

BHReturn = Returns2 %>% 
  filter(!is.na(BH_Returns))

BHReturn = BHReturn[2:5,]

BHReturn = as.data.frame(BHReturn)
BH_equity = 0

for (i in 1:NROW(shares)) {
  BH_equity = rbind(BH_equity,shares[i,1] * BHReturn[i,1] +capital)
}

BH_equity[1,1] = capital

BH_equity = BH_equity[1:5,1]

BH_equity = cbind.data.frame(BH_equity, 
                             year = seq(from = 2007, to = 2019, by=3))




BH_Hold_Plot = ggplot(BH_equity, aes(x = year))+
  geom_point(aes(y = BH_equity), size = 3)+
  geom_line(aes(y = BH_equity), size = 1.5)+
  geom_hline(yintercept = capital, size =1.5, color ="black", linetype = "dashed")+
  geom_hline(yintercept = max(BH_equity$BH_equity), size =1, color ="green", linetype = "solid")+
  geom_hline(yintercept = min(BH_equity$BH_equity), size =1, color ="red", linetype = "solid")

ggplotly(BH_Hold_Plot)




rm(list = ls())

######################################################################

#$$
 # SMA_n=\frac{Sum_n\left( Close \right)}{n}
#$$

#Technical Indicators


#Moving Averages
#These are line overlays that smooth price data to identify trend
getSymbols("SPY",src="yahoo",from="2020-01-01",to="2021-01-01")


#Chart with Indicator
#Notice how the 5 day average (white line) much more closely represents the data
# Whereas the the longer 15 day SMA smooths out price
barChart(SPY,theme=chartTheme("black"))
addSMA(n=5,col="white")
addSMA(n=15,col="red")
legend("bottomright",col=c("grey","red"),lty=1,legend=c("SMA(5)","SMA(15)"),cex=0.6)

#Bolinger Bands
#These bands are in essence moving averages used to identify abnormal
#volatitlity 

#$$Upper\,\,Band=SMA_{20}\left( Close \right) +2*STDEV_{20}\left( Close \right)$$
#$$Middle\,\,Band\,\,=SMA_{20}\left( Close \right)$$
#$$Lower\,\,Band=SMA_{20}\left( Close \right) -2*STDEV_{20}\left( Close \right)$$

#Bollinger bands for 20 days with 2 standard deviations
bb <- BBands(HLC(SPY),n=20,sd=2)
# Technical Analysis Chart
barChart(SPY,theme=chartTheme("white"))
addBBands(n=20,sd=2)

#Moving average crossover strat
#This is by far and large one of the first things you learn in techinical analysis
#In order to test the same time period as the buy and hold strat
# I will be using a 50 and 100 average.
#The buy signal will occur when previous days close was < SMA then 
#the next days current price close becomes greater than the SMA
#$$Buy:PreviousDaysClose<SMA\rightarrow CurrentDaysClose>SMA$$


barChart(SPY,theme=chartTheme("white"))
addSMA(n=5,col="darkblue")
addSMA(n=21,col="darkred")
legend("bottomright",col=c("darkblue","darkred"),lty=1,legend=c("SMA(5)","SMA(21)"),cex=0.6)
# Lag executes command the day after signal
# If yesterdays close price of SPY is less than yesterdays mvng avg &
# if todays closing price is > the mvng avg = 1 which means Buy
# If the opposite sma_Price_Buy = -1 which means sell
# Else 0 which means do nothing

# Simple Moving Average
sma50 = SMA(Cl(SPY),n=20)
sma100 = SMA(Cl(SPY),n=100)
#Signal
sma_Price_Strat = Lag(ifelse(Lag(Cl(SPY))<Lag(sma50)&Cl(SPY)>sma50,1,ifelse(Lag(Cl(SPY))>Lag(sma50)&Cl(SPY)<sma50,-1,0)))
#Do nothing if NA, replace with 0
sma_Price_Strat[is.na(sma_Price_Strat)] = 0

#Strat
#Creates a variable of all 1's
sma_50_posistion = ifelse(sma_Price_Strat>1,0,1)
for(i in 1:length(Cl(SPY))){sma_50_posistion[i] = ifelse(sma_Price_Strat[i]==1,1,
                                                 ifelse(sma_Price_Strat[i]==-1,0,sma_Price_Strat[i-1]))}
sma_50_posistion[is.na(sma_50_posistion)] = 1
sma50poscomp = cbind(Cl(SPY),sma50,sma_Price_Strat,sma_50_posistion)
colnames(sma50poscomp) = c("Close",'sma50',"sma_Price_Strat","sma_50_posistion")
View(sma50poscomp)

Ret = dailyReturn(Cl(SPY),type="arithmetic")
Ret[1] = 0
bh_strat = Ret
sma50strat <- Ret*sma_50_posistion

chart.CumReturns(sma50strat)
table.AnnualizedReturns(sma50strat)

chart.CumReturns(bh_strat)
table.AnnualizedReturns(bh_strat)


