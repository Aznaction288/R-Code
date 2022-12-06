# 1 Library
library(rjson)
library(tidyverse)
library(knitr)
library(car)
library(stargazer)
library(broom)
library(AER)
library(nlWaldTest)
library(lrmest)
library(stargazer)
library(tseries)
library(lmtest)
library(sandwich)
library(forecast)
library(MCMCpack)
library(stats4)
library(gridExtra)
library(dynlm)
library(orcutt)
library(zoo)
library(pdfetch)
library(extrafont)
.stargazertype <- "text"
if (knitr:::is_latex_output()){
  .stargazertype <- "latex"}
library(splitstackshape)
library(sqldf)

#2 Data Prep

df <- txn_history.2021.10.07
df1 <- cSplit(df, 'V1', sep=':',  type.convert = FALSE)
print(df1)
df2 <- df1$V1_2 != "Sold"
print(df2)
df3 <- sqldf("SELECT * FROM df1 WHERE v1_2 = 'Sold'")
print(df3)
df4 <- cSplit(df3, 'V4', sep=':',  type.convert = FALSE)
df5 <- cSplit(df4, 'V7', sep=':',  type.convert = FALSE)
df5$dates_format <- as.Date(df5$V4_2)
df5$V7_2 <- as.numeric(df5$V7_2)
df5$DateYM <- as.yearmon(df5$dates_format, "%m/%Y")
df5$Month <- format(df5$dates_format, "%m/%Y")
df7 <- aggregate(df5$V7_2, list(df5$dates_format), mean)
colnames(df7) <-  c('Month', "AvgETH")

df9 <-df5 %>% group_by(Month, V1_2)
df10 <- df9 %>% summarise(n= n())
colnames(df10) <-  c('Month', "Sold", 'Volume')

nftavg <-merge(x= df10, y = df7, by = 'Month', all = TRUE)

nft <-merge(x= df5, y = nftavg, by = 'Month', all.x = TRUE)

nftmonth <- as.Date(as.yearmon(nft$Month, "%b/%Y"))

#ETH to USD as of 4/28 1 ETH = $2936.80
# Depending on dicky fuller look at first difference equation. 

#3 Initial Regression
nft.ols <- lm(V7_2~Volume, data=nft)
summary(nft.ols)

nft$res <- nft.ols$residuals

#4 Squared residuals

par(cex=1.0,pch=16,lwd=2, mar=c(4,4,3,4))
nft$ressqr <- nft$res^2
plot(x=nft$dates_format, y=nft$ressqr, xlab='Date', ylab='', col='black', pch=16, main='Squared OLS Residuals')

#5 Correlogram and stationary test


acf(nft.res, xlab='Lag', ylim = c(-1,1), main = 'Correlogram')
nft_rho <- cor(nft$res[-1],dplyr::lag(nft$res)[-1])
print(nft_rho)

adf.test(nft.res)
adf.test(nft$Volume)
adf.test(nft$V7_2)

#6 Lagged Model

nft.ts <- ts(data.frame(Volume=nft$Volume, Price=nft$V7_2, nft), start=c(2017,06), end=c(2021,10), frequency=12)

ts.plot(nft.ts, xlab='Date', ylab='', type='p',
        col='black', pch=16, main='Residuals')


nft2 <- dynlm(Price~Volume, data=nft.ts)
nft2res <- nft2$residuals
nft3 <- dynlm(d(Price)~-1 + L(d(Price)) + Volume + L(nft2res), data=nft.ts)
kable(tidy(nft3))

acf(as.numeric(nft3$residuals), main='Correlation for NFT ECM Residual',
                 ylim = c(-0.2,0.2), xlim=c(1,24))


######
#####7 Average lagged model

nftavg.ts <- ts(data.frame(Volume=nftavg$Volume, Priceavg=nftavg$AvgETH, nftavg), start=c(2017,06), end=c(2021,10), frequency=12)

ts.plot(nftavg.ts, xlab='Date', ylab='', type='p',
        col='black', pch=16, main='Residuals')


nftavg2 <- dynlm(Priceavg~Volume, data=nftavg.ts)
nftavg2res <- nft2$residuals
nftavg3 <- dynlm(d(Priceavg)~-1 + L(d(Priceavg)) + Volume + L(nft2res), data=nftavg.ts)
kable(tidy(nftavg3))

par(cex=1.0,pch=16,lwd=2, mar=c(4,4,3,4))
ts.plot(nftavg3$residuals^2, xlab='Date', ylab='', type='p',
        col='black', pch=16, main='Squared ECM Residuals')

acf(as.numeric(nftavg3$residuals), main='Correlation for NFT ECM Residual',
    ylim = c(-0.5,0.5), xlim=c(1,24))[1]

