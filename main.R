library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)
library(gam)
library(prophet)
library(gbm)

# data downloading
# the path is absolute and should be replaced if necessary
data <- read.csv("C:/Users/Иван/Desktop/UNIPD/Business Economic and Financial Data/project/unemployment_rate.csv")

str(data)

data$observation_date <- as.Date(data$observation_date, format = "%Y-%m-%d")

tt <- data$observation_date
unr <- data$UNRATE

plot(tt, unr, type="l", main="Unemployment Rate in the US", xlab="Time", ylab="Rate (%)")

par(mfrow=c(1,2))
Acf(unr, main="Autocorrelation")
Pacf(unr, main="Partial autocorrelation")
tsdisplay(unr)
par(mfrow=c(1,1))

# additional data
avg_hourly_earnings <- read.csv("C:/Users/Иван/Desktop/UNIPD/Business Economic and Financial Data/project/avg_hourly_earnings.csv")
avg_weekly_hours <- read.csv("C:/Users/Иван/Desktop/UNIPD/Business Economic and Financial Data/project/avg_weekly_hours.csv")
cpi <- read.csv("C:/Users/Иван/Desktop/UNIPD/Business Economic and Financial Data/project/consumer_price_index.csv")
fed_rate <- read.csv("C:/Users/Иван/Desktop/UNIPD/Business Economic and Financial Data/project/fed_rate.csv")
labor_force <- read.csv("C:/Users/Иван/Desktop/UNIPD/Business Economic and Financial Data/project/labor_force.csv")
gdp <- read.csv("C:/Users/Иван/Desktop/UNIPD/Business Economic and Financial Data/project/gdp_new.csv")
rgdp <- read.csv("C:/Users/Иван/Desktop/UNIPD/Business Economic and Financial Data/project/real_gdp_new.csv")
gdp_growth <- read.csv("C:/Users/Иван/Desktop/UNIPD/Business Economic and Financial Data/project/gdp_bbk.csv")
covid <- read.csv("C:/Users/Иван/Desktop/UNIPD/Business Economic and Financial Data/project/covid_new.csv")
spx <- read.csv("C:/Users/Иван/Desktop/UNIPD/Business Economic and Financial Data/project/spx_new.csv")

str(avg_hourly_earnings)
str(avg_weekly_hours)
str(cpi)
str(fed_rate)
str(labor_force)

str(gdp)
str(rgdp)
str(gdp_growth)
str(covid)
str(spx)


avg_hourly_earnings$observation_date <- as.Date(avg_hourly_earnings$observation_date, format = "%Y-%m-%d")
avg_weekly_hours$observation_date <- as.Date(avg_weekly_hours$observation_date, format = "%Y-%m-%d")
cpi$observation_date <- as.Date(cpi$observation_date, format = "%Y-%m-%d")
fed_rate$observation_date <- as.Date(fed_rate$observation_date, format = "%Y-%m-%d")
labor_force$observation_date <- as.Date(labor_force$observation_date, format = "%Y-%m-%d")

gdp$observation_date <- as.Date(gdp$observation_date, format = "%Y-%m-%d")
rgdp$observation_date <- as.Date(rgdp$observation_date, format = "%Y-%m-%d")
gdp_growth$observation_date <- as.Date(gdp_growth$observation_date, format = "%Y-%m-%d")
covid$date <- as.Date(covid$date, format = "%Y-%m-%d")
spx$Date <- as.Date(spx$Date, format = "%Y-%m-%d")


# data starting from 2006 
data_06 <- data[(data$observation_date >= "2006-03-01") & (data$observation_date <= "2024-07-01"),]
labor_force_06 <- labor_force[(labor_force$observation_date >= "2006-03-01") & (labor_force$observation_date <= "2024-07-01"),]
cpi_06 <- cpi[(cpi$observation_date >= "2006-03-01") & (cpi$observation_date <= "2024-07-01"),]
fed_rate_06 <- fed_rate[(fed_rate$observation_date >= "2006-03-01") & (fed_rate$observation_date <= "2024-07-01"),]
avg_hourly_earnings_06 <- avg_hourly_earnings[(avg_hourly_earnings$observation_date >= "2006-03-01") & (avg_hourly_earnings$observation_date <= "2024-07-01"),]
avg_weekly_hours_06 <- avg_weekly_hours[(avg_weekly_hours$observation_date >= "2006-03-01") & (avg_weekly_hours$observation_date <= "2024-07-01"),]
gdp_06 <- gdp[gdp$observation_date >= "2006-03-01",]
rgdp_06 <- rgdp[rgdp$observation_date >= "2006-03-01",]
gdp_growth_06 <- gdp_growth[(gdp_growth$observation_date >= "2006-03-01") & (gdp_growth$observation_date <= "2024-07-01"),]
covid_06 <- covid[(covid$date >= "2006-02-01") & (covid$date <= "2024-07-01"),]
spx_06 <- spx[(spx$Date >= "2006-03-01") & (spx$Date <= "2024-07-01"),]

str(data_06)
str(fed_rate_06)
str(labor_force_06)
str(cpi_06)
str(avg_hourly_earnings_06)
str(avg_weekly_hours_06)

str(gdp_06)
str(rgdp_06)
str(gdp_growth_06)
str(covid_06)
str(spx_06)

plot(fed_rate_06$observation_date, fed_rate_06$FEDFUNDS, type="l", main="Federal Funds Excessive Rate in the US", xlab="Time", ylab="Rate (%)")
plot(labor_force_06$observation_date, labor_force_06$CLF16OV, type="l", main="Civilian Labor Force Level in the US", xlab="Time", ylab="Thousands of persons")
plot(cpi_06$observation_date, cpi_06$CPIAUCSL, type="l", main="Consumer Price Index in the US", xlab="Time", ylab="CPI")
plot(gdp_growth_06$observation_date, gdp_growth_06$BBKMGDP, type="l", main="GDP Growth in the US", xlab="Time", ylab="Rate (%)")
plot(gdp_06$observation_date, gdp_06$GDP, type="l", main="GDP in the US", xlab="Time", ylab="Billions of dollars")
plot(spx_06$Date, spx_06$Close, type="l", main="SPX", xlab="Time", ylab="Rate (%)")
plot(avg_hourly_earnings_06$observation_date, avg_hourly_earnings_06$CES0500000003, type="l", main="Average Hourly Earnings in th US", xlab="Time", ylab="Rate (%)")



### Arima models

unr <- data_06$UNRATE
unr.ts <- ts(unr, frequency=12)
tt <- data_06$observation_date

# selecting best parameters

aic_scores <- numeric(27)
idx = 1

for (i in 0:3) {
  for (j in 0:3) {
    for (k in 0:3) {
      print(c(i, j, k))
      test <- arima(unr, order=c(i, j, k))
      print(AIC(test))
      aic_scores[idx] = AIC(test)
      idx = idx + 1
    }
  }
}
min(aic_scores)
aic_scores

#best is (0, 1, 2)

a1 <- Arima(unr, order=c(0, 1, 2))
AIC(a1)
fit <- fitted(a1)
plot(tt, unr, type='l', main="Unemployment Rate in the US", xlab="Time", ylab="Rate (%)")
lines(tt, fit, col=2)
legend("topleft", legend=c("Original data", "ARIMA(0,1,2)"), col=c("black", "red"), lty=1, bty="n")

f1<- forecast(a1, h=24)
plot(f1)

res<- residuals(a1)
plot(res)
Acf(res, main="Residuals from ARIMA(0,1,2)")

par(mfrow=c(1,2))
Acf(res, main="Autocorrelation")
Pacf(res, main="Partial autocorrelation")
par(mfrow=c(1,1))




### TSLM

unr <- data_06$UNRATE[1:154]
unr.ts <- ts(unr, frequency=12)
str(unr)

fr <- fed_rate_06$FEDFUNDS[1:154]
fr.ts <- ts(fr, frequency=12)
lbf <- labor_force_06$CLF16OV[1:154]
lbf.ts <- ts(lbf, frequency=12)
cpiau <- cpi_06$CPIAUCSL[1:154]
cpiau.ts <- ts(cpiau, frequency=12)
ahe <- avg_hourly_earnings_06$CES0500000003[1:154]
ahe.ts <- ts(ahe, frequency=12)
awh <- avg_weekly_hours_06$AWHAETP[1:154]
awh.ts <- ts(awh, frequency=12)

gdp_d <- gdp_06$GDP[1:154]
gdp_d.ts <- ts(gdp_d, frequency=12)
rgdp_d <- rgdp_06$GDPC1[1:154]
rgdp_d.ts <- ts(rgdp_d, frequency=12)
gdpg_d <- gdp_growth_06$BBKMGDP[1:154]
gdpg_d.ts <- ts(gdpg_d, frequency=12)
spx_d <- spx_06$Close[1:154]
spx_d.ts <- ts(spx_d, frequency=12)

fit_base <- tslm(unr.ts~fr.ts+lbf.ts+cpiau.ts+ahe.ts+awh.ts+gdp_d.ts+rgdp_d.ts+gdpg_d.ts+spx_d.ts+trend+season)
summary(fit_base)
AIC(fit_base)

plot(unr.ts, type='l', ylim=c(2, 15), xlab="Time", ylab="Rate (%)")
lines(fitted(fit_base), col=2)
legend("topleft", legend=c("Original data", "baseline TSLM"), col=c("black", "red"), lty=1, bty="n")


# stepwise selection
fit_1 <- tslm(unr.ts~fr.ts+lbf.ts+cpiau.ts+ahe.ts+awh.ts+gdp_d.ts+rgdp_d.ts+gdpg_d.ts+spx_d.ts+trend)
summary(fit_1)
AIC(fit_1)

fit_2 <- tslm(unr.ts~lbf.ts+cpiau.ts+ahe.ts+awh.ts+gdp_d.ts+rgdp_d.ts+gdpg_d.ts+spx_d.ts+trend)
summary(fit_2)
AIC(fit_2)

fit_3 <- tslm(unr.ts~lbf.ts+cpiau.ts+ahe.ts+awh.ts+gdp_d.ts+rgdp_d.ts+gdpg_d.ts+trend)
summary(fit_3)
AIC(fit_3)

fit_4 <- tslm(unr.ts~lbf.ts+cpiau.ts+ahe.ts+awh.ts+gdp_d.ts+gdpg_d.ts+trend)
summary(fit_4)
AIC(fit_4)

fit_5 <- tslm(unr.ts~lbf.ts+cpiau.ts+ahe.ts+gdp_d.ts+gdpg_d.ts+trend)
summary(fit_5)
AIC(fit_5)

fit_tslm_best <- tslm(unr.ts~lbf.ts+cpiau.ts+ahe.ts+gdp_d.ts+gdpg_d.ts+trend-1)
summary(fit_tslm_best)
AIC(fit_tslm_best)

plot(tt[1:154], unr.ts, type='l', ylim=c(2, 15), xlab="Time", ylab="Rate (%)")
lines(tt[1:154], fitted(fit_base), col="blue")
lines(tt[1:154], fitted(fit_tslm_best), col="red")
legend("topleft", legend=c("Original data", "baseline TSLM", "best TSLM"), col=c("black", "blue", "red"), lty=1, bty="n")

res<- residuals(fit_tslm_best)
plot(res)
Acf(res, main="Residuals from best TSLM")
Pacf(res)

par(mfrow=c(1,2))
Acf(res, main="Autocorrelation")
Pacf(res, main="Partial autocorrelation")
par(mfrow=c(1,1))


# ARMAX refinement

s2_tslm <- auto.arima(unr.ts, xreg = fitted(fit_tslm_best))
summary(s2_tslm)
pres2_tslm <- fitted(s2_tslm)


plot(tt[1:154], unr.ts, type='l', ylim=c(0, 18), xlab="Time", ylab="Rate (%)")
lines(tt[1:154], fitted(fit_tslm_best), col="red")
lines(tt[1:154], pres2_tslm, lty=1,lwd=1, col="blue")
legend("topleft", legend=c("Original data", "best TSLM", "ARMAX refinement"), col=c("black", "red", "blue"), lty=1, bty="n")

res<- residuals(s2_tslm)
plot(res)
Acf(res, main="Residuals from best TSLM")
Pacf(res)

par(mfrow=c(1,2))
Acf(res, main="Autocorrelation")
Pacf(res, main="Partial autocorrelation")
par(mfrow=c(1,1))


# forecasting

unr <- data_06$UNRATE[155:221]
unr.ts <- ts(unr, frequency=12)
str(unr)

fr <- fed_rate_06$FEDFUNDS[155:221]
fr.ts <- ts(fr, frequency=12)
lbf <- labor_force_06$CLF16OV[155:221]
lbf.ts <- ts(lbf, frequency=12)
cpiau <- cpi_06$CPIAUCSL[155:221]
cpiau.ts <- ts(cpiau, frequency=12)
ahe <- avg_hourly_earnings_06$CES0500000003[155:221]
ahe.ts <- ts(ahe, frequency=12)
awh <- avg_weekly_hours_06$AWHAETP[155:221]
awh.ts <- ts(awh, frequency=12)

gdp_d <- gdp_06$GDP[155:221]
gdp_d.ts <- ts(gdp_d, frequency=12)
rgdp_d <- rgdp_06$GDPC1[155:221]
rgdp_d.ts <- ts(rgdp_d, frequency=12)
gdpg_d <- gdp_growth_06$BBKMGDP[155:221]
gdpg_d.ts <- ts(gdpg_d, frequency=12)
spx_d <- spx_06$Close[155:221]
spx_d.ts <- ts(spx_d, frequency=12)

df_155 <- data.frame(
  tt=tt[155:221],
  unr=unr.ts,
  fr=fr.ts,
  lbf=lbf.ts,
  cpiau=cpiau.ts,
  ahe=ahe.ts,
  awh=awh.ts,
  gdp_d=gdp_d.ts,
  rgdp_d=rgdp_d.ts,
  gdpg_d=gdpg_d.ts,
  spx_d=spx_d.ts
)
str(df_155)

# extend trend for test data
trend_test <- seq_along(df_155$unr) + 154 
df_155$trend <- trend_test

predicted_tslm_best <- predict(fit_tslm_best, newdata = df_155)
arima_pred_tslm <- forecast(s2_tslm, xreg = predicted_tslm_best, h = nrow(df_155))

plot(tt, data_06$UNRATE, type = "l", lwd = 2, ylim=c(2,18), xlab="Time", ylab="Rate (%)")
lines(tt[1:154], fitted(fit_tslm_best), lwd=2, col = "blue")
lines(df_155$tt, predicted_tslm_best, col = "red", lwd = 2)
abline(v=as.Date("2019-01-01"), lty=2, col=4)
legend("topleft", legend=c("Original data", "fitted by best TSLM", "predicted by best TSLM"), lwd=2, col=c("black", "blue", "red"), bty="n")

plot(tt, data_06$UNRATE, type = "l", lwd = 2, ylim=c(2,18), xlab="Time", ylab="Rate (%)")
lines(tt[1:154], fitted(fit_tslm_best), lwd=2, col = "yellow")
lines(tt[1:154], pres2_tslm, lty=1,lwd=2, col="green")
lines(df_155$tt, predicted_tslm_best, col = "blue", lwd = 2)
lines(df_155$tt, arima_pred_tslm$mean, col = "red", lwd = 2, lty=2)
abline(v=as.Date("2019-01-01"), lty=2, col=4)
legend("topleft", legend=c("Original data", "fitted by best TSLM", "fitted by ARMAX refinement", "predicted by best TSLM", "predicted by ARMAX refinement"), lwd=2, col=c("black", "yellow", "green", "blue", "red"), lty=c(1, 1, 1, 1, 2), bty="n")

mse <- mean((arima_pred_tslm$mean - data_06$UNRATE[155:221])^2)
mse




### prophet

unr <- data_06$UNRATE
unr.ts <- ts(unr)#, frequency=12)
fr <- fed_rate_06$FEDFUNDS
fr.ts <- ts(fr)#, frequency=12)
lbf <- labor_force_06$CLF16OV
lbf.ts <- ts(lbf)#, frequency=12)
cpiau <- cpi_06$CPIAUCSL
cpiau.ts <- ts(cpiau)#, frequency=12)
ahe <- avg_hourly_earnings_06$CES0500000003
ahe.ts <- ts(ahe)#, frequency=12)
awh <- avg_weekly_hours_06$AWHAETP
awh.ts <- ts(awh)#, frequency=12)

gdp_d <- gdp_06$GDP
gdp_d.ts <- ts(gdp_d)
rgdp_d <- rgdp_06$GDPC1
rgdp_d.ts <- ts(rgdp_d)
gdpg_d <- gdp_growth_06$BBKMGDP
gdpg_d.ts <- ts(gdpg_d)
covid_d <- covid_06$cases
covid_d.ts <- ts(covid_d)
covid_d.ts <- diff(covid_d.ts, lag=1)
spx_d <- spx_06$Close
spx_d.ts <- ts(spx_d)

df <- data.frame(
  y=unr.ts[1:160],
  ds=data_06$observation_date[1:160],
  fr=fr.ts[1:160],
  lbf=lbf.ts[1:160],
  cpiau=cpiau.ts[1:160],
  ahe=ahe.ts[1:160],
  awh=awh.ts[1:160],
  gdp_d=gdp_d.ts[1:160],
  rgdp_d=rgdp_d.ts[1:160],
  gdpg_d=gdpg_d.ts[1:160],
  covid=covid_d.ts[1:160],
  spx_d=spx_d.ts[1:160]
)
str(df)

str(unr[161:221])


m1 <- prophet(n.changepoints=25)
m1 <- add_regressor(m1, 'fr')
m1 <- add_regressor(m1, 'lbf')
m1 <- add_regressor(m1, 'cpiau')
m1 <- add_regressor(m1, 'ahe')
m1 <- add_regressor(m1, 'awh')
m1 <- add_regressor(m1, 'gdp_d')
m1 <- add_regressor(m1, 'rgdp_d')
m1 <- add_regressor(m1, 'gdpg_d')
m1 <- add_regressor(m1, 'covid')
m1 <- add_regressor(m1, 'spx_d')
m1 <- fit.prophet(m1, df)

summary(m1)

# future for prediction
future <- make_future_dataframe(m1, periods = 61, freq="month", include_history = T)
future$fr <- fr
future$lbf <- lbf
future$cpiau <- cpiau
future$ahe <- ahe
future$awh <- awh
future$gdp_d <- gdp_d
future$rgdp_d <- rgdp_d
future$gdpg_d <- gdpg_d
future$covid <- gdpg_d
future$spx_d <- spx_d


forecast <- predict(m1, future)

plot(m1, forecast)

dyplot.prophet(m1, forecast) 

#plot with change points
plot(m1, forecast)+add_changepoints_to_plot(m1, threshold=0)

#dates corresponding to change points
m1$changepoints



m2 <- prophet(seasonality.mode='multiplicative', n.changepoints=50)
m2 <- add_regressor(m2, 'fr')
m2 <- add_regressor(m2, 'lbf')
m2 <- add_regressor(m2, 'cpiau')
m2 <- add_regressor(m2, 'ahe')
m2 <- add_regressor(m2, 'awh')
m2 <- add_regressor(m2, 'gdp_d')
m2 <- add_regressor(m2, 'rgdp_d')
m2 <- add_regressor(m2, 'gdpg_d')
m2 <- add_regressor(m2, 'covid')
m2 <- add_regressor(m2, 'spx_d')
m2 <- fit.prophet(m2, df)

summary(m1)

# future for prediction
future <- make_future_dataframe(m2, periods = 61, freq="month", include_history = T)
future$fr <- fr
future$lbf <- lbf
future$cpiau <- cpiau
future$ahe <- ahe
future$awh <- awh
future$gdp_d <- gdp_d
future$rgdp_d <- rgdp_d
future$gdpg_d <- gdpg_d
future$covid <- gdpg_d
future$spx_d <- spx_d

forecast <- predict(m2, future)

plot(m2, forecast)

dyplot.prophet(m2, forecast) 

plot(m2, forecast)+add_changepoints_to_plot(m2, threshold=0)

m2$changepoints




### ses, holt, holt-winters
str(unr)

fit1<- ses(unr, alpha=0.4, initial="simple", h=5)
fit2<- ses(unr, alpha=0.6, initial="simple", h=5)
fit3<- ses(unr, alpha=0.8, initial='simple', h=5)

str(fit3)

fit4 <- ses(unr, alpha=0.9, initial='simple', h=24)
fit5 <- ses(unr, alpha=1, initial='simple', h=24)

plot(unr, type="l", ylab="Oil", xlab="Year")
lines(fitted(fit4), col="blue", type="l")
lines(fitted(fit3), col="green", type="l")
lines(fitted(fit5), col="red", type="l")

round(accuracy(fit4), 2)

summary(fit5)
summary(fit4)
summary(fit3)
summary(fit2)
summary(fit1)

autoplot(fit5)+
  autolayer(fitted(fit5), series="alpha = 1.0")+
  autolayer(fitted(fit4), series="alpha = 0.9")+
  autolayer(fitted(fit3), series="alpha = 0.8")+
  autolayer(fitted(fit2), series="alpha = 0.6")+
  autolayer(fitted(fit1), series="alpha = 0.4")+
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

# grid search
alpha_values <- seq(0.1, 1, by = 0.1)
h_values <- c(5, 10, 15, 20)
results <- data.frame(alpha = numeric(), h = numeric(), AIC = numeric())


for (alpha in alpha_values) {
  for (h in h_values) {
    fit <- ses(unr, alpha = alpha, initial = "simple", h = h)
    
    n <- length(unr)
    sse <- sum(residuals(fit)^2)
    
    # Number of parameters (alpha and the initial level)
    k <- 2

    aic_value <- n * log(sse / n) + 2 * k

    results <- rbind(results, data.frame(alpha = alpha, h = h, AIC = aic_value))
  }
}

best_model <- results[which.min(results$AIC), ]
print(results)
print(best_model)



# holt

unr <- data_06$UNRATE
unr.ts <- ts(unr)

fc<- holt(unr, h=24)
fc3 <- holt(unr, damped=T, h=24)

summary(fc)
summary(fc3)

autoplot(fc)+
  autolayer(fc, series="Holt's method", PI=F)+
  autolayer(fc3, series="Damped Holt's method", PI=F)
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

h_values <- c(5, 7, 10, 12, 15, 17, 20)

results <- data.frame(h = numeric(), AIC = numeric())

# grid search
for (h in h_values) {
    fit <- holt(unr, damped=T, h = h)
    
    sse <- sum(residuals(fit)^2)
    n <- length(unr)
    k <- 2

    aic_value <- n * log(sse / n) + 2 * k

    results <- rbind(results, data.frame(h = h, AIC = aic_value))
}

print(results)

best_model <- results[which.min(results$AIC), ]
print(best_model)


# holt-winters

unr <- data_06$UNRATE
unr.ts <- ts(unr, frequency=12)

fit1<- hw(unr.ts, seasonal="additive")
fit2<- hw(unr.ts, seasonal="multiplicative")
fit3<- hw(unr.ts, seasonal="additive", damped=T)
fit4<- hw(unr.ts, seasonal="multiplicative", damped=T)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

autoplot(fit1)+
  autolayer(fit1, series="Additive H-W", PI=F)+
  autolayer(fit2, series="Multiplicative H-W", PI=F)+
  autolayer(fit3, series="Additive damped H-W", PI=F)+
  autolayer(fit4, series="Multiplicative damped H-W", PI=F)+
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )




### GAM 

tt <- data_06$observation_date

unr <- data_06$UNRATE
unr.ts <- ts(unr, frequency=12)

fr <- fed_rate_06$FEDFUNDS
fr.ts <- ts(fr, frequency=12)
lbf <- labor_force_06$CLF16OV
lbf.ts <- ts(lbf, frequency=12)
cpiau <- cpi_06$CPIAUCSL
cpiau.ts <- ts(cpiau, frequency=12)
ahe <- avg_hourly_earnings_06$CES0500000003
ahe.ts <- ts(ahe, frequency=12)
awh <- avg_weekly_hours_06$AWHAETP
awh.ts <- ts(awh, frequency=12)

gdp_d <- gdp_06$GDP
gdp_d.ts <- ts(gdp_d, frequency=12)
rgdp_d <- rgdp_06$GDPC1
rgdp_d.ts <- ts(rgdp_d, frequency=12)
gdpg_d <- gdp_growth_06$BBKMGDP
gdpg_d.ts <- ts(gdpg_d, frequency=12)
spx_d <- spx_06$Close
spx_d.ts <- ts(spx_d, frequency=12)

df <- data.frame(
  tt=tt,
  unr=unr.ts,
  fr=fr.ts,
  lbf=lbf.ts,
  cpiau=cpiau.ts,
  ahe=ahe.ts,
  awh=awh.ts,
  gdp_d=gdp_d.ts,
  rgdp_d=rgdp_d.ts,
  gdpg_d=gdpg_d.ts,
  spx_d=spx_d.ts
)

train_data <- df[df$tt < "2019-01-01", ]
test_data <- df[df$tt >= "2019-01-01", ]


DEG <- 4
g_base <- gam(unr~s(tt, df=DEG)+s(fr, df=DEG)+s(lbf, df=DEG)+s(cpiau, df=DEG)+s(ahe, df=DEG)+s(awh, df=DEG)+s(gdp_d, df=DEG)+s(rgdp_d, df=DEG)+s(gdpg_d, df=DEG)+s(spx_d, df=DEG), data=train_data)
summary(g_base)
AIC(g_base)

# stepwise selection
g_1 <- gam(unr~s(tt, df=DEG)+s(fr, df=DEG)+s(lbf, df=DEG)+s(cpiau, df=DEG)+s(ahe, df=DEG)+s(gdp_d, df=DEG)+s(rgdp_d, df=DEG)+s(gdpg_d, df=DEG)+s(spx_d, df=DEG), data=train_data)
summary(g_1)
AIC(g_1)


for (DEG in 1:20) {
  g1 <- gam(unr~s(tt, df=DEG)+s(fr, df=DEG)+s(lbf, df=DEG)+s(cpiau, df=DEG)+s(ahe, df=DEG)+s(gdp_d, df=DEG)+s(rgdp_d, df=DEG)+s(gdpg_d, df=DEG)+s(spx_d, df=DEG), data=train_data)
  print('---------------')
  print(DEG)
  
  predictions <- predict(g1, newdata = test_data)
  
  mse <- mean((predictions - test_data$unr)^2)
  cat("Mean Squared Error (MSE):", mse, "\n")
  print(AIC(g1))
  
  plot(df$tt, df$unr, type = "l", col = "blue", lwd = 2,
       xlab = "Time", ylab = "Value", main = paste("Degree =", DEG), ylim=c(0,20))
  lines(test_data$tt, predictions, col = "red", lwd = 2)
}

DEG <- 3
g_1 <- gam(unr~s(tt, df=DEG)+s(fr, df=DEG)+s(lbf, df=DEG)+s(cpiau, df=DEG)+s(ahe, df=DEG)+s(gdp_d, df=DEG)+s(rgdp_d, df=DEG)+s(gdpg_d, df=DEG)+s(spx_d, df=DEG), data=train_data)
summary(g_1)
AIC(g_1)

predictions_gam <- predict(g_1, newdata = test_data)

mse <- mean((predictions_gam - test_data$unr)^2)
cat("Mean Squared Error (MSE):", mse, "\n")
print(AIC(g1))


plot(df$tt, df$unr, type = "l", lwd=2, ylim=c(2,16), xlab="Time", ylab="Rate (%)")
lines(test_data$tt, predictions_gam, col = "red", lwd=2)
lines(train_data$tt, fitted(g_1), col = "blue", lwd=2)
abline(v=as.Date("2019-01-01"), lty=2, col=4)
legend("topleft", legend=c("Original data", "fitted by best GAM", "predicted by best GAM"), lwd=2, col=c("black", "blue", "red"), lty=1, bty="n")


res<- residuals(g_1)
plot(res)
Acf(res)
pacf(res)

par(mfrow=c(1,2))
Acf(res, main="Autocorrelation")
Pacf(res, main="Partial autocorrelation")
par(mfrow=c(1,1))

# armax refinement
s2_gam <- auto.arima(train_data$unr, xreg = fitted(g_1))
summary(s2_gam)
pres2_gam <- fitted(s2_gam)

arima_pred_gam <- forecast(s2_gam, xreg = predictions_gam, h = nrow(test_data))

plot(df$tt, df$unr, type = "l", lwd=2, ylim=c(2,15), xlab="Time", ylab="Rate (%)")
lines(test_data$tt, predictions_gam, col = "blue", lwd=2)
lines(train_data$tt, fitted(g_1), col = "yellow", lwd=2)
lines(train_data$tt, pres2_gam, lwd=2, col="green")
lines(test_data$tt, arima_pred_gam$mean, lwd = 2, lty=2, col = "red")

abline(v=as.Date("2019-01-01"), lty=2, col=4)
legend("topleft", legend=c("Original data", "fitted by best GAM", "fitted by ARMAX refinement", "predicted by best GAM", "predicted by ARMAX refinement"), lwd=2, col=c("black", "yellow", "green", "blue", "red"), lty=c(1, 1, 1, 1, 2), bty="n")

res<- residuals(s2)
par(mfrow=c(1,2))
Acf(res, main="Autocorrelation")
Pacf(res, main="Partial autocorrelation")
par(mfrow=c(1,1))

mse <- mean((arima_pred_gam$mean - data_06$UNRATE[155:221])^2)
mse



### gradient boosting

tt <- data_06$observation_date
str(tt)

unr <- data_06$UNRATE
unr.ts <- ts(unr, frequency=12)

fr <- fed_rate_06$FEDFUNDS
fr.ts <- ts(fr, frequency=12)
lbf <- labor_force_06$CLF16OV
lbf.ts <- ts(lbf, frequency=12)
cpiau <- cpi_06$CPIAUCSL
cpiau.ts <- ts(cpiau, frequency=12)
ahe <- avg_hourly_earnings_06$CES0500000003
ahe.ts <- ts(ahe, frequency=12)
awh <- avg_weekly_hours_06$AWHAETP
awh.ts <- ts(awh, frequency=12)

gdp_d <- gdp_06$GDP
gdp_d.ts <- ts(gdp_d, frequency=12)
rgdp_d <- rgdp_06$GDPC1
rgdp_d.ts <- ts(rgdp_d, frequency=12)
gdpg_d <- gdp_growth_06$BBKMGDP
gdpg_d.ts <- ts(gdpg_d, frequency=12)
covid_d <- covid_06$cases
covid_d.ts <- ts(covid_d, frequency=12)
covid_d.ts <- diff(covid_d.ts, lag=1)
spx_d <- spx_06$Close
spx_d.ts <- ts(spx_d, frequency=12)

df <- data.frame(
  time=tt,
  target=unr.ts,
  fr=fr.ts,
  lbf=lbf.ts,
  cpiau=cpiau.ts,
  ahe=ahe.ts,
  awh=awh.ts,
  gdp_d=gdp_d.ts,
  rgdp_d=rgdp_d.ts,
  gdpg_d=gdpg_d.ts,
  #covid=covid_d.ts,
  spx_d=spx_d.ts
)
str(df)

set.seed(123)

train <- df[df$time < "2019-01-01", ]
test <- df[df$time >= "2019-01-01", ]
str(train)

n_trees <- c(500, 1000, 2000, 5000)
depths <- c(1, 3, 5, 7, 10)
shrinkages <- c(0.001, 0.005, 0.01, 0.02, 0.05, 0.1)
minobs <- c(3, 5, 7, 10, 12, 15)
results <- data.frame(n_trees = numeric(), best_n_t = numeric(), depth = numeric(), shrinkage = numeric(), minobs = numeric(), MSE = numeric())  # Store results

idx <- 1

# grid search
for (n_t in n_trees) {
  for (d in depths) {
    for (s in shrinkages) {
      for (mno in minobs) {
        set.seed(123)
        gbm_model <- gbm(
          formula = target ~ . - time,
          data = train,
          distribution = "gaussian",
          n.trees = n_t,
          interaction.depth = d,
          shrinkage = s,
          n.minobsinnode = mno
        )

        test_predictions <- predict(gbm_model, newdata = test, n.trees = 1:n_t)
        err = apply(test_predictions, 2, function(pred) mean((test$target-pred)^2))
        best=which.min(err)
        best_n_t = best
        mse = min(err)
        
        print(idx)
        idx = idx + 1

        results <- rbind(results, data.frame(n_trees = n_t, best_n_t = best_n_t, depth = d, shrinkage = s, minobs = mno, MSE = mse))
      }
    }
  }
}


best_model <- results[which.min(results$MSE), ]

print(results)
print(best_model)

set.seed(123)
gbm_model <- gbm(
  formula = target ~ . - time,
  data = train,
  distribution = "gaussian",
  n.trees = 500,
  interaction.depth = 1,
  shrinkage = 0.05,
  n.minobsinnode = 15,
)

test_predictions <- predict(gbm_model, newdata = test, n.trees = 1:500)
str(test_predictions[, 117])

mse <- mean((test$target - test_predictions[, 117])^2)
cat("Mean Squared Error on Test Set:", mse, "\n")

fit <- fitted(gbm_model)
fit
str(gbm_model)

summary(gbm_model)
str(predictions)

plot(df$time, df$target, type = "l", lwd = 2, main = "Boosting Results", ylim=c(2,15), xlab="Time", ylab="Rate (%)")
lines(train$time, gbm_model$fit, col = "blue", lwd=2)
lines(test$time, test_predictions[, 130], col = "red", lwd = 2)
abline(v=as.Date("2019-01-01"), lty=2, col=4)
legend("topleft", legend=c("Original data", "fitted by boosting", "predicted by boosting"), lwd=2, col=c("black", "blue", "red"), lty=1, bty="n")



### comparison OF ARMAX over TSLM and ARMAX over GAM

plot(df$tt, df$unr, type = "l", lwd = 2, ylim=c(2,18), xlab="Time", ylab="Rate (%)")
lines(df_155$tt, arima_pred_tslm$mean, col = "blue", lwd = 2, lty=2)
lines(test_data$tt, arima_pred_gam$mean, lwd = 2, lty=2, col = "red")
abline(v=as.Date("2019-01-01"), lty=2, col=4)
legend("topleft", legend=c("Original data", "ARMAX refinement of best TSLM", "ARMAX refinement of best GAM"), lwd=2, col=c("black", "blue", "red"), lty=c(1, 2, 2), bty="n")






