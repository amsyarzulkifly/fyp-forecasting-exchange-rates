library(dplyr)
library(e1071)
library(fable)
library(fpp3)
library(forecast)
library(furrr)
library(future.apply)
library(ggplot2)
library(lubridate)
library(purrr)
library(tsibble)
library(tidyr)
library(urca)
library(zoo)

exchangeRates <- read.csv(file.choose(), header=T)%>% select(Date, USD, EUR, GBP)
exchangeRates

data <- exchangeRates %>%
  mutate(Date = as.Date(Date, format = "%d-%b-%y")) %>%
  as_tsibble(index = Date)

#
exchangeRates$Date <- as.Date(exchangeRates$Date, format = "%d-%b-%y")

full_dates <- tibble(Date = seq(as.Date("2014-1-1"), as.Date("2022-12-31"), by = "day"))

data <- full_dates %>%
  left_join(exchangeRates, by = "Date") %>%
  as_tsibble(index = Date)

data <- data %>%
  mutate(USD = na.approx(USD, rule = 2, na.rm = FALSE), 
         EUR = na.approx(EUR, rule = 2, na.rm = FALSE), 
         GBP = na.approx(GBP, rule = 2, na.rm = FALSE))

### PLOT 2014-2023 ###

exchangeRates2 <- read.csv(file.choose(), header=T)%>% select(Date, USD, EUR, GBP)
exchangeRates2

data2 <- exchangeRates2 %>%
  mutate(Date = as.Date(Date, format = "%d-%b-%y")) %>%
  as_tsibble(index = Date)

exchangeRates2$Date <- as.Date(exchangeRates2$Date, format = "%d-%b-%y")

full_dates2 <- tibble(Date = seq(as.Date("2023-1-1"), as.Date("2023-12-31"), by = "day"))

data2 <- full_dates2 %>%
  left_join(exchangeRates2, by = "Date") %>%
  as_tsibble(index = Date)

data2 <- data2 %>%
   mutate(USD = na.approx(USD, rule = 2, na.rm = FALSE), 
          EUR = na.approx(EUR, rule = 2, na.rm = FALSE), 
          GBP = na.approx(GBP, rule = 2, na.rm = FALSE))

data2 %>% autoplot(USD)

data2014_2023 <- bind_rows(data, data2) 
data2014_2023 %>% autoplot(USD)

#Deskriptif
calculate_stats <- function(series) {
  adf_test <- ur.df(series, type = "drift", selectlags = "AIC")  
  list(
    Minimum = min(series, na.rm = TRUE),
    Maksimum = max(series, na.rm = TRUE),
    Min = mean(series, na.rm = TRUE),
    Kuartil Pertama = quantile(series, 0.25, na.rm = TRUE),
    Median = median(series, na.rm = TRUE),
    Kuartil Ke-3 = quantile(series, 0.75, na.rm = TRUE),
    Kepencongan = skewness(series, na.rm = TRUE),
    Ujian ADF = summary(adf_test)@teststat[1] )}

usd_stats <- calculate_stats(data$USD)
eur_stats <- calculate_stats(data$EUR)
gbp_stats <- calculate_stats(data$GBP)

### PLOT ###

#Overall Plot
data_long <- data %>%
  pivot_longer(cols = c(USD, EUR, GBP), names_to = "Currency", values_to = "ExchangeRate") 

ggplot(data_long, aes(x = Date, y = ExchangeRate, color = Currency)) +
  geom_line() +
  labs(
    title = "Kadar Tukaran Mata Wang MYR/USD, MYR/EUR dan MYR/GBP Tahun 2014 hingga 2022",
    x = "Tahun",
    y = "Nilai Kadar Tukaran",
    color = "Jenis Kadar Tukaran"
  ) +
  scale_color_manual(
    values = c("USD" = "blue", "EUR" = "#228B22", "GBP" = "red"),
    labels = c("USD" = "MYR/USD", "EUR" = "MYR/EUR", "GBP" = "MYR/GBP") )

#
data %>% autoplot(USD) +
ggtitle("Kadar Tukaran Mata Wang MYR/USD Tahun 2014 hingga 2022") + 
xlab("Tahun") + 
ylab("Nilai Kadar Tukaran")

#
data %>% autoplot(EUR) +
ggtitle("Kadar Tukaran Mata Wang MYR/EUR Tahun 2014 hingga 2022") + 
xlab("Tahun") + 
ylab("Nilai Kadar Tukaran")

#
data %>% autoplot(GBP) +
ggtitle("Kadar Tukaran Mata Wang MYR/GBP Tahun 2014 hingga 2022") + 
xlab("Tahun") + 
ylab("Nilai Kadar Tukaran")

### ACF & PACF ###
data %>% ACF(USD) %>% autoplot()+
ggtitle("ACF Kadar Tukaran Mata Wang MYR/USD") + 
xlab("Lat") + 
ylab("ACF")

data %>% PACF(USD) %>% autoplot()+
ggtitle("PACF Kadar Tukaran Mata Wang MYR/USD") + 
xlab("Lat") + 
ylab("PACF")

data %>% ACF(diff(USD)) %>% autoplot()+
ggtitle("ACF Kadar Tukaran Mata Wang MYR/USD setelah dibezakan, d=1") + 
xlab("Lat") + 
ylab("ACF")

data %>% PACF(diff(USD)) %>% autoplot()+
ggtitle("PACF Kadar Tukaran Mata Wang MYR/USD setelah dibezakan, d=1") + 
xlab("Lat") + 
ylab("PACF")

#
data %>% ACF(EUR) %>% autoplot()+
ggtitle("ACF Kadar Tukaran Mata Wang MYR/EUR") + 
xlab("Lat") + 
ylab("ACF")

data %>% PACF(EUR) %>% autoplot()+
ggtitle("PACF Kadar Tukaran Mata Wang MYR/EUR") + 
xlab("Lat") + 
ylab("PACF")

data %>% ACF(diff(EUR)) %>% autoplot()+
ggtitle("ACF Kadar Tukaran Mata Wang MYR/EUR setelah dibezakan, d=1") + 
xlab("Lat") + 
ylab("ACF")

data %>% PACF(diff(EUR)) %>% autoplot()+
ggtitle("PACF Kadar Tukaran Mata Wang MYR/EUR setelah dibezakan, d=1") + 
xlab("Lat") + 
ylab("PACF")

#
data %>% ACF(GBP) %>% autoplot()+
ggtitle("ACF Kadar Tukaran Mata Wang MYR/GBP") + 
xlab("Lat") + 
ylab("ACF")

data %>% PACF(GBP) %>% autoplot()+
ggtitle("PACF Kadar Tukaran Mata Wang MYR/GBP") + 
xlab("Lat") + 
ylab("PACF")

data %>% ACF(diff(GBP)) %>% autoplot()+
ggtitle("ACF Kadar Tukaran Mata Wang MYR/GBP setelah dibezakan, d=1") + 
xlab("Lat") + 
ylab("ACF")

data %>% PACF(diff(GBP)) %>% autoplot()+
ggtitle("PACF Kadar Tukaran Mata Wang MYR/GBP setelah dibezakan, d=1") + 
xlab("Lat") + 
ylab("PACF")

### ARIMA ###

#
fit_data_autoUSD <- data %>% model(ARIMA(USD))
report(fit_data_autoUSD)
accuracy(fit_data_autoUSD)

fit_data_autoUSD %>% forecast(h=365) %>% autoplot(data) +
  ggtitle("Ramalan Kadar Tukaran Mata Wang MYR/USD Tahun 2023 menggunakan ARIMA(1,1,1)") + xlab("Tahun") + ylab("Nilai Kadar Tukaran")  
fit_data_autoUSD %>% forecast(h=365)
view(fit_data_autoUSD %>% forecast(h=365))

#
fit_data_autoEUR <- data %>% model(ARIMA(EUR))
report(fit_data_autoEUR)
accuracy(fit_data_autoEUR)

fit_data_autoEUR %>% forecast(h=365) %>% autoplot(data) +
  ggtitle("Ramalan Kadar Tukaran Mata Wang MYR/EUR Tahun 2023 menggunakan ARIMA(1,1,0)") + xlab("Tahun") + ylab("Nilai Kadar Tukaran")      
fit_data_autoEUR %>% forecast(h=365)
view(fit_data_autoEUR %>% forecast(h=365))

#
fit_data_autoGBP <- data %>% model(ARIMA(GBP))
report(fit_data_autoGBP)
accuracy(fit_data_autoGBP)

fit_data_autoGBP %>% forecast(h=365) %>% autoplot(data) +
  ggtitle("Ramalan Kadar Tukaran Mata Wang MYR/GBP Tahun 2023 menggunakan ARIMA(1,1,1)") + xlab("Tahun") + ylab("Nilai Kadar Tukaran")       
fit_data_autoGBP %>% forecast(h=365)
view(fit_data_autoGBP %>% forecast(h=365))

###ADF Test
#
adf_test_usd <- ur.df(data$USD, type = "drift", selectlags = "AIC")
summary(adf_test_usd)

diff_usd <- diff(data$USD)  # First differencing
adf_test_usd_diff <- ur.df(diff_usd, type = "drift", selectlags = "AIC")
summary(adf_test_usd_diff)

#
adf_test_eur <- ur.df(data$EUR, type = "drift", selectlags = "AIC")
summary(adf_test_eur)

diff_eur <- diff(data$EUR)  # First differencing
adf_test_eur_diff <- ur.df(diff_eur, type = "drift", selectlags = "AIC")
summary(adf_test_eur_diff)

#
adf_test_gbp <- ur.df(data$GBP, type = "drift", selectlags = "AIC")
summary(adf_test_gbp)

diff_gbp <- diff(data$GBP)  # First differencing
adf_test_gbp_diff <- ur.df(diff_gbp, type = "drift", selectlags = "AIC")
summary(adf_test_gbp_diff)


###residuals arima

#USD
fit_data_autoUSD %>% gg_tsresiduals()

augment(fit_data_autoUSD) %>%
 ggplot(aes(x=.fitted, y=.resid)) +
 geom_point() +
 geom_hline(yintercept = 0, color = "red")

report(fit_data_autoUSD)
accuracy(fit_data_autoUSD)

#EUR
fit_data_autoEUR %>% gg_tsresiduals()

augment(fit_data_autoEUR) %>%
 ggplot(aes(x=.fitted, y=.resid)) +
 geom_point() +
 geom_hline(yintercept = 0, color = "red")

report(fit_data_autoEUR)
accuracy(fit_data_autoEUR)

#GBP
fit_data_autoGBP %>% gg_tsresiduals()

augment(fit_data_autoGBP) %>%
 ggplot(aes(x=.fitted, y=.resid)) +
 geom_point() +
 geom_hline(yintercept = 0, color = "red")

report(fit_data_autoGBP)
accuracy(fit_data_autoGBP)

#copypaste
write.table(data, "clipboard", sep = "\t", row.names = FALSE)

###comparison arima vs actual

#USD
forecast_arimaUSD2023 <- fit_data_autoUSD %>% forecast(h = 365)

forecast_arimaUSD2023 %>%
  autoplot(data) + 
  autolayer(data2, USD, color = "red", alpha = 0.5, ) +  
  ggtitle("Perbandingan antara Nilai Sebenar Kadar Tukaran Mata Wang MYR/USD 2023 dengan Ramalan menggunakan ARIMA (1,1,1)") +
  xlab("Tahun") +
  ylab("Nilai Kadar Tukaran") 

#EUR
forecast_arimaEUR2023 <- fit_data_autoEUR %>% forecast(h = 365)

forecast_arimaEUR2023 %>%
  autoplot(data) + 
  autolayer(data2, EUR, color = "red", alpha = 0.5, ) +  
  ggtitle("Perbandingan antara Nilai Sebenar Kadar Tukaran Mata Wang MYR/EUR 2023 dengan Ramalan menggunakan ARIMA (0,1,1)") +
  xlab("Tahun") +
  ylab("Nilai Kadar Tukaran") 

#GBP
forecast_arimaGBP2023 <- fit_data_autoGBP %>% forecast(h = 365)

forecast_arimaGBP2023 %>%
  autoplot(data) + 
  autolayer(data2, GBP, color = "red", alpha = 0.5, ) +  
  ggtitle("Perbandingan antara Nilai Sebenar Kadar Tukaran Mata Wang MYR/GBP 2023 dengan Ramalan menggunakan ARIMA (1,1,1)") +
  xlab("Tahun") +
  ylab("Nilai Kadar Tukaran")


### EXPERIMENT ARIMA ###

train_data <- data %>% filter(Date >= as.Date("2022-12-01"))

data_split <- train_data %>%
  mutate(period = floor_date(Date, unit = "1 days")) %>%
  group_by(period) %>% group_split()


###USD
fit_arima_forecastUSD <- function(sub_data) {

  fit <- auto.arima(sub_data$USD)

  forecast_result <- forecast(fit, h = 10)
  
  forecast_df <- data.frame(
    Date = seq(max(sub_data$Date) + 1, by = "day", length.out = 1),
    Forecast = forecast_result$mean
  )
  
  return(forecast_df)
}

forecast_resultsUSD <- map(data_split, fit_arima_forecastUSD)
combined_forecastUSD <- bind_rows(forecast_resultsUSD)

#

forecasted_valuesUSD <- combined_forecastUSD %>% select(-Date) %>% rename(USD = Forecast) 

forecasted_values_dataUSD <- forecasted_valuesUSD %>%
  mutate(Date = as.Date("2023-1-1") + (row_number() - 1)) %>%
  as_tsibble(index = Date) 

forecasted_values_dataUSD <- forecasted_values_dataUSD %>%
  bind_rows( tibble(Date = seq(max(forecasted_values_dataUSD$Date) + 1, 
  as.Date("2023-12-31"), by = "day"),
  USD = NA_real_, EUR = NA_real_, GBP = NA_real_)
  ) %>%
  mutate(
    USD = zoo::na.approx(USD, rule = 2, na.rm = FALSE),
    EUR = zoo::na.approx(EUR, rule = 2, na.rm = FALSE),
    GBP = zoo::na.approx(GBP, rule = 2, na.rm = FALSE)) 

view(forecasted_values_dataUSD)

combined_dataUSD <- bind_rows(data, forecasted_values_dataUSD) 
combined_dataUSD %>% autoplot(USD)

#

std_dev_USD <- sd(forecasted_values_dataUSD$USD, na.rm = TRUE)
n <- nrow(forecasted_values_dataUSD)
df <- n - 1

t_value_95 <- qt(0.975, df)
t_value_80 <- qt(0.90, df)

forecasted_values_dataUSD <- forecasted_values_dataUSD %>%
  mutate(
    Time_Horizon = row_number(),  
    Scaling_Factor = 0.75 * sqrt(Time_Horizon),      

    USD_lower_95 = USD - (t_value_95 * std_dev_USD * Scaling_Factor),
    USD_upper_95 = USD + (t_value_95 * std_dev_USD * Scaling_Factor),
    USD_lower_80 = USD - (t_value_80 * std_dev_USD * Scaling_Factor),
    USD_upper_80 = USD + (t_value_80 * std_dev_USD * Scaling_Factor)
  )

#comparison plot

fits_arima_USD <- fitted(fit_data_autoUSD, h = 30) %>% mutate(.fitted = na.approx(.fitted, rule = 2, na.rm = FALSE))

ggplot() +
  geom_line(data = as.data.frame(data2014_2023), aes(x = Date, y = USD, color = "Nilai Sebenar")) +
  geom_line(data = forecasted_values_dataUSD, aes(x = Date, y = USD, color = "Nilai Ramalan")) +
  geom_line(data = as.data.frame(fits_arima_USD), aes(x = Date, y = .fitted, color = "Nilai Terlaras")) +
  
  geom_ribbon(data = forecasted_values_dataUSD, 
              aes(x = Date, ymin = USD_lower_95, ymax = USD_upper_95, fill = "95%"), 
              alpha = 0.2) +                
  geom_ribbon(data = forecasted_values_dataUSD, 
              aes(x = Date, ymin = USD_lower_80, ymax = USD_upper_80, fill = "80%"), 
              alpha = 0.35) +                
  
  ggtitle("Perbandingan antara Nilai Sebenar Kadar Tukaran Mata Wang MYR/USD 2023 dengan Ramalan menggunakan ARIMA (1,1,1)") + 
  xlab("Tahun") + 
  ylab("Nilai Kadar Tukaran") +
  
  scale_color_manual(
    name = "", 
    values = c("Nilai Sebenar" = "black", 
               "Nilai Ramalan" = "blue",
               "Nilai Terlaras" = "#D55E00")) +
  
  scale_fill_manual(
    name = "Selang Keyakinan", 
    values = c("95%" = "blue", 
               "80%" = "blue"))




###EUR
fit_arima_forecastEUR <- function(sub_data) {

  fit <- auto.arima(sub_data$EUR)

  forecast_result <- forecast(fit, h = 10)
  
  forecast_df <- data.frame(
    Date = seq(max(sub_data$Date) + 1, by = "day", length.out = 1),
    Forecast = forecast_result$mean
  )
  
  return(forecast_df)
}

forecast_resultsEUR <- map(data_split, fit_arima_forecastEUR)
combined_forecastEUR <- bind_rows(forecast_resultsEUR)

#

forecasted_valuesEUR <- combined_forecastEUR %>% select(-Date) %>% rename(EUR = Forecast) 

forecasted_values_dataEUR <- forecasted_valuesEUR %>%
  mutate(Date = as.Date("2023-1-1") + (row_number() - 1)) %>%
  as_tsibble(index = Date) 

forecasted_values_dataEUR <- forecasted_values_dataEUR %>%
  bind_rows( tibble(Date = seq(max(forecasted_values_dataEUR$Date) + 1, 
  as.Date("2023-12-31"), by = "day"),
  USD = NA_real_, EUR = NA_real_, GBP = NA_real_)
  ) %>%
  mutate(
    USD = zoo::na.approx(USD, rule = 2, na.rm = FALSE),
    EUR = zoo::na.approx(EUR, rule = 2, na.rm = FALSE),
    GBP = zoo::na.approx(GBP, rule = 2, na.rm = FALSE)) 

view(forecasted_values_dataEUR)

combined_dataEUR <- bind_rows(data, forecasted_values_dataEUR) 
combined_dataEUR %>% autoplot(EUR)

#

std_dev_EUR <- sd(forecasted_values_dataEUR$EUR, na.rm = TRUE)
n <- nrow(forecasted_values_dataEUR)
df <- n - 1

t_value_95 <- qt(0.975, df)
t_value_80 <- qt(0.90, df)

forecasted_values_dataEUR <- forecasted_values_dataEUR %>%
  mutate(
    Time_Horizon = row_number(),  
    Scaling_Factor = 0.5 * sqrt(Time_Horizon),  
    
    EUR_lower_95 = EUR - (t_value_95 * std_dev_EUR * Scaling_Factor),
    EUR_upper_95 = EUR + (t_value_95 * std_dev_EUR * Scaling_Factor),
    EUR_lower_80 = EUR - (t_value_80 * std_dev_EUR * Scaling_Factor),
    EUR_upper_80 = EUR + (t_value_80 * std_dev_EUR * Scaling_Factor)
  )

#comparison

fits_arima_EUR <- fitted(fit_data_autoEUR, h = 30) %>% mutate(.fitted = na.approx(.fitted, rule = 2, na.rm = FALSE))

ggplot() +
  geom_line(data = as.data.frame(data2014_2023), aes(x = Date, y = EUR, color = "Nilai Sebenar")) +
  geom_line(data = forecasted_values_dataEUR, aes(x = Date, y = EUR, color = "Nilai Ramalan")) +
  geom_line(data = as.data.frame(fits_arima_EUR), aes(x = Date, y = .fitted, color = "Nilai Terlaras")) +
  
  geom_ribbon(data = forecasted_values_dataEUR, 
              aes(x = Date, ymin = EUR_lower_95, ymax = EUR_upper_95, fill = "95%"), 
              alpha = 0.2) +                
  geom_ribbon(data = forecasted_values_dataEUR, 
              aes(x = Date, ymin = EUR_lower_80, ymax = EUR_upper_80, fill = "80%"), 
              alpha = 0.35) +                
  
  ggtitle("Perbandingan antara Nilai Sebenar Kadar Tukaran Mata Wang MYR/EUR 2023 dengan Ramalan menggunakan ARIMA (0,1,1)") + 
  xlab("Tahun") + 
  ylab("Nilai Kadar Tukaran") +
  
  scale_color_manual(
    name = "", 
    values = c("Nilai Sebenar" = "black", 
               "Nilai Ramalan" = "blue",
               "Nilai Terlaras" = "#D55E00")) +
  
  scale_fill_manual(
    name = "Selang Keyakinan", 
    values = c("95%" = "blue", 
               "80%" = "blue"))



###GBP
fit_arima_forecastGBP <- function(sub_data) {

  fit <- auto.arima(sub_data$GBP)

  forecast_result <- forecast(fit, h = 10)
  
  forecast_df <- data.frame(
    Date = seq(max(sub_data$Date) + 1, by = "day", length.out = 1),
    Forecast = forecast_result$mean
  )
  
  return(forecast_df)
}

forecast_resultsGBP <- map(data_split, fit_arima_forecastGBP)

combined_forecastGBP <- bind_rows(forecast_resultsGBP)

#

forecasted_valuesGBP <- combined_forecastGBP %>% select(-Date) %>% rename(GBP = Forecast) 

forecasted_values_dataGBP <- forecasted_valuesGBP %>%
  mutate(Date = as.Date("2023-1-1") + (row_number() - 1)) %>%
  as_tsibble(index = Date) 

forecasted_values_dataGBP <- forecasted_values_dataGBP %>%
  bind_rows( tibble(Date = seq(max(forecasted_values_dataGBP$Date) + 1, 
  as.Date("2023-12-31"), by = "day"),
  USD = NA_real_, EUR = NA_real_, GBP = NA_real_)
  ) %>%
  mutate(
    USD = zoo::na.approx(USD, rule = 2, na.rm = FALSE),
    EUR = zoo::na.approx(EUR, rule = 2, na.rm = FALSE),
    GBP = zoo::na.approx(GBP, rule = 2, na.rm = FALSE)) 

view(forecasted_values_dataGBP)

combined_dataGBP <- bind_rows(data, forecasted_values_dataGBP) 
combined_dataGBP %>% autoplot(GBP)

#

std_dev_GBP <- sd(forecasted_values_dataGBP$GBP, na.rm = TRUE)
n <- nrow(forecasted_values_dataGBP)
df <- n - 1


t_value_95 <- qt(0.975, df)  # 95% confidence interval (two-tailed)
t_value_80 <- qt(0.90, df)  # 80% confidence interval (two-tailed)

forecasted_values_dataGBP <- forecasted_values_dataGBP %>%
  mutate(
    Time_Horizon = row_number(),  
    Scaling_Factor = 0.5 * sqrt(Time_Horizon),  

    GBP_lower_95 = GBP - (t_value_95 * std_dev_GBP * Scaling_Factor),
    GBP_upper_95 = GBP + (t_value_95 * std_dev_GBP * Scaling_Factor),
    GBP_lower_80 = GBP - (t_value_80 * std_dev_GBP * Scaling_Factor),
    GBP_upper_80 = GBP + (t_value_80 * std_dev_GBP * Scaling_Factor)
  )

#comparison plot

fits_arima_GBP <- fitted(fit_data_autoGBP, h = 30) %>% mutate(.fitted = na.approx(.fitted, rule = 2, na.rm = FALSE))

ggplot() +
  geom_line(data = as.data.frame(data2014_2023), aes(x = Date, y = GBP, color = "Nilai Sebenar")) +
  geom_line(data = forecasted_values_dataGBP, aes(x = Date, y = GBP, color = "Nilai Ramalan")) +
  geom_line(data = as.data.frame(fits_arima_GBP), aes(x = Date, y = .fitted, color = "Nilai Terlaras")) +
  
  geom_ribbon(data = forecasted_values_dataGBP, 
              aes(x = Date, ymin = GBP_lower_95, ymax = GBP_upper_95, fill = "95%"), 
              alpha = 0.2) +                
  geom_ribbon(data = forecasted_values_dataGBP, 
              aes(x = Date, ymin = GBP_lower_80, ymax = GBP_upper_80, fill = "80%"), 
              alpha = 0.35) +                
  
  ggtitle("Perbandingan antara Nilai Sebenar Kadar Tukaran Mata Wang MYR/GBP 2023 dengan Ramalan menggunakan ARIMA (1,1,1)") + 
  xlab("Tahun") + 
  ylab("Nilai Kadar Tukaran") +
  
  scale_color_manual(
    name = "", 
    values = c("Nilai Sebenar" = "black", 
               "Nilai Ramalan" = "blue",
               "Nilai Terlaras" = "#D55E00")) +
  
  scale_fill_manual(
    name = "Selang Keyakinan", 
    values = c("95%" = "blue", 
               "80%" = "blue"))


###test residuals 

#USD
comparisonUSD <- merge(data2, forecasted_values_dataUSD, by = "Date", suffixes = c("_actual", "_forecast")) %>% 
	mutate(Residuals = USD_actual - USD_forecast) %>% select(Date, USD_actual, USD_forecast, Residuals)

mseUSD <- mean(comparisonUSD$Residuals^2)
rmseUSD <- sqrt(mseUSD)
maeUSD <- mean(abs(comparisonUSD$Residuals))
mapeUSD <- mean(abs((comparisonUSD$USD_actual - comparisonUSD$USD_forecast) / comparisonUSD$USD_actual)) * 100



mseUSD
rmseUSD
maeUSD
mapeUSD 

#EUR
comparisonEUR <- merge(data2, forecasted_values_dataEUR, by = "Date", suffixes = c("_actual", "_forecast")) %>% 
	mutate(Residuals = EUR_actual - EUR_forecast) %>% select(Date, EUR_actual, EUR_forecast, Residuals)

mseEUR <- mean(comparisonEUR$Residuals^2)
rmseEUR <- sqrt(mseEUR)
maeEUR <- mean(abs(comparisonEUR$Residuals))

mseEUR
rmseEUR
maeEUR


#GBP
comparisonGBP <- merge(data2, forecasted_values_dataGBP, by = "Date", suffixes = c("_actual", "_forecast")) %>% 
	mutate(Residuals = GBP_actual - GBP_forecast) %>% select(Date, GBP_actual, GBP_forecast, Residuals)

mseGBP <- mean(comparisonGBP$Residuals^2)
rmseGBP <- sqrt(mseGBP)
maeGBP <- mean(abs(comparisonGBP$Residuals))

mseGBP
rmseGBP
maeGBP 


###train residual arima

#USD

fits_arima_USD2 <- fits_arima_USD %>%  rename(USD = .fitted)
comparisonUSD <- merge(data, fits_arima_USD2, by = "Date", suffixes = c("_actual", "_forecast")) %>% 
	mutate(Residuals = USD_actual - USD_forecast) %>% select(Date, USD_actual, USD_forecast, Residuals)

mseUSD <- mean(comparisonUSD$Residuals^2)
rmseUSD <- sqrt(mseUSD)
maeUSD <- mean(abs(comparisonUSD$Residuals))

mseUSD
rmseUSD
maeUSD

#EUR

fits_arima_EUR2 <- fits_arima_EUR %>%  rename(EUR = .fitted)
comparisonEUR <- merge(data, fits_arima_EUR2, by = "Date", suffixes = c("_actual", "_forecast")) %>% 
	mutate(Residuals = EUR_actual - EUR_forecast) %>% select(Date, EUR_actual, EUR_forecast, Residuals)

mseEUR <- mean(comparisonEUR$Residuals^2)
rmseEUR <- sqrt(mseEUR)
maeEUR <- mean(abs(comparisonEUR$Residuals))

mseEUR
rmseEUR
maeEUR


#GBP

fits_arima_GBP2 <- fits_arima_GBP %>%  rename(GBP = .fitted)
comparisonGBP <- merge(data, fits_arima_GBP2, by = "Date", suffixes = c("_actual", "_forecast")) %>% 
	mutate(Residuals = GBP_actual - GBP_forecast) %>% select(Date, GBP_actual, GBP_forecast, Residuals)

mseGBP <- mean(comparisonGBP$Residuals^2)
rmseGBP <- sqrt(mseGBP)
maeGBP <- mean(abs(comparisonGBP$Residuals))

mseGBP
rmseGBP
maeGBP 


###train residual darima

#USD

fits_darima_USD2 <- fits_darima_USD %>%  rename(USD = .fitted)
comparisonUSD <- merge(data, fits_darima_USD2, by = "Date", suffixes = c("_actual", "_forecast")) %>% 
	mutate(Residuals = USD_actual - USD_forecast) %>% select(Date, USD_actual, USD_forecast, Residuals)

mseUSD <- mean(comparisonUSD$Residuals^2)
rmseUSD <- sqrt(mseUSD)
maeUSD <- mean(abs(comparisonUSD$Residuals))

mseUSD
rmseUSD
maeUSD

#EUR

fits_darima_EUR2 <- fits_darima_EUR %>%  rename(EUR = .fitted)
comparisonEUR <- merge(data, fits_darima_EUR2, by = "Date", suffixes = c("_actual", "_forecast")) %>% 
	mutate(Residuals = EUR_actual - EUR_forecast) %>% select(Date, EUR_actual, EUR_forecast, Residuals)

mseEUR <- mean(comparisonEUR$Residuals^2)
rmseEUR <- sqrt(mseEUR)
maeEUR <- mean(abs(comparisonEUR$Residuals))

mseEUR
rmseEUR
maeEUR


#GBP

fits_darima_GBP2 <- fits_darima_GBP %>%  rename(GBP = .fitted)
comparisonGBP <- merge(data, fits_darima_GBP2, by = "Date", suffixes = c("_actual", "_forecast")) %>% 
	mutate(Residuals = GBP_actual - GBP_forecast) %>% select(Date, GBP_actual, GBP_forecast, Residuals)

mseGBP <- mean(comparisonGBP$Residuals^2)
rmseGBP <- sqrt(mseGBP)
maeGBP <- mean(abs(comparisonGBP$Residuals))

mseGBP
rmseGBP
maeGBP

###

train_data <- data

fit_arima_USD <- train_data %>%
  model(ARIMA(USD))
fit_arima_USD %>%
  forecast(h = 1) %>%
  autoplot(data)
