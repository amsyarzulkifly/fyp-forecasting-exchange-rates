# Forecasting Malaysia's Exchange Rates using ARIMA and DARIMA Models  

## 📌 Project Overview  
This final year project forecasts Malaysian Ringgit (MYR) exchange rates using **Autoregressive Integrated Moving Average (ARIMA)** and **Distributed ARIMA (DARIMA)** models. The study is based on daily exchange rate data from Bank Negara Malaysia (BNM) spanning 2014 to 2022 and focuses on three major currencies: the **United States Dollar (USD)**, **Euro (EUR)**, and **British Pound Sterling (GBP)**.

## 🔎 Why ARIMA and DARIMA?  
Exchange rates are sequential time series data that often exhibit **trends, seasonality, and autocorrelation**.  

- **ARIMA** is one of the most established statistical models for time series forecasting. It effectively captures temporal dependencies, making it suitable for financial data like exchange rates.  
- **DARIMA (Distributed ARIMA)** extends ARIMA by partitioning large datasets into smaller subsets, fitting ARIMA models individually, and then combining the results. This approach improves forecasting accuracy and computational efficiency, especially for high-frequency financial data.  

These models provide a strong statistical foundation and serve as reliable benchmarks for evaluating more advanced machine learning or deep learning approaches.  

## 📂 Dataset  
- **Source**: Bank Negara Malaysia (BNM)  
- **Frequency**: Daily exchange rate data  
- **Training Period**: 2014 – 2022  
- **Testing Period**: 2023  
- **Currencies**: MYR/USD, MYR/EUR, MYR/GBP  

## ⚙️ Methodology  
1. **Data Preprocessing**  
   - Collected daily exchange rates from BNM.  
   - Handled missing values using interpolation for weekends and public holidays.  
   - Transformed data into a standardized time series format.  

2. **Stationarity Check**  
   - Applied Augmented Dickey-Fuller (ADF) test to confirm stationarity.  
   - Differencing was applied when necessary.  

3. **Modeling**  
   - **ARIMA**: Traditional time series model fitted using auto.arima in R.  
   - **DARIMA**: Data split into smaller yearly subseries, fitted with ARIMA individually, then combined into a global model.  

4. **Evaluation Metrics**  
   - Mean Squared Error (MSE)  
   - Root Mean Squared Error (RMSE)  
   - Mean Absolute Error (MAE)  
   - Mean Absolute Percentage Error (MAPE)  

5. **Forecasting**  
   - Conducted one-step-ahead forecasts for 2023.  
   - Generated 95% confidence intervals.  

## 📊 Key Findings  
- **DARIMA outperformed ARIMA**, achieving lower MSE, RMSE, MAE, and MAPE across MYR/USD, MYR/EUR, and MYR/GBP.  
- Both models produced reliable forecasts, with actual 2023 values falling within their confidence intervals.  
- External factors such as geopolitical conflicts and monetary policies strongly influence MYR movements, highlighting the limits of purely statistical models.  

---

## 🖼️ Visualizations  

### 📈 ARIMA Forecasts  

- **MYR/USD (ARIMA)**  
<div align="left">
  <img src="https://github.com/amsyarzulkifly/fyp-forecasting-exchange-rates/raw/main/attachment/myr_to_usd_arima.PNG" alt="MYR to USD ARIMA" width="800" height="450"/>
  <p><em>Forecasted MYR/USD exchange rate using ARIMA model compared with actual 2023 values.</em></p>
</div>  

- **MYR/EUR (ARIMA)**  
<div align="left">
  <img src="https://github.com/amsyarzulkifly/fyp-forecasting-exchange-rates/raw/main/attachment/myr_to_eur_arima.PNG" alt="MYR to EUR ARIMA" width="800" height="450"/>
  <p><em>Forecasted MYR/EUR exchange rate using ARIMA model compared with actual 2023 values.</em></p>
</div>  

- **MYR/GBP (ARIMA)**  
<div align="left">
  <img src="https://github.com/amsyarzulkifly/fyp-forecasting-exchange-rates/raw/main/attachment/myr_to_gbp_arima.PNG" alt="MYR to GBP ARIMA" width="800" height="450"/>
  <p><em>Forecasted MYR/GBP exchange rate using ARIMA model compared with actual 2023 values.</em></p>
</div>  

### 📈 DARIMA Forecasts  

- **MYR/USD (DARIMA)**  
<div align="left">
  <img src="https://github.com/amsyarzulkifly/fyp-forecasting-exchange-rates/raw/main/attachment/myr_to_usd_darima.PNG" alt="MYR to USD DARIMA" width="800" height="450"/>
  <p><em>Forecasted MYR/USD exchange rate using DARIMA model compared with actual 2023 values.</em></p>
</div>  

- **MYR/EUR (DARIMA)**  
<div align="left">
  <img src="https://github.com/amsyarzulkifly/fyp-forecasting-exchange-rates/raw/main/attachment/myr_to_eur_darima.PNG" alt="MYR to EUR DARIMA" width="800" height="450"/>
  <p><em>Forecasted MYR/EUR exchange rate using DARIMA model compared with actual 2023 values.</em></p>
</div>  

- **MYR/GBP (DARIMA)**  
<div align="left">
  <img src="https://github.com/amsyarzulkifly/fyp-forecasting-exchange-rates/raw/main/attachment/myr_to_gbp_darima.PNG" alt="MYR to GBP DARIMA" width="800" height="450"/>
  <p><em>Forecasted MYR/GBP exchange rate using DARIMA model compared with actual 2023 values.</em></p>
</div>  

---

## 📊 Model Performance  

### MYR/USD (2023)  
| Metric | ARIMA | DARIMA |  
|--------|-------|--------|  
| Mean Squared Error (MSE)        | 0.0024 | 0.0011 |  
| Root Mean Squared Error (RMSE)  | 0.0490 | 0.0331 |  
| Mean Absolute Error (MAE)       | 0.0457 | 0.0275 |  
| Mean Absolute Percentage Error (MAPE) | 0.9120 | 0.6810 |  

### MYR/EUR (2023)  
| Metric | ARIMA | DARIMA |  
|--------|-------|--------|  
| Mean Squared Error (MSE)        | 0.0022 | 0.0012 |  
| Root Mean Squared Error (RMSE)  | 0.0469 | 0.0346 |  
| Mean Absolute Error (MAE)       | 0.0325 | 0.0289 |  
| Mean Absolute Percentage Error (MAPE) | 0.8891 | 0.6842 |  

### MYR/GBP (2023)  
| Metric | ARIMA | DARIMA |  
|--------|-------|--------|  
| Mean Squared Error (MSE)        | 0.0023 | 0.0009 |  
| Root Mean Squared Error (RMSE)  | 0.0480 | 0.0301 |  
| Mean Absolute Error (MAE)       | 0.0454 | 0.0257 |  
| Mean Absolute Percentage Error (MAPE) | 0.9040 | 0.6795 |  

## 🚀 Conclusion  
This study demonstrates that both ARIMA and DARIMA models can forecast MYR exchange rates effectively, but DARIMA provides better accuracy for large and complex datasets. The findings contribute to a deeper understanding of time series forecasting in finance and can assist policymakers, investors, and financial analysts in decision-making and risk management.  

## 📌 Tools Used  
- **R Programming**  
- **Tidyverse & Tsibble** for data manipulation and visualization  
- **Forecast & Fable** packages for ARIMA and DARIMA modeling  
- **Zoo** package for handling missing values  
