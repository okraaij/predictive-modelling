# Stock price prediction
An implementation of several time-series forecasting methods (Exponential Smoothing, ARIMA and (G)ARCH models) in the context of stock price prediction.

## Overview

- This repository contains several of the implementations of stock price prediction models
- This project focused on forecasting the stock price of Dixons Carphone (FTSE: DC) for both the short (< 6 months) and long (1 > year) and exploring price driving factors for DC's stock price. 
- The [R implementation](/stock_price_prediction/stock_price_prediction.R) is provided for the following predictive models:
  - Several Exponential Smoothing models with various parameters and plotted predictions (Holt's Linear Trend, Exponential Trend and Damped Trend)
  - Several Autoregressive Integrated Moving Average (ARIMA) models with various orders and plotted predictions
  - Several (Generalized) Autoregressive Conditional Heteroskedasticity (G)(ARCH) models with plotted predictions
  - Bivariate Granger-causality tests for:
    - The UK Consumer Confidence Index (CCI)
    - The UK Consumer Price Inflation (CPI)
    - Google Trends ('dixons carphone' and 'dixons carphone share')
- The implementations cover all the typical procedures required in time-series forecasting; unit-root testing (using ADF and KPSS tests), differencing (where applicable), checking residuals for autocorrelation using the ACF and PACF plots etc.
- Pre-processing techniques are also applied where neccesary.
- The models can be evaluated by using several metrics, this project has used the AIC, AICc and BIC values.

## Data

### Stock price data sourced from [Yahoo Finance](https://finance.yahoo.com/quote/DC.L)
A total of four datasets related to DC’s stock price were taken from Yahoo Finance from October 2017:
1. past 1.5 years, weekly
2. past 3 years, weekly
3. past 3 years, monthly 
4. past 5 years, daily

### Potential price driving factors
- UK Consumer Confidence Index (CCI) (source: [OECD](https://data.oecd.org/leadind/consumer-confidence-index-cci.htm))
    - The CCI: “is based on households' plans for major purchases and their economic situation, both currently and their expectations for the immediate future. Opinions compared to a “normal” state are collected and the difference between positive and negative answers provides a qualitative index on economic conditions.” (OECD, 2017).
    - 5-year monthly data was obtained ranging from October 2012 to October 2017.

- UK Consumer Price Inflation (CPI) (source [Office for National Statistics](https://www.ons.gov.uk/economy/inflationandpriceindices/methodologies/consumerpriceinflationincludesall3indicescpihcpiandrpiqmi))
    - The CPI is “the rate at which the prices of goods and services bought by households rise and fall; it is estimated using consumer price indices” (ONS, 2017).
    - 5-year monthly data was obtained ranging from October 2012 to October 2017.

- [Google Trends](https://trends.google.com/trends/) 
    - Data is obtained from Google Trends to find the popularity of the terms ‘dixons carphone’ (dataset ‘3yearweekGT’) and ‘dixons carphone share’ (dataset ‘3yearweekGTshare’) in the UK over the past 3 years. Data was converted to monthly data.



