# Sensitivity of the SIMulation-EXtrapolation (SIMEX) Methodology to Mis-specification of the Statistical Properties of the Measurement Errors

<p align="justify">
  This repository contains the main codes from Villarini et al. 2023 - "Sensitivity of the SIMulation-EXtrapolation (SIMEX) Methodology to Mis-specification of the Statistical Properties of the Measurement Errors"
</p>

## The *R_Codes* folder

### 01_SIMEX_basic.R

<p align="justify">
  This code contains a simple application of the SIMEX method and generates the dataframe that we use to plot the Figure 1 in the draft:
</p>

<p align="center">
  <img src="https://user-images.githubusercontent.com/115516870/218445729-07b7a3cf-7f41-4d13-a9ca-a014dc3cff59.png" width=80% height=80% class="center">
</p>

<br>

### 02_SIMEX_SOI_Nino34_Application.R

<p align="justify">
  This code returns the output used in the application of SIMEX for the seasonal forecasting of the Southern Oscillation Index (SOI) in February as a function of sea   surface temperature (SST) forecasts in the Niño3.4 region (Figure 2 in the draft). The input data are stored in the <b>DATA</b> folder. The SOI data are downloaded from the National Oceanic and Atmospheric Administration (<a href="https://www.ncei.noaa.gov/access/monitoring/enso/soi">NOAA</a>), while the SST are computed by averaging the 12 members of the GFDL-CM2p5-FLOR-B01 by the Geophysical Fluid Dynamics Laboratory and part of the North American Multi-Model Ensemble (<a href="https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-B01/.MONTHLY/">NMME</a>)
</p>


<p align="center">
  <img src="https://user-images.githubusercontent.com/115516870/218471730-646f9103-789c-42a8-a34c-17e630e438d2.png" width=60% height=60% class="center">
</p>

<br>

### 03_SIMEX_sensitivity_parameters.R

<p align="justify">
  This code returns the output from the sensitivity analysis of the intercept and slope estimates, based on the OLS and SIMEX methodologies, to mis-specification of the mean and standard deviation of the measurement error U (Figure 3 - 6). 
</p>

<p align="center">
  <img src="https://user-images.githubusercontent.com/115516870/218481893-e71c4689-7e94-4118-88a6-36359755a1df.png" width=60% height=60% class="center">
</p>



### 04_SIMEX_sensitivity_distributions.R

<p align="justify">
  This code returns the output from the sensitivity analysis of SIMEX and Na\u00efve estimation of the slope as a function of sample size and for different error distributions (Figure 7). 
</p>

<p align="center">
  <img src="https://user-images.githubusercontent.com/115516870/218481805-9e81fe61-02ba-4d7e-bc83-f8608087f7df.png" width=60% height=60% class="center">
</p>

