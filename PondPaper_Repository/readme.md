# Water Temperature and Catchment Characteristics Drive Variance in Greenhouse Gas Emissions from Small Ponds in a Peatland-Rich, High-Altitude Tropical Ecosystem

### Keridwen M. Whitmore, Amanda Gay Delvecchia, Dani Zarate, Martina Bautista, Kayla Emerson, Amy Madrigal,  Esteban Suárez, and Diego A. Riveros-Iregui


This repository serves to host data and analyses used in the research supporting the work in *Water Temperature and Catchment Characteristics Drive Variance in Greenhouse Gas Emissions from Small Ponds in a Peatland-Rich, High-Altitude Tropical Ecosystem*

## Purpose
To provide access to the data and make analyses reproducible for others. All scripts and data files for creating our figures using R statistical software are provided within this repository.

## Guide to File Names 

### Rmarkdown

- File name: **Figures.Rmd**

This file contains all code needed to reproduce manuscript figures that do not contain data previously published data

### Data

- File name: **continuous_data**

| Column Name | Description |
| :--- | :---------- |
| DateTime | Date and Time of collection |
| Site_ID | site where data was collected  |
| AirPressure_kPa | Air pressure collected by a barometric logger located at the center of sites (kPa)  |
| AirTemp_c | Air temperature collected by a barometric logger located at the center of sites (Celsius)  |
| WaterTemp_c | Water temperature collected by water level logger at pond and wetland center (Celsius)  |
| WaterLevel_m | Water level collected by water level logger at pond and wetland center (meters)  |
| depth_ave_m | Average depth of pond (meters) |
| surface_area_m2 | Surface Area of pond (meters squared)  |
| Volumn_m3 | Volumn of pond [meters cubed] |

- File name: **GHGsampling_discrete_data**


| Column Name | Description |
| :--- | :---------- |
| DateTime | Date and Time of collection |
| Date | Date of collection |
| Site_ID | Site where data was collected  |
| DOC_mg.L | Dissolved organic  carbon concentration of pond collected 1x during study period (mg/l)|
| TDN_mg.L | Total Dissoved Nitrogen concentration of pond collected 1x during study period (mg/l)|
| Elevation_m | Elevation of site (meters) |
| percent_DTW | Percent of the watershed where DTW index is less than 1 (%)|
| AirPressure_kPa | Air pressure collected at the site (kPa)  |
| AirTemp_c | Air temperature collected at the site (C)  |
| WaterTemp_c | Water temperature collected by water level logger at pond and wetland center (C)  |
| depth_ave_m | Average depth of pond (m) |
| surface_area_m2 | Surface Area of pond (meters squared)  |
| Volumn_m3 | Volumn of pond [meters cubed] |
| SA_to_Vol_ratio | Surface area to volumn ratio |
| PrecipAccuDay_mm | Accumulation of precipitation on day of sampling (mm) |
| windspeed_m_s | Windspeed at time of sampling (m/s) |
| pCO2_ppm | Partial pressure of CO<sub>2</sub> at site (ppm) |
| CO2_umol.L | Concntration of CO<sub>2</sub> at site (umol/l) |
| CO2_sat_precent | Percent saturation of CO<sub>2</sub> at site (%) |
| pCH4_ppm | Partial pressure of CH<sub>4</sub> at site (ppm) |
| CH4_umol.L | Concntration of CH<sub>4</sub> at site (umol/l) |
| CH4_sat_precent | Percent saturation of CH<sub>4</sub> at site (%) |
| pCO2_air_atm | Partial pressure  of CO<sub>2</sub> in the air (ppm) |
| Flux_CO2_umol.m2.s | Flux  of CO<sub>2</sub> from the water to the atmosphere at site (umol/m2/s) |
| KH_CO2_mol.m3.atm | Henry's constant of CO<sub>2</sub> at site (mol/m3/atm) |
| KH_CH4_mol.L.atm| Henry's constant of CH<sub>4</sub> at site (mol/m3/atm) |
| k_CO2_m.d | Gas transfer velocity of CO<sub>2</sub> at site (meter/day) |
| k_ch4_m.d | Gas transfer velocity of CH<sub>4</sub> at site (meter/day) |
| K600 | Gas transfer velocity normalized to a Schmidt number of 600  (meter/day) |


- File name: **modeled_GHG_data**

| Column Name | Description |
| :--- | :---------- |
| Site_ID | site where data was collected  |
| DateTime | Date and Time of collection |
| WaterTemp_c | Water temperature collected by water level logger at pond and wetland center (Celsius)  |
| flux_mol.sec.m2_co2_fixed | Modeled flux  of CO<sub>2</sub> calculated using fixed temperature (mol/m2/sec) | 
| flux_mol.sec.m2_co2_fluctuate | Modeled flux  of CO<sub>2</sub> calculated using field measurements of temperature (mol/m2/sec) | 
| flux_mol.sec.m2_ch4_fixed | Modeled flux  of CH<sub>4</sub> calculated using fixed temperature (mol/m2/sec) | 
| flux_mol.sec.m2_ch4_fluctuate | Modeled flux  of CH<sub>4</sub> calculated using field measurements of temperature (mol/m2/sec) | 
| F_mol.s_CH4 | Modeled flux of CH<sub>4</sub> calculated using field measurements of temperature and surface area (mol/sec) | 
| F_mol.s_CH4_fixedSA | Modeled flux of CH<sub>4</sub> calculated using field measurements of temperature and fixed surface area (mol/sec) | 
| F_mol.s_CH4_fixedTemp | Modeled flux of CH<sub>4</sub> calculated using fixed temperature and field measurements of surface area (mol/sec) | 
| F_mol.s_CH4_fixedTempfixedSA | Modeled flux of CH<sub>4</sub> calculated using fixed measurements of temperature and surface area (mol/sec) | 
| F_mol.s_CO2 | Modeled flux of CO<sub>2</sub> calculated using field measurements of temperature and surface area (mol/sec) | 
| F_mol.s_CO2_fixedSA | Modeled flux of CO<sub>2</sub> calculated using field measurements of temperature and fixed surface area (mol/sec) | 
| F_mol.s_CO2_fixedTemp | Modeled flux of CO<sub>2</sub> calculated using fixed temperature and field measurements of surface area (mol/sec) | 
| F_mol.s_CO2_fixedTempfixedSA | Modeled flux of CO<sub>2</sub> calculated using fixed measurements of temperature and surface area (mol/sec) | 


- File name: **wetland_GHG_data**

| Column Name | Description |
| :--- | :---------- |
| Date | Date of collection |
| Lat | Latitude |
| Lon | Longitude |
| Site | Wetland site |
| Site2 | Waypoint were data was collected |
| Location | General description of where sample was collected  |
| AirPressure_kPa | Air pressure collected by a barometric logger located at the center of sites (kPa)  |
| WaterTemp_c | Water temperature collected by water level logger at pond and wetland center (C)  |
| pCO2_ppm | Partial pressure of CO<sub>2</sub> at site (ppm) |
| CO2_umol.L | Concntration of CO<sub>2</sub> at site (umol/l) |
| pCH4_ppm | Partial pressure of CH<sub>4</sub> at site (ppm) |
| CH4_umol.L | Concntration of CH<sub>4</sub> at site (umol/l) |


<strong>______________________________________________________________________</strong>
  
## Points of contact  

Direct **questions about the paper** to Dr. Diego Riveros-Iregui: <diegori@email.unc.edu> or
Keridwen Whitmore : <kriddie@email.unc.edu>

Direct **questions about the code** to:

Andrew Murray: <kriddie@email.unc.edu>