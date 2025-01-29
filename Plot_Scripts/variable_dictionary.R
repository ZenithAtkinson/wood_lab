# Load necessary libraries
library(dplyr)
library(stringr)

# Loa
existing_data_file = './Data_Files/CSV_files/Cleaned Great Salt Lake Phragmites_Flux_AmeriFluxFormat.csv'
ameriflux_file = './Data_Files/CSV_files/flux_data_variables.csv'

# GIVEN claned data + ameriflux dictionary file
existing_data <- read.csv(existing_data_file, header = TRUE)
ameriflux_data <- read.csv(ameriflux_file, header = TRUE)

# get headers
existing_headers <- colnames(existing_data)

# create a dictionary from the AmeriFlux data where the first column is the variable name and the second is the description
# clean both the AmeriFlux and existing headers by removing spaces and converting to lowercase
clean_ameriflux_dict <- setNames(
  as.list(ameriflux_data[[3]]),
  str_trim(tolower(ameriflux_data[[2]]))
)

# Match the headers from the existing data with descriptions from AmeriFlux (case-insensitive and ignore spaces)
matching_headers_dict <- sapply(existing_headers, function(header) {
  cleaned_header <- str_trim(tolower(header))  # Clean the header name
  if (cleaned_header %in% names(clean_ameriflux_dict)) {
    clean_ameriflux_dict[[cleaned_header]]
  } else {
    "No description available"
  }
})

  # it is not working perfectly but works good enough for now.


View(matching_headers_dict)

colnames(existing_data)
#New, manually inputted dictionary:
ameriflux_dict <- list(
  "RECORD" = "Record number",
  "TIMESTAMP_START" = "Start of the measurement period. Units: YYYYMMDDHHMM",
  "TIMESTAMP_END" = "End of the measurement period. Units: YYYYMMDDHHMM",
  "CO2" = "Carbon dioxide mixing ratio. Units: µmolCO2 mol-1",
  "CO2_SIGMA" = "Standard deviation of carbon dioxide mole fraction in wet air. Units: µmolCO2 mol-1",
  "H2O" = "Water vapor mixing ratio. Units: mmolH2O mol-1",
  "H2O_SIGMA" = "Standard deviation of water vapor mole fraction. Units: mmolH2O mol-1",
  "FC" = "CO2 flux. Units: µmolCO2 m-2 s-1",
  "FC_SSITC_TEST" = "Results of the quality flagging for FC according to Foken et al 2004, based on a combination of Steady State and Integral Turbulence Characteristics tests by Foken and Wichura (1996) (i.e., 0, 1, 2). Nondimensional",
  "LE" = "Latent heat flux. Units: W m-2",
  "LE_SSITC_TEST" = "Results of the quality flagging for LE according to Foken et al 2004, based on a combination of Steady State and Integral Turbulence Characteristics tests by Foken and Wichura (1996) (i.e., 0, 1, 2). Nondimensional",
  "ET" = "Evapotranspiration",
  "ET_SSITC_TEST" = "Evapotranspiration, results of a test",
  "H" = "Sensible heat flux. Units: W m-2",
  "H_SSITC_TEST" = "Results of the quality flagging for H according to Foken et al 2004, based on a combination of Steady State and Integral Turbulence Characteristics tests by Foken and Wichura (1996) (i.e., 0, 1, 2). Nondimensional",
  "FETCH_MAX" = "Distance at which footprint contribution is maximum. Units: m",
  "FETCH_90" = "Distance at which cross-wind integrated footprint cumulative probability is 90%. Units: m",
  "FETCH_55" = "Distance at which cross-wind integrated footprint cumulative probability is 55%. Units: m",
  "FETCH_40" = "Distance at which cross-wind integrated footprint cumulative probability is 40%. Units: m",
  "WD" = "Wind direction. Units: degrees from north",
  "WS" = "Wind speed. Units: m s-1",
  "WS_MAX" = "maximum WS in the averaging period. Units: m s-1",
  "USTAR" = "Friction velocity. Units: m s-1",
  "ZL" = "Monin-Obukhov Stability parameter. Nondimensional",
  "TAU" = "Momentum flux. Units: kg m-1 s-2",
  "TAU_SSITC_TEST" = "Results of the quality flagging for TAU according to Foken et al 2004, based on a combination of Steady State and Integral Turbulence Characteristics tests by Foken and Wichura (1996) (i.e., 0, 1, 2). Nondimensional",
  "MO_LENGTH" = "Monin-Obukhov length. Units: m",
  "U" = "Uknown. Related to velocity?",
  "U_SIGMA" = "Standard deviation of velocity fluctuations (towards main-wind direction after coordinates rotation). Units: m s-1",
  "V" = "Uknown. Related to velocity?",
  "V_SIGMA" = "Standard deviation of lateral velocity fluctuations (cross main-wind direction after coordinates rotation). Units: m s-1",
  "W" = "Uknown. Related to velocity?",
  "W_SIGMA" = "Standard deviation of vertical velocity fluctuations. Units: m s-1",
  "PA" = "Atmospheric pressure. Units: kPa",
  "TA" = "Air temperature. Units: °C",
  "RH" = "Relative humidity. Units: %",
  "T_DP" = "Unkown",
  "VPD" = "Vapor Pressure Deficit. Units: hPa",
  "T_SONIC" = "Sonic temperature. Units: degree C",
  "T_SONIC_SIGMA" = "Standard deviation of sonic temperature. Units: degree C",
  "PBLH" = "Planetary boundary layer height. Units: m",
  "TS" = "Soil temperature. Units: deg C",
  "ALB" = "Albedo, range 0-100. %",
  "NETRAD" = "Net radiation. Units: W m-2",
  "SW_IN" = "Incoming shortwave radiation. Units: W m-2",
  "SW_OUT" = "Outgoing shortwave radiation. Units: W m-2",
  "LW_IN" = "Incoming longwave radiation. Units: W m-2",
  "LW_OUT" = "Outgoing longwave radiation. Units: W m-2",
  "P" = "Precipitation. Units: mm"
)
print(ameriflux_dict)
