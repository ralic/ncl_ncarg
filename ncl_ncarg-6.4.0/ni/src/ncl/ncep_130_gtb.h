/*
 *
 * US National Weather Service - NCEP (WMC)
 * For Land Modeling and Land Data Assimilation
 *
 * Center: 7
 * Subcenter: 0
 * Parameter table version: 130
 */

TBLE2 ncep_130_params[] = {
{1, "Pressure", "Pa", "PRES"},
{2, "Pressure reduced to MSL", "Pa", "PRMSL"},
{3, "Pressure tendency", "Pa/s", "PTEND"},
{4, "Potential vorticity", "km^2/kg/s", "PVORT"},
{5, "ICAO Standard Atmosphere Reference Height", "M", "ICAHT"},
{6, "Geopotential", "m^2/s^2", "GP"},
{7, "Geopotential height", "gpm", "HGT"},
{8, "Geometric height", "m", "DIST"},
{9, "Std dev of height", "m", "HSTDV"},
{10, "Total ozone", "Dobson", "TOZONE"},
{11, "Temperature", "K", "TMP"},
{12, "Virtual temperature", "K", "VTMP"},
{13, "Potential temperature", "K", "POT"},
{14, "Pseudo-adiabatic potential temperature", "K", "EPOT"},
{15, "Maximum temperature", "K", "T_MAX"},
{16, "Minimum temperature", "K", "T_MIN"},
{17, "Dew point temperature", "K", "DPT"},
{18, "Dew point depression", "K", "DEPR"},
{19, "Lapse rate", "K/m", "LAPR"},
{20, "Visibility", "m", "VIS"},
{21, "Radar spectra (1)", "non-dim", "RDSP1"},
{22, "Radar spectra (2)", "non-dim", "RDSP2"},
{23, "Radar spectra (3)", "non-dim", "RDSP3"},
{24, "Parcel lifted index (to 500 hPa)", "K", "PLI"},
{25, "Temperature anomaly", "K", "TMP_A"},
{26, "Pressure anomaly", "Pa", "PRESA"},
{27, "Geopotential height anomaly", "gpm", "GPA"},
{28, "Wave spectra (1)", "non-dim", "WVSP1"},
{29, "Wave spectra (2)", "non-dim", "WVSP2"},
{30, "Wave spectra (3)", "non-dim", "WVSP3"},
{31, "Wind direction", "deg", "WDIR"},
{32, "Wind speed", "m/s", "WIND"},
{33, "u-component of wind", "m/s", "U_GRD"},
{34, "v-component of wind", "m/s", "V_GRD"},
{35, "Stream function", "m^2/s", "STRM"},
{36, "Velocity potential", "m^2/s", "V_POT"},
{37, "Montgomery stream function", "m^2/s^2", "MNTSF"},
{38, "Sigma coord. vertical velocity", "/s", "SGCVV"},
{39, "Pressure vertical velocity", "Pa/s", "V_VEL"},
{40, "Geometric vertical velocity", "m/s", "DZDT"},
{41, "Absolute vorticity", "/s", "ABS_V"},
{42, "Absolute divergence", "/s", "ABS_D"},
{43, "Relative vorticity", "/s", "REL_V"},
{44, "Relative divergence", "/s", "REL_D"},
{45, "Vertical u-component shear", "/s", "VUCSH"},
{46, "Vertical v-component shear", "/s", "VVCSH"},
{47, "Direction of current", "deg", "DIR_C"},
{48, "Speed of current", "m/s", "SP_C"},
{49, "u-component of current", "m/s", "UOGRD"},
{50, "v-component of current", "m/s", "VOGRD"},
{51, "Specific humidity", "kg/kg", "SPF_H"},
{52, "Relative humidity", "%", "R_H"},
{53, "Humidity mixing ratio", "kg/kg", "MIXR"},
{54, "Precipitable water", "kg/m^2", "P_WAT"},
{55, "Vapor pressure", "Pa", "VAPP"},
{56, "Saturation deficit", "Pa", "SAT_D"},
{57, "Evaporation", "kg/m^2", "EVP"},
{58, "Cloud Ice", "kg/m^2", "C_ICE"},
{59, "Precipitation rate", "kg/m^2/s", "PRATE"},
{60, "Thunderstorm probability", "%", "TSTM"},
{61, "Total precipitation", "kg/m^2", "A_PCP"},
{62, "Large scale precipitation", "kg/m^2", "NCPCP"},
{63, "Convective precipitation", "kg/m^2", "ACPCP"},
{64, "Snowfall rate water equivalent.", "kg/m^2/s", "SRWEQ"},
{65, "Water equivalent of accum. snow depth", "kg/m^2", "WEASD"},
{66, "Snow depth", "m", "SNO_D"},
{67, "Mixed layer depth", "m", "MIXHT"},
{68, "Transient thermocline depth", "m", "TTHDP"},
{69, "Main thermocline depth", "m", "MTHD"},
{70, "Main thermocline anomaly", "m", "MTH_A"},
{71, "Total cloud cover", "%", "T_CDC"},
{72, "Convective cloud cover", "%", "CDCON"},
{73, "Low level cloud cover", "%", "L_CDC"},
{74, "Mid level cloud cover", "%", "M_CDC"},
{75, "High level cloud cover", "%", "H_CDC"},
{76, "Cloud water", "kg/m^2", "C_WAT"},
{77, "Best lifted index (to 500 hPa)", "K", "BLI"},
{78, "Convective snow", "kg/m^2", "SNO_C"},
{79, "Large scale snow", "kg/m^2", "SNO_L"},
{80, "Water temperature", "K", "WTMP"},
{81, "Land-sea mask (land=1;sea=0)", "fraction", "LAND"},
{82, "Deviation of sea level from mean", "m", "DSL_M"},
{83, "Surface roughness", "m", "SFC_R"},
{84, "Albedo", "%", "ALBDO"},
{85, "Soil temperature", "K", "TSOIL"},
{86, "Soil moisture content", "kg/m^2", "SOIL_M"},
{87, "Vegetation", "%", "VEG"},
{88, "Salinity", "kg/kg", "SALTY"},
{89, "Density", "kg/m^3", "DEN"},
{90, "Water runoff", "kg/m^2", "WATR"},
{91, "Ice concentration (ice=1;no ice=0)", "fraction", "ICE_C"},
{92, "Ice thickness", "m", "ICETK"},
{93, "Direction of ice drift", "deg", "DICED"},
{94, "Speed of ice drift", "m/s", "SICED"},
{95, "u-component of ice drift", "m/s", "U_ICE"},
{96, "v-component of ice drift", "m/s", "V_ICE"},
{97, "Ice growth rate", "m/s", "ICE_G"},
{98, "Ice divergence", "/s", "ICE_D"},
{99, "Snow melt", "kg/m^2", "SNO_M"},
{100, "Sig height of wind waves and swell", "m", "HTSGW"},
{101, "Direction of wind waves", "deg", "WVDIR"},
{102, "Significant height of wind waves", "m", "WVHGT"},
{103, "Mean period of wind waves", "s", "WVPER"},
{104, "Direction of swell waves", "deg", "SWDIR"},
{105, "Significant height of swell waves", "m", "SWELL"},
{106, "Mean period of swell waves", "s", "SWPER"},
{107, "Primary wave direction", "deg", "DIRPW"},
{108, "Primary wave mean period", "s", "PERPW"},
{109, "Secondary wave direction", "deg", "DIRSW"},
{110, "Secondary wave mean period", "s", "PERSW"},
{111, "Net short wave radiation (surface)", "W/m^2", "NSWRS"},
{112, "Net long wave radiation(surface)", "W/m^2", "NLWRS"},
{113, "Net short wave radiation (top)", "W/m^2", "NSWRT"},
{114, "Net long wave radiation (top)", "W/m^2", "NLWRT"},
{115, "Long wave radiation", "W/m^2", "LWAVR"},
{116, "Short wave radiation", "W/m^2", "SWAVR"},
{117, "Global radiation", "W/m^2", "G_RAD"},
{118, "Brightness temperature", "K", "BRTMP"},
{119, "Radiance with respect to wave no.", "W/m/sr", "LWRAD"},
{120, "Radiance with respect ot wave len.", "W/m^3/sr", "SWRAD"},
{121, "Latent heat flux", "W/m^2", "LHTFL"},
{122, "Sensible heat flux", "W/m^2", "SHTFL"},
{123, "Boundary layer dissipation", "W/m^2", "BLYDP"},
{124, "Momentum flux, u component", "N/m^2", "U_FLX"},
{125, "Momentum flux, v component", "N/m^2", "V_FLX"},
{126, "Wind mixing energy", "J", "WMIXE"},
{127, "Image data", "none", "IMG_D"},
{144, "Volumetric soil moisture (frozen + liquid)", "fraction", "SOILW"},
{145, "Potential latent heat flux (potential evaporation)", "W/m^2", "PEVPR"},
{146, "Vegetation canopy temperature", "K", "VEGT"},
{147, "Bare soil surface skin temperature", "K", "BARET"},
{148, "Average surface skin temperature", "K", "AVSFT"},
{149, "Effective radiative skin temperature", "K", "RADT"},
{150, "Surface water storage", "kg/m^2", "SSTOR"},
{151, "Liquid soil moisture content (non-frozen)", "kg/m^2", "LSOIL"},
{152, "Open water evaporation (standing water)", "W/m^2", "EWATR"},
{153, "Number concentration for rain particles", "non-dim", "NCRAIN"},
{154, "Land Surface Precipitation Accumulation", "kg/m^2", "LSPA"},
{155, "Ground Heat Flux", "W/m^2", "GFLUX"},
{156, "Convective inhibition", "J/kg", "CIN"},
{157, "Convective available potential energy", "J/kg", "CAPE"},
{158, "Turbulent Kinetic Energy", "J/kg", "TKE"},
{159, "Maximum snow albedo", "%", "MXSALB"},
{160, "Liquid volumetric soil moisture (non-frozen)", "fraction", "SOILL"},
{161, "Frozen precipitation (e.g. snowfall)", "kg/m^2", "ASNOW"},
{162, "Liquid precipitation (rainfall)", "kg/m^2", "ARAIN"},
{163, "Groundwater recharge", "kg/m^2", "GWREC"},
{164, "Flood plain recharge", "kg/m^2", "QREC"},
{165, "Snow temperature, depth-avg", "K", "SNOWT"},
{166, "Visible beam downward solar flux", "W/m^2", "VBDSF"},
{167, "Visible diffuse downward solar flux", "W/m^2", "VDDSF"},
{168, "Near IR beam downward solar flux", "W/m^2", "NBDSF"},
{169, "Near IR diffuse downward solar flux", "W/m^2", "NDDSF"},
{170, "Snow-free albedo", "%", "SNFALB"},
{171, "Number of soil layers in root zone", "non-dim", "RLYRS"},
{172, "Momentum flux", "N/m^2", "MFLX"},
{176, "Latitude (-90 to +90)", "deg", "NLAT"},
{177, "East longitude (0-360)", "deg", "ELON"},
{178, "Field Capacity (soil moisture)", "fraction", "FLDCAP"},
{179, "Aerodynamic conductance", "m/s", "ACOND"},
{180, "Snow age", "s", "SNOAG"},
{181, "Canopy conductance", "m/s", "CCOND"},
{182, "Leaf area index (0-9)", "non-dim", "LAI"},
{183, "Roughness length for heat", "m", "SFCRH"},
{184, "Snow albedo (over snow cover area only)", "%", "SALBD"},
{187, "Normalized Difference Vegetation Index", "non-dim", "NDVI"},
{188, "Canopy drip", "kg/m^2", "DRIP"},
{189, "Visible, Black Sky Albedo", "%", "VBSALB"},
{190, "Visible, White Sky Albedo", "%", "VWSALB"},
{191, "Near IR, Black Sky Albedo", "%", "NBSALB"},
{192, "Near IR, White Sky Albedo", "%", "NWSALB"},
{193, "Freezing Rain", "kg/m^2", "FRZR"},
{194, "Frozen Rain", "kg/m^2", "FROZR"},
{195, "Total Snow", "kg/m^2", "TSNOW"},
{196, "Model Terrain Height", "m", "MTERH"},
{198, "Sublimation (evaporation from snow)", "W/m^2", "SBSNO"},
{199, "Direct evaporation from bare soil", "W/m^2", "EVBS"},
{200, "Canopy water evaporation", "W/m^2", "EVCW"},
{201, "Virtual Temperature Based Convective Inhibition", "J/kg", "VTCIN"},
{202, "Virtual Temperature Based Convective Available Potential Energy", "J/kg", "VTCAPE"},
{203, "Minimal stomatal resistance", "s/m", "RSMIN"},
{204, "Downward shortwave radiation flux", "W/m^2", "DSWRF"},
{205, "Downward longwave radiation flux", "W/m^2", "DLWRF"},
{206, "Vertically Integrated Liquid", "kg m^-2", "VIL"},
{207, "Moisture availability", "%", "MSTAV"},
{208, "Exchange coefficient", "(kg/m^3)(m/s)", "SFEXC"},
{210, "Transpiration", "W/m^2", "TRANS"},
{211, "Upward short wave radiation flux", "W/m^2", "USWRF"},
{212, "Upward long wave radiation flux", "W/m^2", "ULWRF"},
{219, "Wilting point", "fraction", "WILT"},
{220, "Field Capacity", "fraction", "FLDCP"},
{221, "Planetary boundary layer height", "m", "HPBL"},
{222, "Surface slope type", "Index", "SLTYP"},
{223, "Plant canopy surface water", "kg/m^2", "CNWAT"},
{224, "Soil type", "Index (0-9)", "SOTYP"},
{225, "Vegetation type", "Index (0-13)", "VGTYP"},
{226, "Blackadar's mixing length scale", "m", "BMIXL"},
{227, "Asymptotic mixing length scale", "m", "AMIXL"},
{228, "Potential evaporation", "kg/m^2", "PEVAP"},
{229, "Snow phase-change heat flux", "W/m^2", "SNOHF"},
{230, "Transpiration stress-onset (soil moisture)", "fraction", "SMREF"},
{231, "Direct evaporation cease (soil moisture)", "fraction", "SMDRY"},
{234, "Subsurface runoff (baseflow)", "kg/m^2", "BGRUN"},
{235, "Surface runoff (non-infiltrating)", "kg/m^2", "SSRUN"},
{238, "Snow cover", "%", "SNOWC"},
{239, "Snow temperature", "K", "SNOT"},
{240, "Soil porosity", "fraction", "POROS"},
{241, "Simulated brightness temperature for GOES11, channel 2", "K", "SBT112"},
{242, "Simulated brightness temperature for GOES11, channel 3", "K", "SBT113"},
{243, "Simulated brightness temperature for GOES11, channel 4", "K", "SBT114"},
{244, "Simulated brightness temperature for GOES11, channel 5", "K", "SBT115"},
{246, "Solar parameter in canopy conductance", "fraction", "RCS"},
{247, "Temperature parameter in canopy conductance", "fraction", "RCT"},
{248, "Humidity parameter in canopy conductance", "fraction", "RCQ"},
{249, "Soil moisture parameter in canopy conductance", "fraction", "RCSOL"},
{252, "Surface drag coefficient", "non-dim", "CD"},
{253, "Surface friction velocity", "m/s", "FRICV"},
{254, "Richardson number", "non-dim", "RI"}
};
