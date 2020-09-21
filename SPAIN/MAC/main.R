
# DO NOT CHANGE: remove all variables and clean warnings
rm(list = ls())
assign("last.warning", NULL, envir = baseenv())

# DO NOT CHANGE: libraries required and source files that we will need
repos <- "http://cran.us.r-project.org"
if(!require(ggplot2)) install.packages("ggplot2", repos = repos)
if(!require(plotly)) install.packages("plotly", repos = repos)
if(!require(dplyr)) install.packages("dplyr", repos = repos)
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = repos)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source('./plot_functions.R')
source('./preproc_functions.R')


# DO NOT CHANGE: some demographic data (source: INE) for each municipality
data_INE_ccaa <- data_INE_ccaa()


###############################  
###   EXTRACTING THE DATA   ###
###############################


##
## PARAMETERS TO BE CHANGED: please, answer the following questions
##

# IMPORTANT: admissions and ICU data are just provided for each CCAA and
# aggregate for the whole Spain but not corrections neither graphics are
# performed since each CCAA is provided different data (prevalence vs incidence)

# Do you want that cumulative data is corrected to have positive increments?
cum_corrected <- TRUE # Just the whole aggregate Spanish data is corrected

# Which regions do you want to be extracted? If regions is NULL, all regions
# will be extracted. IMPORTANT: note that the filtering is just performed for
# the disaggregate data for each region (CCAA), the whole aggregate Spanish
# data is fully computed.
#
# Regions available: "Andalucía", "Aragón", "Asturias", "Baleares", "Canarias",
#                    "Cantabria", "Castilla y León", "Castilla La Mancha",
#                    "Cataluña", "C. Valenciana", "Extremadura", "Galicia",
#                    "Madrid", "Murcia", "Navarra", "País Vasco", "La Rioja",
#                    "Ceuta", "Melilla"
regions <- c("Andalucía", "Aragón", "Asturias", "Castilla La Mancha",
             "Castilla y León", "Cataluña", "Extremadura", "Madrid", "Murcia",
             "País Vasco", "La Rioja", "Canarias", "Baleares")

#
# The following raw data for each CCAA is available:
# cum_cases_ccaa: cumulative cases (just confirmed by PCR)
# cum_antibod_ccaa: cumulative positive with antibodies (even without symptomps)
# cum_deaths_ccaa: cumulative deaths
# cum_recov_ccaa: cumulative recovered (not positive with antibodies)
# admin_ccaa: admissions (ICU included) data
# ICU_ccaa: ICU data
# daily_cases_ccaa: daily cases (just PCR)
# daily_antibod_ccaa: daily positive with antibodies (even without symptomps)
# daily_deaths_ccaa: daily deaths
# daily_recov_ccaa: daily recovered (not positive with antibodies)
# susc_ccaa: daily susceptibles = pop - recov - antibodies - deaths - actives
# actives_ccaa: daily actives = cum_cases - deaths - recovered - antibodies
# vel_actives_ccaa: % daily growth of actives
# vel_cases_ccaa: % daily growth of cases (just PCR)
# vel_antibod_ccaa: % daily growth of positive with antibodies
# vel_deaths_ccaa: % daily growth of deaths
# vel_recov_ccaa: % daily growth of recovered
# vel_susc_ccaa: % daily growth of susceptibles
# vel_admin_ccaa: % daily growth of admissions (ICU included)
# vel_ICU_ccaa: % daily growth of ICU
# fatality_ccaa: fatality rates = cum_deaths / (cum_cases + antibodies)
# mortality_ccaa: mortality rates cum deaths / population
#
# Raw files raw_isciii_data_ccaa and raw_health_minis_ccaa are also provided.
# 


# Filtering data required (TRUE if you want to get it). Global data of Spain
# will be stored in data_isciii_ccaa$out_var$total_data_spa. Please, check
# (TRUE/FALSE) which variables you want to be filtered for each CCAA
data_ccaa <-
  filter_data_ccaa(data_INE_ccaa,
                   cum_corrected = cum_corrected, regions = regions,
                   cum_cases_ccaa = TRUE, cum_antibod_ccaa = TRUE,
                   cum_deaths_ccaa = TRUE, cum_recov_ccaa = TRUE,
                   admin_ccaa = TRUE, ICU_ccaa = TRUE, daily_cases_ccaa = TRUE,
                   daily_antibod_ccaa = TRUE, daily_deaths_ccaa = TRUE,
                   daily_recov_ccaa = TRUE,
                   actives_ccaa = TRUE, susc_ccaa = TRUE,
                   vel_cases_ccaa = TRUE, vel_antibod_ccaa = TRUE,
                   vel_deaths_ccaa = TRUE, vel_recov_ccaa = TRUE,
                   vel_admin_ccaa = TRUE, vel_ICU_ccaa = TRUE,
                   vel_actives_ccaa = TRUE, vel_susc_ccaa = TRUE,
                   fatality_ccaa = TRUE, mortality_ccaa = TRUE)



####################################  
###   PLOTTING THE GLOBAL DATA   ###
#################################### 


##
## PARAMETERS TO BE CHANGED: please, answer the following questions
##

# Which lag do you want to be considered for plotting smooth daily data? For
# example, if ma_lag = 7, 7-days moving average smooth daily data is plotted.
ma_lag <- 7

# Which threshold do you want to be considered? Data will be plotted since the
# day in which cumulative cases were greater than (thres * 100)% of population.
thres <- 0.00003 # 0.003% of population of each region

# Do you want to include some technical notes on the figures?
notes <- FALSE

# Plotting summary data of Spain. The following figures will be generated:
# fig_data_spa$fig_SIR_spa (actives, recovered and deaths),
# fig_data_spa$fig_daily_spa (daily new caes, recovered, deaths and positive
# with antibodies), fig_data_spa$fig_smooth_daily_spa (daily smooth data),
# fig_data_spa$fig_fat_mort_spa (fatality vs mortality rates) and
# fig_data_spa$fig_vel_spa (% daily growths of actives, cases, recovered and
# deaths)
fig_data_spa <-
  plot_variables_spa(data_ccaa$out_var$total_data_spa,
                     data_INE_ccaa, ma_lag = ma_lag, thres = thres,
                     notes = notes)



####################################  
###   PLOTTING THE GLOBAL DATA   ###
#################################### 


##
## PARAMETERS TO BE CHANGED: please, answer the following questions
##

# Which lag do you want to be considered for plotting smooth daily data? For
# example, if ma_lag = 7, 7-days moving average smooth daily data is plotted,
# as long as daily_smooth is TRUE
ma_lag <- 7
daily_smooth <- TRUE

# Which threshold do you want to be considered? Data will be plotted since the
# day in which cumulative cases were greater than (thres * 100)% of population,
# such that curves will be aligned to this Day 0 of pandemic for each region
thres <- 0.00003

# Do you want to compare curves for each CCAA by population? For example, if
# each_hab = 1e3, data will be plotted for each 1000 people.
each_hab <- 1e3

# Do you want to include some technical notes on the figures?
notes <- TRUE

# Which variables do you want to be compared?
#
# Variables available: "cum_cases", "cum_recov", "cum_deaths", "actives",
#                      "daily_cases", "daily_recov", "daily_deaths",
#                      "vel_cases", "vel_actives", "vel_recov", "vel_deaths",
#                      "fatality", "mortality"
#
var_ccaa <- c("cum_cases", "cum_recov", "cum_deaths", "daily_cases",
              "daily_deaths", "actives", "vel_cases",
              "fatality", "mortality")
fig_data_ccaa <-
  plot_variables_ccaa(var_ccaa = var_ccaa, daily_smooth = daily_smooth,
                      ma_lag = ma_lag, data_INE_ccaa, regions = regions,
                      thres = thres, each_hab = 1e3, mypalette = NULL,
                      notes = notes)

