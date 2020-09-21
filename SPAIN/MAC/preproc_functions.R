
###################################
###   PREPROCESSING FUNCTIONS   ###
###################################



#
# Function to extract the census data from INE
#
census_spain <- function(name_csv = "Censuses2011_2.csv") {
  
  # Loading the file from INE
  data_census_spain <- read.csv(name_csv, stringsAsFactors = FALSE)  
  data_census_spain$municipality_code <-
    as.numeric(separate(data_census_spain, Municipality.of.residence,
                        "mun_code", " ")$mun_code) # Get codes  
  data_census_spain$People <- as.numeric(data_census_spain$People)  
  data_census_spain$Average.age <- as.numeric(data_census_spain$Average.age)
  data_census_spain$Municipality.of.residence <- NULL
  
  # Output
  return(data_census_spain)
}




#
# Function to obtain population for each region (CCAA) included in names,
# ICU places, ICU places for each 1e6 people, total population and population
# by sex. If save_local = TRUE, files are stored
#
data_INE_ccaa <-
  function(cod_INE = 1:19,
           names = c("Andalucía", "Aragón", "Asturias", "Baleares",
                     "Canarias", "Cantabria", "Castilla y León",
                     "Castilla La Mancha", "Cataluña", "C. Valenciana",
                     "Extremadura", "Galicia", "Madrid", "Murcia",
                     "Navarra", "País Vasco", "La Rioja", "Ceuta",
                     "Melilla"),
           url_ICU_places_ccaa =
             paste0("https://raw.githubusercontent.com/datadista/datasets/",
                    "master/COVID%2019/ccaa_camas_uci_2017.csv"),
           file_pop = "pop_ccaa_INE.csv",
           save_local = TRUE) {
    
    # Create a data.frame with the administrative name of each region
    data_INE_ccaa <-
      data.frame("cod_INE" = cod_INE, "name" = names) 
    
    # ICU places              
    raw_data <- read.csv(url_ICU_places_ccaa)
    data_INE_ccaa$ICU_places <- c(raw_data$Total[order(raw_data$cod_ine)], NA, NA)
    
    # Population: total, male, female
    raw_data <-
      read.csv(file = file_pop, check.names = FALSE, sep = "\t",
               fileEncoding = "UTF-8", stringsAsFactors = FALSE, dec = ",")
    data_INE_ccaa <-
      cbind(data_INE_ccaa,
            t(matrix(as.numeric(sub("\\.", "", sub("\\.", "",
                                                   raw_data$Total[-(1:3)]))),
                     nrow = 3))) # Remove first "." in the strings
    names(data_INE_ccaa)[4:6] <- c("Total_pop", "Male_pop", "Female_pop")
    
    # ICU places/1e6hab
    data_INE_ccaa$ICU_places_hab <- data_INE_ccaa$ICU_places /
      (data_INE_ccaa$Total_pop / 1e6)
    
    # Save in local
    if (save_local) {
      
      save(data_INE_ccaa, file = paste0("data_INE_ccaa.RData"))
      
    }
    
    # Output
    return(data_INE_ccaa)
}




#
# Function to obtain data for each CCAA from the whole historical data series
# provided by the ISCIII: they correct the daily data series of Spanish Health
# Ministery. Data from INE should be provided in data_INE_ccaa.
#
# The following raw data for each CCAA is provided:
# cum_cases_ccaa: cumulative cases (just confirmed by PCR)
# cum_antibod_ccaa: cumulative positive with antibodies (even without symptomps)
# cum_deaths_ccaa: cumulative deaths
# cum_recov_ccaa: cumulative recovered (not positive with antibodies)
# admin_ccaa: admissions data (ICU included)
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
# vel_admin_ccaa: % daily growth of admissions data (ICU included)
# vel_ICU_ccaa: % daily growth of ICU data
# fatality_ccaa: fatality rates = cum_deaths / (cum_cases + antibodies)
# mortality_ccaa: mortality rates cum deaths / population
#
data_isciii_ccaa <-
  function(data_INE_ccaa, url_isciii_data =
             paste0("https://raw.githubusercontent.com/datadista/datasets/",
                    "master/COVID%2019/ccaa_covid19_datos_isciii.csv"),
           save_local = TRUE) {
    
    # ISCIII data: Spanish Health Ministery is correcting here the past data
    raw_isciii_data <- read.csv(url_isciii_data)

    if (save_local) {
      
      save(raw_isciii_data, file = paste0("raw_isciii_data.RData"))
      
    }
    
    cum_cases_ccaa <- cum_antibod_ccaa <- cum_deaths_ccaa <- cum_recov_ccaa <-
      admin_ccaa <- ICU_ccaa <-
      data.frame("date" = as.Date(unique(raw_isciii_data$Fecha)))
    susc_ccaa <- daily_cases_ccaa <- daily_antibod_ccaa <-
      daily_deaths_ccaa <- daily_recov_ccaa <-
      data.frame("date" = as.Date(unique(raw_isciii_data$Fecha)))
    actives_ccaa <- data.frame("date" = as.Date(unique(raw_isciii_data$Fecha)))
    vel_cases_ccaa <- vel_antibod_ccaa <- vel_actives_ccaa <- vel_deaths_ccaa <-
      vel_recov_ccaa <- vel_susc_ccaa <- vel_admin_ccaa <- vel_ICU_ccaa <- 
      data.frame("date" = as.Date(unique(raw_isciii_data$Fecha)))
    for (i in 1:19) {
      
      ##
      ## CUMULATIVE
      ##
      
      # Cumulative cases
      aux <- rep(0, length(raw_isciii_data$Fecha))
      for (j in 1:length(raw_isciii_data$Fecha)) {
        
        aux[j] <- ifelse(is.na(raw_isciii_data$PCR.[j]),
                         raw_isciii_data$Casos[j], raw_isciii_data$PCR.[j])
        
      }
      cum_cases_ccaa <- cbind(cum_cases_ccaa,
                              aux[raw_isciii_data$cod_ine == i])
      cum_cases_ccaa <- replace(cum_cases_ccaa, is.na(cum_cases_ccaa), 0)
      names(cum_cases_ccaa)[length(names(cum_cases_ccaa))] <-
        as.character(unique(raw_isciii_data$CCAA[raw_isciii_data$cod_ine == i]))

      # Cumulative positives with antibodies
      cum_antibod_ccaa <-
        cbind(cum_antibod_ccaa,
              raw_isciii_data$TestAc.[raw_isciii_data$cod_ine == i])
      cum_antibod_ccaa <- replace(cum_antibod_ccaa, is.na(cum_antibod_ccaa), 0)

      # Cumulative deaths
      cum_deaths_ccaa <-
        cbind(cum_deaths_ccaa,
              raw_isciii_data$Fallecidos[raw_isciii_data$cod_ine == i])
      cum_deaths_ccaa <- replace(cum_deaths_ccaa, is.na(cum_deaths_ccaa), 0)
      
      # Cumulative recovered
      cum_recov_ccaa <-
        cbind(cum_recov_ccaa,
              raw_isciii_data$Recuperados[raw_isciii_data$cod_ine == i])
      cum_recov_ccaa <- replace(cum_recov_ccaa, is.na(cum_recov_ccaa), 0)
      
      # Admissions (included ICU) data
      admin_ccaa <-
        cbind(admin_ccaa,
              raw_isciii_data$Hospitalizados[raw_isciii_data$cod_ine == i])
      admin_ccaa <- replace(admin_ccaa, is.na(admin_ccaa), 0)
      
      # ICU data
      ICU_ccaa <-
        cbind(ICU_ccaa,
              raw_isciii_data$UCI[raw_isciii_data$cod_ine == i])
      ICU_ccaa <- replace(ICU_ccaa, is.na(ICU_ccaa), 0)
      
      
      ##
      ## DAILY
      ##
      
      # Daily cases
      daily_cases_ccaa <- cbind(daily_cases_ccaa,
                                c(0, diff(cum_cases_ccaa[, i + 1])))
      
      # Daily antibodies
      daily_antibod_ccaa <-
        cbind(daily_antibod_ccaa, c(0, diff(cum_antibod_ccaa[, i + 1])))

      # Daily deaths
      daily_deaths_ccaa <-
        cbind(daily_deaths_ccaa, c(0, diff(cum_deaths_ccaa[, i + 1])))
      
      # Daily recovered
      daily_recov_ccaa <-
        cbind(daily_recov_ccaa, c(0, diff(cum_recov_ccaa[, i + 1])))
      
      # Active cases
      actives_ccaa <-
        cbind(actives_ccaa, cum_cases_ccaa[, i + 1] -
                raw_isciii_data$Fallecidos[raw_isciii_data$cod_ine == i] - 
                (raw_isciii_data$Recuperados[raw_isciii_data$cod_ine == i] +
                   cum_antibod_ccaa[, i + 1]))
      actives_ccaa <- replace(actives_ccaa, is.na(actives_ccaa), 0)
      
      # Daily susceptibles
      susc_ccaa <- cbind(susc_ccaa, data_INE_ccaa$Total_pop[i] -
                           (cum_recov_ccaa[, i + 1] +
                              cum_antibod_ccaa[, i + 1]) -
                           cum_deaths_ccaa[, i + 1] - actives_ccaa[, i + 1])
      
      ##
      ## % DAILY GROWTHS
      ##

      # % daily growth of cases
      aux <- c(0, cum_cases_ccaa[2:length(cum_cases_ccaa[, i + 1]), i + 1] /
                 cum_cases_ccaa[1:(length(cum_cases_ccaa[, i + 1]) - 1), i + 1]) - 1
      aux <- replace(aux, is.infinite(aux), 0)
      vel_cases_ccaa <-
        cbind(vel_cases_ccaa, aux)
      vel_cases_ccaa <- replace(vel_cases_ccaa, is.na(vel_cases_ccaa), 0)
      
      # % daily growth of antibodies
      aux <- c(0, cum_antibod_ccaa[2:length(cum_antibod_ccaa[, i + 1]), i + 1] /
                 cum_antibod_ccaa[1:(length(cum_antibod_ccaa[, i + 1]) - 1), i + 1]) - 1
      aux <- replace(aux, is.infinite(aux), 0)
      vel_antibod_ccaa <-
        cbind(vel_antibod_ccaa, aux)
      vel_antibod_ccaa <- replace(vel_antibod_ccaa, is.na(vel_antibod_ccaa), 0)
      
      # % daily growth of active cases
      aux <- c(0, actives_ccaa[2:length(actives_ccaa[, i + 1]), i + 1] /
                 actives_ccaa[1:(length(actives_ccaa[, i + 1]) - 1), i + 1]) - 1
      aux <- replace(aux, is.infinite(aux), 0)
      vel_actives_ccaa <-
        cbind(vel_actives_ccaa, aux)
      vel_actives_ccaa <- replace(vel_actives_ccaa, is.na(vel_actives_ccaa), 0)
      
      # % daily growth of deaths
      aux <- c(0, cum_deaths_ccaa[2:length(cum_deaths_ccaa[, i + 1]), i + 1] /
                 cum_deaths_ccaa[1:(length(cum_deaths_ccaa[, i + 1]) - 1), i + 1]) - 1
      aux <- replace(aux, is.infinite(aux), 0)
      vel_deaths_ccaa <-
        cbind(vel_deaths_ccaa, aux)
      vel_deaths_ccaa <- replace(vel_deaths_ccaa, is.na(vel_deaths_ccaa), 0)
      
      # % daily growth of recovered
      aux <- c(0, cum_recov_ccaa[2:length(cum_recov_ccaa[, i + 1]), i + 1] /
                 cum_recov_ccaa[1:(length(cum_recov_ccaa[, i + 1]) - 1), i + 1]) - 1
      aux <- replace(aux, is.infinite(aux), 0)
      vel_recov_ccaa <-
        cbind(vel_recov_ccaa, aux)
      vel_recov_ccaa <- replace(vel_recov_ccaa, is.na(vel_recov_ccaa), 0)
      
      # % daily growth of admin data (included ICU)
      aux <- c(0, admin_ccaa[2:length(admin_ccaa[, i + 1]), i + 1] /
                 admin_ccaa[1:(length(admin_ccaa[, i + 1]) - 1), i + 1]) - 1
      aux <- replace(aux, is.infinite(aux), 0)
      vel_admin_ccaa <- cbind(vel_admin_ccaa, aux)
      vel_admin_ccaa <- replace(vel_admin_ccaa, is.na(vel_admin_ccaa), 0)
      
      # % daily growth of ICU data
      aux <- c(0, ICU_ccaa[2:length(ICU_ccaa[, i + 1]), i + 1] /
                 ICU_ccaa[1:(length(ICU_ccaa[, i + 1]) - 1), i + 1]) - 1
      aux <- replace(aux, is.infinite(aux), 0)
      vel_ICU_ccaa <- cbind(vel_ICU_ccaa, aux)
      vel_ICU_ccaa <- replace(vel_ICU_ccaa, is.na(vel_ICU_ccaa), 0)
      
      # % daily growth of susceptibles
      aux <- c(0, susc_ccaa[2:length(susc_ccaa[, i + 1]), i + 1] /
                 susc_ccaa[1:(length(susc_ccaa[, i + 1]) - 1), i + 1]) - 1
      aux <- replace(aux, is.infinite(aux), 0)
      vel_susc_ccaa <-
        cbind(vel_susc_ccaa, aux)
      vel_susc_ccaa <- replace(vel_susc_ccaa, is.na(vel_susc_ccaa), 0)
      
    }
    names(cum_antibod_ccaa) <- names(cum_deaths_ccaa) <-
      names(cum_recov_ccaa) <- names(admin_ccaa) <- names(ICU_ccaa) <-
      names(susc_ccaa) <- names(daily_cases_ccaa) <-
      names(daily_antibod_ccaa) <- names(daily_deaths_ccaa) <-
      names(daily_recov_ccaa) <- names(actives_ccaa) <- names(vel_cases_ccaa) <-
      names(vel_antibod_ccaa) <- names(vel_actives_ccaa) <-
      names(vel_deaths_ccaa) <- names(vel_recov_ccaa) <- names(vel_susc_ccaa) <-
      names(vel_admin_ccaa) <- names(vel_ICU_ccaa) <- names(cum_cases_ccaa)
    
    ##
    ## FATALITY VS MORTALITY
    ##
    
    # Fatality = cum_deaths / cum_cases
    fatality_ccaa <- mortality_ccaa <-
      data.frame("date" = as.Date(unique(cum_cases_ccaa$date)))
    # fatality_ccaa <- cbind(fatality_ccaa, cum_deaths_ccaa[, -1] /
    #                          (cum_cases_ccaa[, -1] + cum_antibod_ccaa[, -1]))
    # 
    # Mortality = cum deaths / population
    # mortality_ccaa <-
    #   cbind(mortality_ccaa, cum_deaths_ccaa[, -1] / data_INE_ccaa$Total)
    # 
    for (i in 1:19) {
      
      fatality_ccaa <- cbind(fatality_ccaa, cum_deaths_ccaa[, i + 1] /
                               (cum_cases_ccaa[, i + 1] +
                                  cum_antibod_ccaa[, i + 1]))
      
      mortality_ccaa <- cbind(mortality_ccaa, cum_deaths_ccaa[, i + 1] /
                                data_INE_ccaa$Total[i])
        
      fatality_ccaa[, i + 1] <-
        replace(fatality_ccaa[, i + 1], is.na(fatality_ccaa[, i + 1]) |
                  is.infinite(fatality_ccaa[, i + 1]), 0)
      mortality_ccaa[, i + 1] <-
        replace(mortality_ccaa[, i + 1], is.na(mortality_ccaa[, i + 1]) |
                  is.infinite(mortality_ccaa[, i + 1]), 0)
      
    }
    names(fatality_ccaa) <- names(mortality_ccaa) <- names(cum_cases_ccaa)
    
    # Output
    return(list("cum_cases_ccaa" = cum_cases_ccaa,
                "cum_antibod_ccaa" = cum_antibod_ccaa,
                "cum_deaths_ccaa" = cum_deaths_ccaa,
                "cum_recov_ccaa" = cum_recov_ccaa,
                "admin_ccaa" = admin_ccaa, "ICU_ccaa" = ICU_ccaa,
                "daily_cases_ccaa" = daily_cases_ccaa,
                "daily_antibod_ccaa" = daily_antibod_ccaa,
                "daily_deaths_ccaa" = daily_deaths_ccaa,
                "daily_recov_ccaa" = daily_recov_ccaa,
                "susc_ccaa" = susc_ccaa,
                "actives_ccaa" = actives_ccaa,
                "vel_actives_ccaa" = vel_actives_ccaa,
                "vel_cases_ccaa" = vel_cases_ccaa,
                "vel_antibod_ccaa" = vel_antibod_ccaa,
                "vel_deaths_ccaa" = vel_deaths_ccaa,
                "vel_recov_ccaa" = vel_recov_ccaa,
                "vel_susc_ccaa" = vel_susc_ccaa,
                "vel_admin_ccaa" = vel_admin_ccaa,
                "vel_ICU_ccaa" = vel_ICU_ccaa,
                "fatality_ccaa" = fatality_ccaa,
                "mortality_ccaa" = mortality_ccaa))
    
}





#
# Function to obtain the raw data for each CCAA from the Spanish Health
# Ministery. Note that this data is not consistent, and then, no statistical
# analysis or preprocessing will be done. The whole historical data series
# is provided by the ISCIII: they correct the daily data series
#
# The following raw data for each CCAA is provided:
# raw_cases_ccaa: cumulative cases (PCR + antibodies)
# raw_antibod_ccaa: cumulative positive with antibodies (even without symptomps)
# raw_pcr_ccaa: cumulative PCR (note that raw_antibod + raw_pcr is not raw_cases)
# raw_deaths_ccaa: cumulative deaths
# raw_recov_ccaa: cumulative recovered (not positive with antibodies)
# raw_admin_ccaa: admissions data (ICU included)
# raw_ICU_ccaa: ICU data
#
data_health_minis_ccaa  <-
  function(url_recov_data =
           paste0("https://raw.githubusercontent.com/datadista/datasets/master/",
                  "COVID%2019/ccaa_covid19_altas_long.csv"),
           url_deaths_data =
             paste0("https://raw.githubusercontent.com/datadista/datasets/",
                    "master/COVID%2019/ccaa_covid19_fallecidos_long.csv"),
           url_cases_data =
             paste0("https://raw.githubusercontent.com/datadista/datasets/",
                    "master/COVID%2019/ccaa_covid19_casos_long.csv"),
           url_antibod_data =
             paste0("https://raw.githubusercontent.com/datadista/datasets/",
                    "master/COVID%2019/ccaa_covid19_confirmados_test_long.csv"),
           url_pcr_data =
             paste0("https://raw.githubusercontent.com/datadista/datasets/",
                    "master/COVID%2019/ccaa_covid19_confirmados_pcr_long.csv"),
           url_admin_data =
             paste0("https://raw.githubusercontent.com/datadista/datasets/",
                    "master/COVID%2019/ccaa_covid19_hospitalizados_long.csv"),
           url_ICU_data = 
             paste0("https://raw.githubusercontent.com/datadista/datasets/",
                    "master/COVID%2019/ccaa_covid19_uci_long.csv"),
           save_local = TRUE) {
    
    #  Spanish Health Ministery daily published data
    raw_cases_ccaa <- read.csv(url_cases_data)
    raw_recov_ccaa <- read.csv(url_recov_data)
    raw_deaths_ccaa <- read.csv(url_deaths_data)
    raw_antibod_ccaa <- read.csv(url_antibod_data)
    raw_pcr_ccaa <- read.csv(url_pcr_data)
    raw_admin_ccaa <- read.csv(url_admin_data)
    raw_ICU_ccaa <- read.csv(url_ICU_data)
    
    if (save_local) {
      
      save(raw_cases_ccaa, file = paste0("raw_cases_ccaa.RData"))
      save(raw_recov_ccaa, file = paste0("raw_recov_ccaa.RData"))
      save(raw_deaths_ccaa, file = paste0("raw_deaths_ccaa.RData"))
      save(raw_antibod_ccaa, file = paste0("raw_antibod_ccaa.RData"))
      save(raw_pcr_ccaa, file = paste0("raw_pcr_ccaa.RData"))
      save(raw_admin_ccaa, file = paste0("raw_admin_ccaa.RData"))
      save(raw_ICU_ccaa, file = paste0("raw_ICU_ccaa.RData"))
      
    }
    
    # Output
    return(list("raw_cases_ccaa" = raw_cases_ccaa,
                "raw_antibod_ccaa" = raw_antibod_ccaa,
                "raw_pcr_ccaa" = raw_pcr_ccaa,
                "raw_deaths_ccaa" = raw_deaths_ccaa,
                "raw_recov_ccaa" = raw_recov_ccaa,
                "raw_admin_ccaa" = raw_admin_ccaa,
                "raw_ICU_ccaa" = raw_ICU_ccaa))
    
}





#
# Function to aggregate the data for each CCAA to obtain the whole summary of
# data in Spain. If cum_corrected = TRUE, cumulative data is corrected to have
# positive increments. This correction is perfomed with all cumulative variables
# except admissions and ICU variables: each CCAA provide different data, even
# the aggregate data is provided.
#
summary_data_spa <- function(data_ccaa, cum_corrected = TRUE) {
  
  # Cumulative cases (just PCR)
  total_data_spa <-
    data.frame("date" = as.Date(unique(data_ccaa$cum_cases_ccaa$date)),
               "cum_cases" = rowSums(data_ccaa$cum_cases_ccaa[, -1]))
  
  if (cum_corrected) {
    
    for (i in 1:length(total_data_spa$cum_cases)) {
      if (i > 1) {
        if(total_data_spa$cum_cases[i] < total_data_spa$cum_cases[i - 1]) {
    
          total_data_spa$cum_cases[i] <- total_data_spa$cum_cases[i - 1] + 
            round((total_data_spa$cum_cases[i + 1] -
                     total_data_spa$cum_cases[i - 1])/2)
    
        }
      }
    }
    
  }
  
  # Cumulative positive with antibodies
  total_data_spa$cum_antibod <- rowSums(data_ccaa$cum_antibod_ccaa[, -1])
  
  if (cum_corrected) {
    
    for (i in 1:length(total_data_spa$cum_antibod)) {
      if (i > 1) {
        if(total_data_spa$cum_antibod[i] < total_data_spa$cum_antibod[i - 1]) {
          
          total_data_spa$cum_antibod[i] <- total_data_spa$cum_antibod[i - 1] + 
            round((total_data_spa$cum_antibod[i + 1] -
                     total_data_spa$cum_antibod[i - 1])/2)
          
        }
      }
    }
  }
  
  # Cumulative deaths
  total_data_spa$cum_deaths <- rowSums(data_ccaa$cum_deaths_ccaa[, -1])
  
  if (cum_corrected) {
    
    for (i in 1:length(total_data_spa$cum_deaths)) {
      if (i > 1) {
        if(total_data_spa$cum_deaths[i] < total_data_spa$cum_deaths[i - 1]) {
          
          total_data_spa$cum_deaths[i] <- total_data_spa$cum_deaths[i - 1] + 
            round((total_data_spa$cum_deaths[i + 1] -
                     total_data_spa$cum_deaths[i - 1])/2)
          
        }
      }
    }
  }
  
  # Cumulative recovered
  total_data_spa$cum_recov <- rowSums(data_ccaa$cum_recov_ccaa[, -1])
  
  if (cum_corrected) {
    
    for (i in 1:length(total_data_spa$cum_recov)) {
      if (i > 1) {
        if(total_data_spa$cum_recov[i] < total_data_spa$cum_recov[i - 1]) {
          
          total_data_spa$cum_recov[i] <- total_data_spa$cum_recov[i - 1] + 
            round((total_data_spa$cum_recov[i + 1] -
                     total_data_spa$cum_recov[i - 1])/2)
          
        }
      }
    }
  }
  
  # Admin data
  total_data_spa$admin <- rowSums(data_ccaa$admin_ccaa[, -1])
  
  # ICU data
  total_data_spa$ICU <- rowSums(data_ccaa$ICU_ccaa[, -1])
  
  # Daily cases
  total_data_spa$daily_cases <- c(0, diff(total_data_spa$cum_cases))
  
  # Daily positive with antibodies
  total_data_spa$daily_antibod <- c(0, diff(total_data_spa$cum_antibod))

  # Daily deaths
  total_data_spa$daily_deaths <- c(0, diff(total_data_spa$cum_deaths))

  # Daily recovered
  total_data_spa$daily_recov <- c(0, diff(total_data_spa$cum_recov))

  # Active cases
  total_data_spa$actives <- total_data_spa$cum_cases -
    total_data_spa$cum_deaths - total_data_spa$cum_antibod -
    total_data_spa$cum_recov

  # Susceptibles
  total_data_spa$susc <- sum(data_INE_ccaa$Total_pop) -
    total_data_spa$cum_recov - total_data_spa$cum_antibod -
    total_data_spa$cum_deaths - total_data_spa$actives

  # % daily growth of cases
  aux <-
    c(0, total_data_spa$cum_cases[-1] /
        total_data_spa$cum_cases[1:(length(total_data_spa$cum_cases) - 1)]) - 1
  total_data_spa$vel_cases <- replace(aux, is.infinite(aux) | is.na(aux), 0)
  
  # % daily growth of positive with antibodies
  aux <-
    c(0, total_data_spa$cum_antibod[-1] /
        total_data_spa$cum_antibod[1:(length(total_data_spa$cum_antibod) - 1)]) - 1
  total_data_spa$vel_antibod <- replace(aux, is.infinite(aux) | is.na(aux), 0)
  
  # % daily growth of deaths
  aux <-
    c(0, total_data_spa$cum_deaths[2:length(total_data_spa$cum_deaths)] /
        total_data_spa$cum_deaths[1:(length(total_data_spa$cum_deaths) - 1)]) - 1
  total_data_spa$vel_deaths <- replace(aux, is.infinite(aux) | is.na(aux), 0)
  
  # % daily growth of recovered
  aux <-
    c(0, total_data_spa$cum_recov[2:length(total_data_spa$cum_recov)] /
        total_data_spa$cum_recov[1:(length(total_data_spa$cum_recov) - 1)]) - 1
  total_data_spa$vel_recov <- replace(aux, is.infinite(aux) | is.na(aux), 0)
  
  # % daily growth of admin data
  aux <-
    c(0, total_data_spa$admin[2:length(total_data_spa$admin)] /
        total_data_spa$admin[1:(length(total_data_spa$admin) - 1)]) - 1
  total_data_spa$vel_admin <- replace(aux, is.infinite(aux) | is.na(aux), 0)
  
  # % daily growth of ICU data
  aux <-
    c(0, total_data_spa$ICU[2:length(total_data_spa$ICU)] /
        total_data_spa$ICU[1:(length(total_data_spa$ICU) - 1)]) - 1
  total_data_spa$vel_ICU <- replace(aux, is.infinite(aux) | is.na(aux), 0)

  # % daily growth of actives
  aux <-
    c(0, total_data_spa$actives[2:length(total_data_spa$actives)] /
        total_data_spa$actives[1:(length(total_data_spa$actives) - 1)]) - 1
  total_data_spa$vel_actives <- replace(aux, is.infinite(aux) | is.na(aux), 0)
  
  # % daily growth of susceptibles
  aux <-
    c(0, total_data_spa$susc[2:length(total_data_spa$susc)] /
        total_data_spa$susc[1:(length(total_data_spa$susc) - 1)]) - 1
  total_data_spa$vel_susc <- replace(aux, is.infinite(aux) | is.na(aux), 0)

  # Fatality = cum_deaths / cum_cases
  total_data_spa$fatality <- total_data_spa$cum_deaths /
    (total_data_spa$cum_cases + total_data_spa$cum_antibod)
  
  # Mortality = cum deaths / population
  total_data_spa$mortality <- total_data_spa$cum_deaths /
    sum(data_INE_ccaa$Total)
  
  # Output
  return(total_data_spa)
}



#
# Function to filter the data for regions (CCAA) and variables: the whole
# summary of data in Spain is provided anyhow. If cum_corrected = TRUE,
# cumulative data is corrected to have positive increments. This correction is
# perfomed with all cumulative variables except admissions and ICU variables:
# each CCAA provide different data, albeit the aggregate data is provided.
# Data is obtained from the Corrected historial data series provided by ISCIII.
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
# vel_admin_ccaa: % daily growth of admissions (ICU included) data
# vel_ICU_ccaa: % daily growth of ICU data
# fatality_ccaa: fatality rates = cum_deaths / (cum_cases + antibodies)
# mortality_ccaa: mortality rates cum deaths / population
#
# Note also that the raw files from Spanish Health Ministery are also provided
# in data_health_minis_ccaa
#
filter_data_ccaa <-
  function(data_INE_ccaa, cum_corrected = TRUE, regions = NULL,
           cum_cases_ccaa = TRUE, cum_antibod_ccaa = TRUE,
           cum_deaths_ccaa = TRUE, cum_recov_ccaa = TRUE, admin_ccaa = TRUE,
           ICU_ccaa = TRUE, daily_cases_ccaa = TRUE,
           daily_antibod_ccaa = TRUE, daily_deaths_ccaa = TRUE,
           daily_recov_ccaa = TRUE,
           actives_ccaa = TRUE, susc_ccaa = TRUE,
           vel_cases_ccaa = TRUE, vel_antibod_ccaa = TRUE,
           vel_deaths_ccaa = TRUE, vel_recov_ccaa = TRUE,
           vel_admin_ccaa = TRUE, vel_ICU_ccaa = TRUE,
           vel_actives_ccaa = TRUE, vel_susc_ccaa = TRUE,
           fatality_ccaa = TRUE, mortality_ccaa = TRUE) {
    
    # Data for each CCAA corrected by ISCIII
    data_ccaa <- data_isciii_ccaa(data_INE_ccaa) 
    
    # Raw data by the Spanish Health Ministery
    data_health_minis_ccaa <- data_health_minis_ccaa()
    
    ##
    ## GLOBAL DATA OF SPAIN
    ##
    total_data_spa <- summary_data_spa(data_ccaa, cum_corrected = cum_corrected)
    
    out_var <- list("total_data_spa" = total_data_spa)
    if (cum_cases_ccaa) {
      out_var$cum_cases_ccaa <- data_ccaa$cum_cases_ccaa }
    if (daily_cases_ccaa) {
      out_var$daily_cases_ccaa <- data_ccaa$daily_cases_ccaa }
    if (vel_cases_ccaa) {
      out_var$vel_cases_ccaa <- data_ccaa$vel_cases_ccaa }
    
    if (cum_antibod_ccaa) {
      out_var$cum_antibod_ccaa <- data_ccaa$cum_antibod_ccaa }
    if (daily_antibod_ccaa) {
      out_var$daily_antibod_ccaa <- data_ccaa$daily_antibod_ccaa }
    if (vel_antibod_ccaa) {
      out_var$vel_antibod_ccaa <- data_ccaa$vel_antibod_ccaa }
    
    if (actives_ccaa) {
      out_var$actives_ccaa <- data_ccaa$actives_ccaa }
    if (vel_actives_ccaa) {
      out_var$vel_actives_ccaa <- data_ccaa$vel_actives_ccaa }
    
    if (cum_deaths_ccaa) {
      out_var$cum_deaths_ccaa <- data_ccaa$cum_deaths_ccaa }
    if (daily_deaths_ccaa) {
      out_var$daily_deaths_ccaa <- data_ccaa$daily_deaths_ccaa }
    if (vel_deaths_ccaa) {
      out_var$vel_deaths_ccaa <- data_ccaa$vel_deaths_ccaa }
    
    if (cum_recov_ccaa) {
      out_var$cum_recov_ccaa <- data_ccaa$cum_recov_ccaa }
    if (daily_recov_ccaa) {
      out_var$daily_recov_ccaa <- data_ccaa$daily_recov_ccaa }
    if (vel_recov_ccaa) {
      out_var$vel_recov_ccaa <- data_ccaa$vel_recov_ccaa }
    
    if (admin_ccaa) {
      out_var$admin_ccaa <- data_ccaa$admin_ccaa }
    if (vel_admin_ccaa) {
      out_var$vel_admin_ccaa <- data_ccaa$vel_admin_ccaa }
    
    if (ICU_ccaa) {
      out_var$ICU_ccaa <- data_ccaa$ICU_ccaa }
    if (vel_ICU_ccaa) {
      out_var$vel_ICU_ccaa <- data_ccaa$vel_ICU_ccaa }

    if (susc_ccaa) {
      out_var$susc_ccaa <- data_ccaa$susc_ccaa }
    if (vel_susc_ccaa) {
      out_var$vel_susc_ccaa <- data_ccaa$vel_susc_ccaa }

    if (fatality_ccaa) {
      out_var$fatality_ccaa <- data_ccaa$fatality_ccaa }
    if (mortality_ccaa) {
      out_var$mortality_ccaa <- data_ccaa$mortality_ccaa }
    
    # Filtering by regions: first check that names are correctly introduced
    if (is.null(regions)) { regions_c <- as.character(data_INE_ccaa$name) }
    else { regions_c <- regions[regions %in% data_INE_ccaa$name]}
    message(paste("Data is provided for the following regions:\n \t",
                    do.call(paste, c(as.list(regions_c), sep = " - "))))
    
    # If the number of correctly introduced names is not the same as the number
    # of regions, the user introduced some wrong region name
    if (!is.null(regions) & (length(regions_c) != length(regions))) {
      
      message(paste0("The following regions cannot be found, ",
                     "please check the names:\n \t",
                     do.call(paste, c(as.list(setdiff(regions, regions_c)),
                                      sep = " - "))))
    }
      
    # For each variable i, all regions are included in out_var[[i]][, -1]: we
    # just select the indexes whose regions names are included in the asked
    # regions (regions_c). The first element of the list is the global summary
    # of Spain
    for (i in 2:length(out_var)) {
      
      index_reg <- which(names(out_var[[i]][, -1]) %in% regions_c)
      out_var[[i]] <- data.frame("date" = out_var[[i]]$date,
                                 out_var[[i]][, index_reg + 1])
      
    }
      
    # Output
    return(list("out_var" = out_var,
                "raw_isciii_data_ccaa" = data_ccaa,
                "raw_health_minis_ccaa" = data_health_minis_ccaa))
  
  }
