# Remove all the variables
rm(list = ls())
assign("last.warning", NULL, envir = baseenv()) # Clean warnings

# DO NOT CHANGE: libraries required: Rcode that we will need
repos <- "http://cran.us.r-project.org"
if(!require(ggplot2)) install.packages("ggplot2", repos = repos)
if(!require(rgdal)) install.packages("rgdal", repos = repos,
                                     configure.args =
                                       c("--with-proj-lib=/usr/local/lib/",
                                         "--with-proj-include=/usr/local/include/"))
if(!require(sf)) install.packages("sf", repos = repos,
                                  configure.args =
                                    "--with-proj-lib=/usr/local/lib/")
if(!require(rnaturalearth)) install.packages("rnaturalearth", repos = repos)
if(!require(rnaturalearthdata)) install.packages("rnaturalearthdata",
                                                 repos = repos)
if(!require(geometry)) install.packages("geometry", repos = repos)
if(!require(maps)) install.packages("maps", repos = repos)
if(!require(rgeos)) install.packages("rgeos", repos = repos)
if(!require(plotly)) install.packages("plotly", repos = repos)
if(!require(stringr)) install.packages("stringr", repos = repos)
if(!require(tidyverse)) install.packages("tidyverse", repos = repos)

# Setting classic dark-on-light theme 
theme_set(theme_bw()) 

# The package rnaturalearth provides a map of countries of the entire world.
# Use ne_countries to pull country data and choose the scale
# (rnaturalearthhires is necessary for scale = "large").
# The function can return sp classes (default) or directly sf classes,
# as defined in the argument returnclass:
world <- ne_countries(scale = "medium",
                      returnclass = "sf")
class(world)

# Preprocess the mobility data
mobility_data <-
  read.csv(file = paste0("/Users/javieralvarezliebana/Downloads/",
                         "COVID19_MOBILITY_GOOGLE.csv"))
names_var <- names(mobility_data)[3:8] # Names of variables

# mob_data is a matrix with countries on rows and variables on columns
mob_data <- matrix(0, length(world$name_long), length(names_var))
for (i in 1:length(world$name_long)) {
  if (any(mobility_data$Country %in% world$name_long[i])) {
  
    for (j in 1:length(names_var)) {
      
      aux <- as.character(mobility_data[which(mobility_data$Country %in%
                                                world$name_long[i]), j + 2])
      mob_data[i, j] <- ifelse(j == 6, as.numeric(str_remove_all(aux, "%")),
                               as.numeric(str_remove_all(aux, "%")))
            
    }
  } else { mob_data[i, ] <- NA}
  
}

# Plotting the geographical data as a map for each mobility variable
for (j in 1:length(names_var)) {
  
  ggplot(data = world) +  geom_sf(aes(fill = mob_data[, j])) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle(paste("COVID-19 community mobility compared to baseline",
                  "(source = Google, graphics by Javier Álvarez Liébana)"),
            subtitle = paste0("variable: ", names_var[j],", baseline = median",
                              " during Jan 3–Feb 6, 2020 (",
                              length(unique(mobility_data$Country)),
                              " countries, ", "missing values in grey)")) +
    # scale_fill_distiller(palette = "viridis")
    scale_fill_viridis_c(option = "plasma", na.value = "grey40")  # trans = "sqrt")
}


#mob_data <- replace(mob_data, is.na(mob_data), 0)

install.packages("tidyverse")
library(stringr)
library(plotly)


mob_data <- replace(mob_data, is.na(mob_data), 0)
sort_data <- apply(mob_data, FUN = "order", MARGIN = 2)
sort_data[, 6] <- rev(sort_data[, 6])
max_values <- matrix(0, 7, length(names_var))
name_countries <- list()
for (i in 1:length(names_var)) {

  max_values[ , i]<- mob_data[sort_data[1:7, i], i]
  name_countries[[i]] <- world$name_long[sort_data[1:7, i]]
  
}

fig <- plot_ly()
fig <- fig %>%
  add_trace (x = ~names_var, y = ~max_values[1, ],
               type = 'bar', name = "1st",
               text =
                 lapply(name_countries,
                        FUN = function(data) { return(data[1]) }),
               textposition = 'auto')

fig <- fig %>%
  add_trace(x = ~names_var, y = ~max_values[2, ], name = "2nd",
            text =
              lapply(name_countries,
                     FUN = function(data) { return(data[2]) }),
            textposition = 'auto')
fig <- fig %>%
  add_trace(x = ~names_var, y = ~max_values[3, ], name = "3rd",
            text =
              lapply(name_countries,
                     FUN = function(data) { return(data[3]) }),
            textposition = 'auto')
fig <- fig %>%
  add_trace(x = ~names_var, y = ~max_values[4, ], name = "4th",
            text =
              lapply(name_countries,
                     FUN = function(data) { return(data[4]) }),
            textposition = 'auto')
fig <- fig %>%
  add_trace(x = ~names_var, y = ~max_values[5, ], name = "5th",
            text =
              lapply(name_countries,
                     FUN = function(data) { return(data[5]) }),
            textposition = 'auto')
fig <- fig %>%
  add_trace(x = ~names_var, y = ~max_values[6, ], name = "6th",
            text =
              lapply(name_countries,
                     FUN = function(data) { return(data[6]) }),
            textposition = 'auto')
fig <- fig %>%
  add_trace(x = ~names_var, y = ~max_values[7, ], name = "7th",
            text =
              lapply(name_countries,
                     FUN = function(data) { return(data[7]) }),
            textposition = 'auto')
fig <- fig %>%
  layout(title = paste("COVID-19 community mobility compared to baseline",
                       "(median) during Jan 3–Feb 6 2020\n",
                       "(source: Google, graphics by Javier Álvarez Liébana)"),
         xaxis = list(title = 'Mobility aspects'),
         yaxis = list(title = 'Mobility changes compared to baseline (median)'),
         barmode = 'group')
fig

