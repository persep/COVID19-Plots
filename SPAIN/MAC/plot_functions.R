

##############################
###   PLOTTING FUNCTIONS   ###
##############################


plot_variables_spa <- function(data_spa, data_INE_ccaa, ma_lag = 7,
                               thres = 0.00001, notes = TRUE) {
  
  # Filtering since cumulative cases are greater than (thres * 100)% of pop.
  filter_idx <- data_spa$cum_cases > thres * sum(data_INE_ccaa$Total_pop)
    
  # Figure of SIR data with deaths
  fig_SIR_spa <- plot_ly()
  
  # Actives
  fig_SIR_spa <- fig_SIR_spa %>%
    add_bars(x = data_spa$date[filter_idx],
             y = data_spa$actives[filter_idx], name = 'Active cases',
             marker = list(color = 'rgba(231, 208, 3, 0.8)',
                           line = list(color = 'rgba(33, 5, 75, 1)',
                                       width = 1.5)),
             offsetgroup = 1, yaxis = "y")
  
  # Recovered
  fig_SIR_spa <- fig_SIR_spa %>%
    add_bars(x = data_spa$date[filter_idx],
             y = data_spa$cum_recov[filter_idx],
             name = 'Recovered',
             marker = list(color = 'rgba(114, 178, 242, 0.8)',
                           line = list(color = 'rgba(33, 5, 75, 1)',
                                       width = 1.3)),
             offsetgroup = 2, yaxis = "y")
  
  # Deaths
  fig_SIR_spa <- fig_SIR_spa %>%
    add_bars(x = data_spa$date[filter_idx],
             y = data_spa$cum_deaths[filter_idx],
             name = 'Deaths',
             marker = list(color = 'rgba(232, 93, 95, 0.8)',
                           line = list(color = 'rgba(33, 5, 75, 1)',
                                       width = 1.5)),
             offsetgroup = 3, yaxis = "y")
  
  # Layout
  fig_SIR_spa <- fig_SIR_spa %>%
    layout(#yaxis2 = ay,
           plot_bgcolor = 'rgba(91, 91, 91, 0.25)',
           paper_bgcolor = 'rgba(219, 193, 245, 0.2)',
           title = list(text =
                          paste("Current SIR model in Spain (no predictive).",
                                 "Source: Datadista Github repository.",
                                 "Graphics by Javier Álvarez Liébana"),
                        font = list(family = "Agency FB", face = 2,
                                    size = 17, color = 'rgba(1, 1, 1, 1)'),
                        x = 0.05),
           xaxis = list(title = "Dates",
                        tickfont = list(family = "Agency FB", size = 14,
                                        color = 'rgba(7, 7, 7, 1)'),
                        tickangle = -45, nticks = 21, type = "date"),
           yaxis = list(title = "Scale of population ",
                        titlefont = list(family = "Agency FB", size = 16,
                                         color = 'rgba(7, 7, 7, 1)'),
                        side = "right"),
           legend = list(bgcolor = 'rgba(112, 104, 223, 0.2)',
                         bordercolor = 'rgba(2, 2, 2, 1)',
                         font = list(family = "Agency FB", size = 15),
                         x = 0.05, y = 0.25), barmode = 'group', bargap = 0.08,
           margin = list(b = 10, l = 20, r = 10))
  
  if (notes) {
    
    fig_SIR_spa <- fig_SIR_spa %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                    x = 0.01, y = 0.99, text = "NOTES:",
                    showarrow = FALSE, xref = 'paper', yref = 'paper',
                    font = list(color = 'rgba(8, 8, 8, 1)',
                                size = 17)) %>%
    add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                    x = 0.02, y = 0.9,
                    text = paste("Actives = cases (PCR) - recov (PCR)",
                                 "- pos. antibodies - deaths"),
                    showarrow = FALSE, xref = 'paper', yref = 'paper',
                    font = list(color = 'rgba(8, 8, 8, 1)',
                                size = 12))  %>%
    add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                    x = 0.02, y = 0.84,
                    text = "Recovered just confirmed by PCR",
                    showarrow = FALSE, xref = 'paper', yref = 'paper',
                    font = list(color = 'rgba(8, 8, 8, 1)',
                                size = 11))  %>%
    add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                    x = 0.02, y = 0.78,
                    text = "18/04: cases and pos. with antibod. data was modified",
                    showarrow = FALSE, xref = 'paper', yref = 'paper',
                    font = list(color = 'rgba(8, 8, 8, 1)',
                                size = 11)) 
    
  }
  
  # Figure of daily data
  fig_daily_spa <- plot_ly()
  fig_daily_spa <-
    fig_daily_spa %>%
    add_bars(x = data_spa$date[filter_idx],
             y = data_spa$daily_cases[filter_idx] +
               data_spa$daily_antibod[filter_idx],
             name = 'Daily pos. with antibod.',
             marker = list(color = 'rgba(1, 1, 1, 0.2)',
                           line = list(color = 'rgba(33, 5, 75, 1)',
                                       width = 1.5)),
             offsetgroup = 1, hoverinfo = 'text',
             text = data_spa$daily_antibod[filter_idx])
  fig_daily_spa <-
    fig_daily_spa %>%
    add_bars(x = data_spa$date[filter_idx],
             y = data_spa$daily_cases[filter_idx],
             name = 'Daily cases',
             marker = list(color = 'rgba(231, 208, 3, 0.7)',
                           line = list(color = 'rgba(33, 5, 75, 1)',
                                       width = 1.5)),
             offsetgroup = 1)
  
  fig_daily_spa <- 
    fig_daily_spa %>%
    add_bars(x = data_spa$date[filter_idx],
             y = data_spa$daily_recov[filter_idx],
             name = 'Daily recovered',
             marker = list(color = 'rgba(114, 178, 242, 0.8)',
                           line = list(color = 'rgba(33, 5, 75, 1)',
                                       width = 1.3)),
             offsetgroup = 2, yaxis = "y")
  
  fig_daily_spa <-
    fig_daily_spa %>%
    add_bars(x = data_spa$date[filter_idx],
             y = data_spa$daily_deaths[filter_idx],
             name = 'Daily deaths',
             marker = list(color = 'rgba(232, 93, 95, 0.8)',
                           line = list(color = 'rgba(33, 5, 75, 1)',
                                       width = 1.5)),
             offsetgroup = 3, yaxis = "y")
  
  # Layout
  fig_daily_spa <- fig_daily_spa %>%
    layout(plot_bgcolor = 'rgba(7, 7, 7, 0.25)',
           paper_bgcolor = 'rgba(219, 193, 245, 0.2)',
           title =
             list(text =
                    paste("Daily data: cases, deaths, positive with",
                          "antibodies and recovered. Source: Datadista.",
                          "Graphics by Javier Álvarez Liébana"),
                        font = list(family = "Agency FB", face = 2,
                                    size = 17, color = 'rgba(1, 1, 1, 1)'),
                        x = 0.05),
           xaxis = list(title = "Dates",
                        tickfont = list(family = "Agency FB", size = 14,
                                        color = 'rgba(7, 7, 7, 1)'),
                        tickangle = -45, nticks = 21, type = "date"),
           yaxis = list(title = "Amount of people",
                        titlefont = list(family = "Agency FB", size = 16,
                                         color = 'rgba(7, 7, 7, 1)'),
                        side = "right"),
           legend = list(bgcolor = 'rgba(112, 104, 223, 0.2)',
                         bordercolor = 'rgba(2, 2, 2, 1)',
                         font = list(family = "Agency FB", size = 15),
                         x = 0.8, y = 0.97), barmode = 'group', bargap = 0.08,
           margin = list(b = 10, l = 20, r = 10))
  
  if (notes) {
    
    fig_daily_spa <- fig_daily_spa  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.01, y = 0.99, text = "NOTES:",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 17)) %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.02, y = 0.9,
                      text = paste("Actives = cases (PCR) - recov (PCR)",
                                   "- pos. antibodies - deaths"),
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.02, y = 0.84,
                      text = "Recovered just confirmed by PCR",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 11))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.02, y = 0.78,
                      text = "18/04: cases and pos. with antibod. data was modified",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 11))
  }
  
  ##
  ## Smoothing data
  ##
  
  # Smoothing moving average: daily cases
  variable <- data_spa$daily_cases
  smooth_ma_cases <- rep(0, length(variable))
  for (i in ma_lag:length(variable)) {
    
    smooth_ma_cases[i] <-
      mean(variable[(i - ma_lag + 1):i][variable[(i - ma_lag + 1):i] >= 0])
    
  }
  
  # Smoothing moving average: daily deaths
  variable <- data_spa$daily_deaths
  smooth_ma_deaths <- rep(0, length(variable))
  for (i in ma_lag:length(variable)) {
    
    smooth_ma_deaths[i] <- mean(variable[(i - ma_lag + 1):i])
    
  }
  
  # Smoothing moving average: daily recovered
  variable <- data_spa$daily_recov
  smooth_ma_recov <- rep(0, length(variable))
  for (i in ma_lag:length(variable)) {
    
    smooth_ma_recov[i] <- mean(variable[(i - ma_lag + 1):i])
    
  }
  
  # Figure of smoothing data
  fig_smooth_daily_spa <- plot_ly()
  fig_smooth_daily_spa <-
    fig_smooth_daily_spa %>%
    add_bars(x = data_spa$date[filter_idx],
             y = smooth_ma_cases[filter_idx],
             name = paste0(ma_lag, "-days MA daily smooth cases"),
             marker = list(color = 'rgba(231, 208, 3, 0.7)',
                           line = list(color = 'rgba(33, 5, 75, 1)',
                                       width = 1.5)),
             offsetgroup = 1, yaxis = "y")
  
  fig_smooth_daily_spa <-
    fig_smooth_daily_spa %>%
    add_bars(x = data_spa$date[filter_idx],
             y = smooth_ma_recov[filter_idx],
             name = paste0(ma_lag, "-days MA daily smooth recovered"),
             marker = list(color = 'rgba(114, 178, 242, 0.8)',
                           line = list(color = 'rgba(33, 5, 75, 1)',
                                       width = 1.3)),
             offsetgroup = 2, yaxis = "y")
  
  fig_smooth_daily_spa <-
    fig_smooth_daily_spa %>%
    add_bars(x = data_spa$date[filter_idx],
             y = smooth_ma_deaths[filter_idx],
             name = paste0(ma_lag, "-days MA daily deaths"),
             marker = list(color = 'rgba(232, 93, 95, 0.8)',
                           line = list(color = 'rgba(33, 5, 75, 1)',
                                       width = 1.5)),
             offsetgroup = 3, yaxis = "y")
  
  # Layout
  fig_smooth_daily_spa <- fig_smooth_daily_spa %>%
    layout(plot_bgcolor = 'rgba(7, 7, 7, 0.2)',
           paper_bgcolor = 'rgba(219, 193, 245, 0.25)',
           title =
             list(text =
                    paste("Daily smooth data: cases, deaths and recovered.",
                          "Source: Datadista. Graphics by Javier Álvarez Liébana"),
                  font = list(family = "Agency FB", face = 2,
                              size = 17, color = 'rgba(1, 1, 1, 1)'),
                  x = 0.05),
           xaxis = list(title = "Dates",
                        tickfont = list(family = "Agency FB", size = 14,
                                        color = 'rgba(7, 7, 7, 1)'),
                        tickangle = -45, nticks = 21, type = "date"),
           yaxis = list(title = "Amount of people",
                        titlefont = list(family = "Agency FB", size = 16,
                                         color = 'rgba(7, 7, 7, 1)'),
                        side = "right"),
           legend = list(bgcolor = 'rgba(112, 104, 223, 0.2)',
                         bordercolor = 'rgba(2, 2, 2, 1)',
                         font = list(family = "Agency FB", size = 15),
                         x = 0.7, y = 0.97), barmode = 'group', bargap = 0.08,
           margin = list(b = 10, l = 20, r = 10))
  
  if (notes) {
    
    fig_smooth_daily_spa <- fig_smooth_daily_spa %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.01, y = 0.99, text = "NOTES:",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 17)) %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.02, y = 0.9,
                      text = paste("Actives = cases (PCR) - recov (PCR)",
                                   "- pos. antibodies - deaths"),
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.02, y = 0.84,
                      text = "Recovered just confirmed by PCR",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 11))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.02, y = 0.78,
                      text = "18/04: cases and pos. with antibod. data was modified",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 11))
  }
  
  
  # Figure of fatality and mortality data
  ay <- list(overlaying = "y", side = "left",
             title = list(text = "Fatality rate",
                          font = list(family = "Agency FB", size = 16,
                                      color = 'rgba(7, 7, 7, 1)')))
  fig_fat_mort_spa <- plot_ly()
  fig_fat_mort_spa <-
    fig_fat_mort_spa %>%
    add_trace(x = data_spa$date[filter_idx],
              y = data_spa$fatality[filter_idx],
              name = "Fatality rate",
              type = "scatter", mode = "markers+lines",
              line = list(color = 'rgba(110, 5, 152, 1)'),
              marker = list(color = 'rgba(7, 139, 176, 0.8)',
                            size = 9,
                            line = list(color = 'rgba(33, 5, 75, 1)',
                                        width = 2)), yaxis = "y")
  fig_fat_mort_spa <-
    fig_fat_mort_spa %>%
    add_trace(x = data_spa$date[filter_idx],
              y = data_spa$mortality[filter_idx],
              name = "Mortality rate",
              type = "scatter", mode = "markers+lines",
              line = list(color = 'rgba(47, 152, 5, 1)'),
              marker = list(color = 'rgba(232, 93, 95, 0.8)',
                            size = 9,
                            line = list(color = 'rgba(33, 5, 75, 1)',
                                        width = 2)), yaxis = "y2")
  
  
  # Layout
  fig_fat_mort_spa <-
    fig_fat_mort_spa %>%
    layout(yaxis2 = ay, plot_bgcolor = 'rgba(7, 7, 7, 0.2)',
           paper_bgcolor = 'rgba(219, 193, 245, 0.25)',
           title =
             list(text =
                    paste("Fatality and mortality rates.",
                          "Source: Datadista. Graphics by Javier Álvarez Liébana"),
                  font = list(family = "Agency FB", face = 2,
                              size = 17, color = 'rgba(1, 1, 1, 1)'),
                  x = 0.05),
           xaxis = list(title = "Dates",
                        tickfont = list(family = "Agency FB", size = 14,
                                        color = 'rgba(7, 7, 7, 1)'),
                        tickangle = -45, nticks = 21, type = "date"),
           yaxis = list(title = "Mortality rate",
                        titlefont = list(family = "Agency FB", size = 16,
                                         color = 'rgba(7, 7, 7, 1)'),
                        side = "right"),
           legend = list(bgcolor = 'rgba(112, 104, 223, 0.2)',
                         bordercolor = 'rgba(2, 2, 2, 1)',
                         font = list(family = "Agency FB", size = 15),
                         x = 0.85, y = 0.2), barmode = 'group', bargap = 0.08,
           margin = list(b = 10, l = 20, r = 10))
  
  if (notes) {
    
    fig_fat_mort_spa <- fig_fat_mort_spa %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.01, y = 0.99, text = "NOTES:",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 17)) %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.02, y = 0.9,
                      text = paste("Actives = cases (PCR) - recov (PCR)",
                                   "- pos. antibodies - deaths"),
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.02, y = 0.84,
                      text = "Recovered just confirmed by PCR",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 11))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.02, y = 0.78,
                      text = "18/04: cases and pos. with antibod. data was modified",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 11))
  }
  
  # Figure of % growths
  fig_vel_spa <-
    plot_ly(x = data_spa$date[filter_idx], y = data_spa$vel_recov[filter_idx],
            name = 'Daily growth recovered',  type = "scatter",
            mode = "lines", fill = 'tozeroy',
            fillcolor = 'rgba(114, 178, 242, 0.7)',
            line = list(color = 'rgba(13, 76, 139, 1)'))
  fig_vel_spa <-
    fig_vel_spa %>%
    add_trace(x = data_spa$date[filter_idx],
              y = data_spa$vel_deaths[filter_idx],
              name = 'Daily growth deaths', fill = 'tozeroy',
              fillcolor = 'rgba(232, 93, 95, 0.7)',
              line = list(color = 'rgba(123, 14, 14, 1)'))
  fig_vel_spa <-
    fig_vel_spa %>%
    add_trace(x = data_spa$date[filter_idx], y = data_spa$vel_cases[filter_idx],
              name = "Daily growth cases", type = "scatter", mode = "lines",
              fill = "tozeroy", fillcolor = 'rgba(231, 208, 3, 0.7)',
              line = list(color = 'rgba(109, 97, 7, 1)'))
  fig_vel_spa <-
    fig_vel_spa %>%
    add_trace(x = data_spa$date[filter_idx],
              y = data_spa$vel_actives[filter_idx],
              name = 'Daily growth actives', fill = 'tozeroy',
              fillcolor = 'rgba(89, 231, 134, 0.7)',
              line = list(color = 'rgba(8, 98, 37, 1)'))
  
  # Layout
  fig_vel_spa <- fig_vel_spa %>%
    layout(plot_bgcolor = 'rgba(7, 7, 7, 0.2)',
           paper_bgcolor = 'rgba(219, 193, 245, 0.25)',
           title =
             list(text =
                    paste("Daily growths: cases, deaths, actives and recovered.",
                          "Source: Datadista. Graphics by Javier Álvarez Liébana"),
                  font = list(family = "Agency FB", face = 2,
                              size = 17, color = 'rgba(1, 1, 1, 1)'),
                  x = 0.05),
           xaxis = list(title = "Dates",
                        tickfont = list(family = "Agency FB", size = 14,
                                        color = 'rgba(7, 7, 7, 1)'),
                        tickangle = -45, nticks = 21, type = "date"),
           yaxis = list(title = "% daily growths",
                        titlefont = list(family = "Agency FB", size = 16,
                                         color = 'rgba(7, 7, 7, 1)'),
                        side = "right"),
           legend = list(bgcolor = 'rgba(112, 104, 223, 0.2)',
                         bordercolor = 'rgba(2, 2, 2, 1)',
                         font = list(family = "Agency FB", size = 15),
                         x = 0.8, y = 0.97), barmode = 'group', bargap = 0.08,
           margin = list(b = 10, l = 20, r = 10))
  
  if (notes) {
    
    fig_vel_spa <- fig_vel_spa  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.01, y = 0.99, text = "NOTES:",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 17)) %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.02, y = 0.9,
                      text = paste("Actives = cases (PCR) - recov (PCR)",
                                   "- pos. antibodies - deaths"),
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 12))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.02, y = 0.84,
                      text = "Recovered just confirmed by PCR",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 11))  %>%
      add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
                      x = 0.02, y = 0.78,
                      text = "18/04: cases and pos. with antibod. data was modified",
                      showarrow = FALSE, xref = 'paper', yref = 'paper',
                      font = list(color = 'rgba(8, 8, 8, 1)',
                                  size = 11))
  }

  # Output
  return(list("fig_SIR_spa" = fig_SIR_spa, "fig_daily_spa" = fig_daily_spa,
              "fig_smooth_daily_spa" = fig_smooth_daily_spa,
              "fig_fat_mort_spa" = fig_fat_mort_spa,
              "fig_vel_spa" = fig_vel_spa))
  
}








#
# Plotting function
plot_variables_ccaa <-
  function(var_ccaa = c("cum_cases"), daily_smooth = TRUE, ma_lag = 7,
           data_INE_ccaa, regions = NULL,
           thres = 0.00003, each_hab = 1e3, mypalette = NULL, notes = TRUE) {
    
    var_available <-
      c("cum_cases", "cum_recov", "cum_deaths", "actives", "daily_cases",
        "daily_recov", "daily_deaths", "vel_cases", "vel_actives", "vel_recov",
        "vel_deaths", "fatality", "mortality")
    
    if(all(var_ccaa %in% var_available)) {
      
      message(paste("The following variables will be plotted:\n \t",
                    do.call(paste, c(as.list(var_ccaa), sep = " - "))))

    } else {
      
      stop("Some of variables in var_ccaa cannot be plotted. Please, check again")
      
    }
    
    # Which variable are asked?
    var_plot <- list()
    var_plot$cum_cases_ccaa <- TRUE # Mandatory
    var_plot$cum_recov_ccaa <- ifelse("cum_recov" %in% var_ccaa, TRUE, FALSE)
    var_plot$cum_deaths_ccaa <- ifelse("cum_deaths" %in% var_ccaa, TRUE, FALSE)
    var_plot$actives_ccaa <- ifelse("actives" %in% var_ccaa, TRUE, FALSE)
    var_plot$daily_cases_ccaa <- ifelse("daily_cases" %in% var_ccaa,
                                        TRUE, FALSE)
    var_plot$daily_recov_ccaa <- ifelse("daily_recov" %in% var_ccaa,
                                        TRUE, FALSE)
    var_plot$daily_deaths_ccaa <- ifelse("daily_deaths" %in% var_ccaa,
                                         TRUE, FALSE)
    var_plot$vel_actives_ccaa <- ifelse("vel_actives" %in% var_ccaa,
                                        TRUE, FALSE)
    var_plot$vel_cases_ccaa <- ifelse("vel_cases" %in% var_ccaa, TRUE, FALSE)
    var_plot$vel_recov_ccaa <- ifelse("vel_recov" %in% var_ccaa, TRUE, FALSE)
    var_plot$vel_deaths_ccaa <- ifelse("vel_deaths" %in% var_ccaa, TRUE, FALSE)
    var_plot$fatality_ccaa <- ifelse("fatality" %in% var_ccaa, TRUE, FALSE)
    var_plot$mortality_ccaa <- ifelse("mortality" %in% var_ccaa, TRUE, FALSE)

    # Data for each CCAA included in regions and asked variables
    data_ccaa <-
      filter_data_ccaa(data_INE_ccaa, cum_corrected = TRUE, regions = regions,
                       cum_cases_ccaa = var_plot$cum_cases_ccaa,
                       cum_antibod_ccaa = TRUE,
                       cum_deaths_ccaa = var_plot$cum_deaths_ccaa,
                       cum_recov_ccaa = var_plot$cum_recov_ccaa,
                       admin_ccaa = FALSE, ICU_ccaa = FALSE,
                       daily_cases_ccaa = var_plot$daily_cases_ccaa,
                       daily_antibod_ccaa = FALSE,
                       daily_deaths_ccaa = var_plot$daily_deaths_ccaa,
                       daily_recov_ccaa = var_plot$daily_recov_ccaa,
                       actives_ccaa = var_plot$actives_ccaa, susc_ccaa = FALSE,
                       vel_cases_ccaa = var_plot$vel_cases_ccaa,
                       vel_antibod_ccaa = FALSE,
                       vel_deaths_ccaa = var_plot$vel_deaths_ccaa,
                       vel_recov_ccaa = var_plot$vel_recov_ccaa,
                       vel_admin_ccaa = FALSE, vel_ICU_ccaa = FALSE,
                       vel_actives_ccaa = var_plot$vel_actives_ccaa,
                       vel_susc_ccaa = FALSE,
                       fatality_ccaa = var_plot$fatality_ccaa,
                       mortality_ccaa = var_plot$mortality_ccaa)
    
    # Dates
    dates <- data_ccaa$out_var$cum_cases_ccaa$date
    
    # Palette of colors
    if (is.null(mypalette)) {
      
      mypalette <- c(brewer.pal(n = 8, name = "Dark2"),
                     brewer.pal(n = 11, name = "Paired"))
      
    }
    
    # Plotting each of the asked variables
    fig <- list()
    cont_fig <- 0
    for (v in 1:length(var_plot)) {
      if (var_plot[[v]]) {
        
        cont_fig <- cont_fig + 1
        
        variable <-
          data_ccaa$out_var[names(var_plot)[v] == names(data_ccaa$out_var)][[1]]
        variable_spa <-
          data_ccaa$out_var$total_data_spa[
            names(var_plot)[v] == paste0(names(data_ccaa$out_var$total_data_spa),
                                         "_ccaa")][[1]]
        
        if(startsWith(names(var_plot)[v], "daily") & daily_smooth) {
          
          smooth_ma <- rep(0, length(variable_spa))
          for (j in ma_lag:length(variable_spa)) {
            
            smooth_ma[j] <-
              mean(variable_spa[(j - ma_lag + 1):j][
                variable_spa[(j - ma_lag + 1):j] >= 0])
            
          }
          variable_spa <- smooth_ma
        }
        
        fig[[cont_fig]] <- plot_ly()
        min_date <- as.Date("2100-01-01")
        len_max <- 0
        for(i in 2:length(names(data_ccaa$out_var$cum_cases_ccaa))) {
      
          # Smoothing moving averate if daily data 
          if(startsWith(names(var_plot)[v], "daily") & daily_smooth) {
            
            smooth_ma <- rep(0, length(variable[, i]))
            for (j in ma_lag:length(variable[, i])) {
              
              smooth_ma[j] <-
                mean(variable[(j - ma_lag + 1):j, i][
                  variable[(j - ma_lag + 1):j, i] >= 0])
              
            }
            variable[, i] <- smooth_ma
          } 
          
          # Data to be plotted: since cumulative cases are greater than
          # (threshold * 100)% of pop of each region
          pop_reg <-
            data_INE_ccaa$Total_pop[
              gsub(" ", ".", data_INE_ccaa$name) ==
                names(data_ccaa$out_var$cum_cases_ccaa)[i]]
          filter_data <-
            data_ccaa$out_var$cum_cases_ccaa[, i] > thres * pop_reg
          min_date <- min(min_date, dates[filter_data][1])
          len_max <- max(len_max, sum(filter_data))
          
          fig[[cont_fig]] <- fig[[cont_fig]] %>%
            add_lines(x = 0:(sum(filter_data) - 1),
                      y = variable[filter_data, i] /
                        ifelse(is.null(each_hab) |
                                 startsWith(names(var_plot)[v], "fatality") |
                                 startsWith(names(var_plot)[v], "mortality"), 1,
                               (pop_reg / each_hab)),
                      name = names(data_ccaa$out_var$cum_cases_ccaa)[i],
                      type = "scatter", mode = "lines",
                      line = list(color = mypalette[i], width = 3.5),
                      text = variable[filter_data, i],
                      hovertemplate = paste('<i>Date</i>: %{x}',
                                          '<b>%{text}</b>'))
          
          if (v == 1) {
            
            fig[[cont_fig]] <- fig[[cont_fig]] %>%
              add_trace(x = 0:(sum(filter_data) - 1),
                        y = (variable[filter_data, i] +
                               data_ccaa$out_var$cum_antibod_ccaa[filter_data,
                                                                  i]) /
                          ifelse(is.null(each_hab) |
                                   startsWith(names(var_plot)[v], "fatality") |
                                   startsWith(names(var_plot)[v], "mortality"), 1,
                                 (pop_reg / each_hab)),
                        name = paste(names(data_ccaa$out_var$cum_cases_ccaa)[i],
                                     "(PCR + antibodies)"),
                        type = 'scatter', mode = 'markers+lines',
                        marker = list(symbol = "circle-open-dot", size = 3,
                                      line = list(width = 1)),
                        line = list(color = mypalette[i], width = 1),
                        text = variable[filter_data, i] +
                          data_ccaa$out_var$cum_antibod_ccaa[filter_data, i],
                        hovertemplate = paste('<i>Date</i>: %{x}',
                                              '<b>%{text}</b>'))
          }
      }
      
      # Indexes for plotting total of Spain
      filter_global_data <-
        (length(variable_spa) - len_max + 1):length(variable_spa)
    
      # rowSums(cum_cases) > threshold * pop_spa
      fig[[cont_fig]] <- fig[[cont_fig]] %>%
        add_bars(x = 0:(len_max - 1), y = variable_spa[filter_global_data] /
                   ifelse(is.null(each_hab) |
                            startsWith(names(var_plot)[v], "fatality") |
                            startsWith(names(var_plot)[v], "mortality"), 1,
                          (sum(data_INE_ccaa$Total_pop) / each_hab)),
                 name = "Spain",
                 marker = list(color = 'rgba(7, 7, 7, 0.1)',
                               line = list(color = 'rgba(1, 1, 1, 0.5)',
                                           width = 1.7)),
                 text = variable_spa[filter_global_data],
                 hovertemplate = paste('<i>Date</i>: %{x}',
                                       '<b>%{text}</b>'))
      
      # Layout
      fig[[cont_fig]] <- fig[[cont_fig]] %>%
        layout(title =
                 paste0(names(var_plot)[v], ifelse(is.null(each_hab), " ",
                                         paste0(" ", " by each ",
                                                each_hab, " ")),
                        "people (aligned). ",
                        "Source: Datadista Github repository.\n",
                        "Graphics by J. Álvarez Liébana ",
                        "(", names(var_plot)[v], "/", each_hab,
                        "p for Spain in grey bars)"),
               xaxis = list(title = "Days of pandemic",
                            tickfont = list(family = "Agency FB", size = 14,
                                            color = 'rgba(7, 7, 7, 1)'),
                            tickangle = -45, nticks = 30),
               yaxis =
                 list(title =
                        paste("Data for each", each_hab, "people",
                              ifelse(startsWith(names(var_plot)[v], "daily") &
                                       daily_smooth,
                                     paste0(ma_lag, "-days MA smooth"), "")),
                            titlefont = list(family = "Agency FB", size = 16,
                                             color = 'rgba(7, 7, 7, 1)'),
                            side = "left"),
               
               legend = list(bgcolor = 'rgba(112, 104, 223, 0.2)',
                             bordercolor = 'rgba(2, 2, 2, 1)',
                             font = list(family = "Agency FB", size = 15),
                             x = 1, y = 1),
               margin = list(b = 10, l = 20, r = 10))
      
      }
    }
    # if (notes) {
    #   
    #   fig <- fig %>%
    #     add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
    #                     x = 0.005, y = 0.99, text = "NOTES:",
    #                     showarrow = FALSE, xref = 'paper', yref = 'paper',
    #                     font = list(color = 'rgba(8, 8, 8, 1)',
    #                                 size = 18))  %>%
    #     add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
    #                     x = 0.015, y = 0.9,
    #                     text = "09/03: closing schools in Madrid",
    #                     showarrow = FALSE, xref = 'paper', yref = 'paper',
    #                     font = list(color = 'rgba(8, 8, 8, 1)',
    #                                 size = 12))  %>%
    #     add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
    #                     x = 0.015, y = 0.85,
    #                     text = "14/03: lockdown",
    #                     showarrow = FALSE, xref = 'paper', yref = 'paper',
    #                     font = list(color = 'rgba(8, 8, 8, 1)',
    #                                 size = 12))  %>%
    #     add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
    #                     x = 0.015, y = 0.8,
    #                     text = "28/03: extreme lockdown: just essential workers",
    #                     showarrow = FALSE, xref = 'paper', yref = 'paper',
    #                     font = list(color = 'rgba(8, 8, 8, 1)',
    #                                 size = 12))   %>%
    #     add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
    #                     x = 0.015, y = 0.75,
    #                     text = paste0("07/04: admission and ICU from Madrid, ",
    #                                   "Galicia,\n C. Valenciana, Castilla-La Mancha",
    #                                   "and Castilla y León\n are prevalence data ",
    #                                   "since some dates"),
    #                     showarrow = FALSE, xref = 'paper', yref = 'paper',
    #                     font = list(color = 'rgba(8, 8, 8, 1)',
    #                                 size = 12)) %>%
    #     add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
    #                     x = 0.015, y = 0.595,
    #                     text = "13/04: previous lockdown again",
    #                     showarrow = FALSE, xref = 'paper', yref = 'paper',
    #                     font = list(color = 'rgba(8, 8, 8, 1)',
    #                                 size = 12))  %>%
    #     add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
    #                     x = 0.015, y = 0.505,
    #                     text = paste0("18/04: dataset with cum. pos.",
    #                                   " antibodies\n without symptoms at the",
    #                                   " time\n of the test, just for some CCAA"),
    #                     showarrow = FALSE, xref = 'paper', yref = 'paper',
    #                     font = list(color = 'rgba(8, 8, 8, 1)',
    #                                 size = 12))  %>%
    #     add_annotations(bgcolor = 'rgba(126, 235, 148, 0.25)',
    #                     x = 0.015, y = 0.375,
    #                     text = paste0("20/04: recovered from Galicia\n updated",
    #                                   "just in the recovered \n file: ",
    #                                   "before just discharges"),
    #                     showarrow = FALSE, xref = 'paper', yref = 'paper',
    #                     font = list(color = 'rgba(8, 8, 8, 1)',
    #                                 size = 12))
    # 
    # }
    
    # Rename figures according to the variables
    names(fig) <- names(var_plot)[var_plot == TRUE]
    
    # Output
    return(fig)
}