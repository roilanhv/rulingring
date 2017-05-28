plot_selected_dates <- function(hh_data, 
                                dts, 
                                vars = "NEE", 
                                window = 1){
  #dts =  dates_n_lt_26
  # hh_data <- nee_hh_r_qc
  # vars = c("NEE", "Rg", "PotRad")
  # window = 1
  
  dts_df <- data.frame(sdate = as.POSIXct(paste(dts - window, "00:00:00"), tz = "GMT"), 
                       date = as.POSIXct(paste(dts, "00:00:00"), tz = "GMT"), 
                       edate = as.POSIXct(paste(dts + window, "23:00:00"), tz = "GMT"))
  
  dts_dmy <- format(dts, "%d/%m/%Y")
  
  # looping para gráficos de cada data
  ll <- l_ply(seq(along.with = dts_dmy),
              function(i){
                #  i = 1
                tmp <- select(selectByDate(hh_data, 
                                           start = format(dts_df[i,"sdate"], "%d/%m/%Y"), 
                                           end = format(dts_df[i,"edate"], "%d/%m/%Y")), 
                              one_of(c("date", "daytime", vars)))
                
                if(nrow(tmp) > 1){
                  
                  sun_hours <- 
                  tmp %>% 
                    filter(daytime == "day") %>% 
                    select(date) %>% 
                    mutate(h = hour(date) + minute(date)/60,
                           d = as.Date(date)) %>%
                    group_by(d) %>%
                    summarise(hn = date[which.min(h)],
                              hx = date[which.max(h)]) %>%
                    select(-d)
                  
                  
                  xpos <- as.POSIXct(c(t(sun_hours)), tz = "GMT")
                  
                  tp <- timePlot(tmp, 
                                 vars, 
                                 lwd = 3,
                                 ref.y = list(h = 0, col = 5, lty = 3, lwd = 2),
                                 ref.x = list(v = xpos, col = 7, lty = 3, lwd = 3),
                                 main = dts_dmy[i],
                                 date.format = "%H:%M\n %b %d")
                  
                } else {
                  message("Dados com número de linhas menor ou igual a 1. i = ", dmy_nee_fr_lt0[i])
                }
              })
}

