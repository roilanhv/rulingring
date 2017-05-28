

# Ciclo anual da variável
getcycleAvgTS <- function(x, each8day) {
  viclima <- round(rep(c(t(unlist(lapply(split(t(x),each8day), mean, na.rm=T)))),
                       length(x)/max(each8day)), 2)
  data2fill <- viclima[is.na(x)]
  return(data2fill)
}

date.falt <- function(dates,start,end,by){
  require(chron)
  require(dplyr)
  # Determinando continuidade da série
  dates_seq <- seq.Date(from = as.Date(start, format = "%Y-%m-%d"),
                        to = as.Date(end, format = "%Y-%m-%d"),
                        by = by)
  splitdates <- split(dates, years(dates))
  # dates list sem datas faltantes com intervalo 
  # dos arquivos de LAI
  dates_l <- lapply(splitdates, function(x) { 
    merge(data.frame(date = seq(x[1], x[length(x)], by=by)),
          data.frame(date=x, difdate=c(4,diff(x))),
          all = T)
  } ) %>% bind_rows    
  # Datas faltantes??
  date_falt <- dates_l$date[!(dates_l$date %in% dates)]
  date_falt
  
}



# Função para escrever configuração de parámetros do TIMESAT
writeTSS <- function(dir = "~/Dissertação/MODIS/TIMESAT_in/",
                     set = "input_lai.txt",
                     file = "LAISat.txt",
                     range = c(0.0,7.0),
                     mask = "QCSat.txt",
                     qcvetor = c(0,0,0,0.5,0.5,0.5,1,1,1),
                     A = 0,
                     S = 2.0,
                     season = 0.5,
                     fit.steps = 3,
                     adapt = 2,
                     methods = c(1,1,1),
                     windw = c(4,5,6),
                     fseasampl = 30,
                     jobname="pauliceia"
){
  sink(paste0(dir,set))
  cat(file,"\n")           #  LAISat.txt
  cat(range,"\n")          #  0.0000     7.0000
  cat(mask,"\n")           #  NONE
  if(qcvetor != "") cat(qcvetor,"\n")        #  0,0,0,1,1,1,2,100,0,101,256,0
  cat(A,"\n")              #  0.0000
  cat(S,"\n")              #  2.0000
  cat(season,"\n")         #  0.5000
  cat(fit.steps,"\n")      #  3
  cat(adapt,"\n")          #  2.0000
  cat(methods,"\n")        #  1     1     1
  cat(windw,"\n")          #  4     5     6
  cat(fseasampl,"\n")      #  20.0000
  cat(jobname,"\n")        #  pauliceia
  sink()
}