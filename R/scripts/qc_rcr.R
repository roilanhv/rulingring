#########################################################################
# FUNCTION for rolling function with variable width
#########################################################################
wmapply <- function(x, width, fun,...){
  
  #stopifnot(require(plyr) & require(foreach) & require(doParallel))
  #workers <- makeCluster(n.cores)
  #registerDoParallel(workers)
    
  width <- rep(width, length(x))
  fun <- match.fun(fun)
  SEQ1 <- 1:length(x)
  #SEQ1[SEQ1 <  width] <- NA_integer_
  #SEQ2 <- lapply(SEQ1, function(i) if(!is.na(i)) (i - (width[i]-1)) : (i + (width[i]-1)) )
  #SEQ2 <- llply(SEQ1, .parallel = TRUE, .fun = 
  SEQ2 <- lapply(SEQ1, 
                 function(i) {
                   positions <- (i - (width[i]-1)) : (i + (width[i]-1)) 
                   positions[positions > 0]
                 }# end fun
  )# end lapply
  #OUT <- lapply(SEQ2, function(i) if(!is.null(i)) fun(x[i], ...) else NA_real_)
  OUT <- lapply(SEQ2, function(i) fun(x[i], ...))
  #OUT <- llply(SEQ2, .parallel = TRUE, .fun = function(i) fun(x[i], ...))
  return(base:::simplify2array(OUT, higher = TRUE))
}

#########################################################################
# moving avg diurnal cycle
#########################################################################
roll_daily_cycle <- function(
  # input data
  hhd, 
  # roll window size in days
  win.size = 30,
  # function to compute central measure
  mean.fun = median, 
  # function to compute spread measure
  sd.fun = mad,
  # weight to the sd.fun, if w.sd.fun = 0, 
  # the average daily cycle range is used 
  # (not acconting for the deviations from the mean)
  w.sd.fun = 1){
  # hhd = d
  
  #! check data
  hhd <- as.data.frame(hhd)
  if(ncol(hhd) != 2 | !"date" %in% names(hhd)) {
    stop("input data should have 2 columns: 1st date (POSIXct class) and 2nd the variable (integer or numeric class).")
  }
  # reorder coluns if it isn't in the right order (date, variable)
  date_col <- which(names(hhd) %in% "date")
  hhd <- data.frame(date = hhd[, date_col], value = hhd[, -date_col])
  
  
  # dados de 30 min com coluna de hora, 
  if(require(lubridate) & require(tidyr)){
    hhd_t <- transmute(hhd,
                       hh = lubridate::hour(date) + lubridate::minute(date)/60,
                       #hh = ordered(hh, levels = seq(0, 23.5, by = 0.5)),
                       date = as.Date(date),
                       value = value)
  } else {stop("lubridate and tidyr packages required.")}
  
  # reshape data, spreading half-hour column (hh) along columns, 
  s <- spread(hhd_t, hh, value)
  rm(hhd_t)
  ## check
  # g <- arrange(gather(s, hh, value, -date), date, hh)
  
  # exclude first column("hh") and convert to matrix
  wide <- as.matrix(s[,-1])
  wide <- unrowname(wide)
  
  #run_avg <- c(t(apply(wide,1, stats::filter, rep(1/win.size, win.size))))
  
  # rolling average
  run_avg <- apply(X = wide,
                   MARGIN = 2, 
                   FUN = wmapply, 
                   # args from wmapply function
                   width = win.size, fun = mean.fun, na.rm = TRUE)
  # rolling deviation
  run_sd <- apply(X = wide,
                  MARGIN = 2, 
                  FUN = wmapply, width = win.size, fun = sd.fun, na.rm = TRUE)
  rm(wide)
  # envelope superior
  run_upper <- run_avg + (run_sd * w.sd.fun)
  run_upper <- cbind(run_upper, upper_mx = apply(run_upper, 1, max, na.rm = TRUE))
  # envelope inferior
  run_lower <- run_avg - (run_sd * w.sd.fun)
  run_lower <- cbind(run_lower, lower_mn = apply(run_lower, 1, min, na.rm = TRUE))
  
  rm(run_sd)  
  
  roll_df <- data.frame(s[1], 
                        run_avg, 
                        upper = run_upper[, "upper_mx"],
                        lower = run_lower[, "lower_mn"], 
                        stringsAsFactors = FALSE)
  # names adjustment
  names(roll_df) <- gsub("X", "", names(roll_df))
  rm(run_avg, s)
  roll_df_g <- roll_df %>% 
    gather(hh, value, -date, -upper, -lower) %>% 
    arrange(date, hh) %>%
    mutate(hh = as.numeric(as.character(hh)),
           date = as.POSIXct(paste(date, "00:00:00"), 
                             tz = "UTC") + dhours(hh),
           hh = NULL,
           med = value,
           value = NULL)
  # join data
  j <- join(x = hhd, 
            y = roll_df_g, 
            by = "date", 
            type = "left")
  
  return(j)
  
} # end roll_daily_cycle



#########################################################################
## Funçao para cálculo do qc_flag 
#########################################################################
qc_flags_rcr <- function(rdc.out.data, damping = 1){
  
  # rdc.out.data = roll_daily_cycle(hhd = d, win.size = 60, mean.fun = median, sd.fun = mad,  w.sd.fun = 0)
  # rdc.out.data = rdc_out_data
#  if(require(tidyr) & require(dplyr)){
  qc_rcrsd <- rdc.out.data %>% 
    # amplitude do ciclo diurno do dia
    transform(rng = upper - lower,
              upper = NULL,
              lower = NULL,
              # váriavel preenchida com valores do ciclo médio móvel (p/ calc difs para casos com NA)
              fvalue = ifelse(is.na(value), med, value)
    )
  # primeiras diferencas
  qc_rcrsd %<>% transform(difs = c(NA, diff(fvalue)))
  # série sintética de limites superiores e inferiores
  # baseada nas 1as diferenças
  # se NEE[i]-NEE[i-1] > 0; qc = NEE[i-1] + DELTA_NEE[day]
  # se NEE[i]-NEE[i-1] < 0; qc = NEE[i-1] - DELTA_NEE[day]
  qc_rcrsd %<>% transform(qc = ifelse(difs > 0
                                      ,dplyr::lag(fvalue) + rng
                                      ,dplyr::lag(fvalue) - rng))
  
  
  # PAREI DOCUMENTAÇAO AQUI
  # se NEE_real[i] > NEE[i-1] + DELTA_NEE[day], qc = 0
  # se NEE_real[i] > NEE[i-1] + DELTA_NEE[day], qc = 0
  qc_rcrsd %<>% transform(qcU = ifelse((qc > value & qc > 0), qc, NA),
                          qcL = ifelse((qc < value & qc < 0), qc, NA))
  qc_rcrsd %<>% transform(qcU = approx(x = seq_along(qcU)[!is.na(qcU)], 
                                       y = qcU[!is.na(qcU)], 
                                       xout = seq_along(qcU))$y,
                          qcL = approx(x = seq_along(qcL)[!is.na(qcL)], 
                                       y = qcL[!is.na(qcL)], 
                                       xout = seq_along(qcL))$y)
  qc_rcrsd %<>% transform(qcU    = pmax(qcU, qcL),
                          qcL    = pmin(qcL, qcU))
  #qc_rcrsd %<>% transform(qcU = ifelse(abs(qcU-qcL) < 10, NA, qcU),
  #                        qcL = ifelse(abs(qcU-qcL) < 10, NA, qcL))
  qc_rcrsd %<>% transform(qcU = approx(x = seq_along(qcU)[!is.na(qcU)], 
                                       y = qcU[!is.na(qcU)], 
                                       xout = seq_along(qcU))$y,
                          qcL = approx(x = seq_along(qcL)[!is.na(qcL)], 
                                       y = qcL[!is.na(qcL)], 
                                       xout = seq_along(qcL))$y)
  qc_rcrsd %<>% transform(qcU    = qcU * damping,
                          qcL    = qcL * damping)
  
  
  
  # qc_rcrsd <- rdc.out.data %>% 
  #   # amplitude do ciclo diurno do dia
  #   dplyr::mutate(rng = upper - lower,
  #                 upper = NULL,
  #                 lower = NULL,
  #                 # váriavel preenchida com valores do ciclo médio móvel (p/ calc difs para casos com NA)
  #                 fvalue = ifelse(is.na(value), med, value),
  #                 # primeiras diferencas
  #                 difs = c(NA, diff(fvalue)),
  #                 # serie sintetica
  #                 qc     = ifelse(difs > 0, lag(fvalue) + rng, lag(fvalue) - rng),
  #                 # selecionando qc > filled NEE
  #                 #qcU    = ifelse(qc > value, qc, NA),
  #                 qcU    = ifelse((qc > value & qc > 0), qc, NA),
  #                 # interpolando linearmente maximos
  #                 qcU    = approx(x = seq_along(qcU)[!is.na(qcU)], 
  #                                 y = qcU[!is.na(qcU)], 
  #                                 xout = seq_along(qcU))$y,
  #                 #qcL    = ifelse(qc < value, qc, NA),
  #                 qcL    = ifelse(qc < value & qc < 0, qc, NA),
  #                 # interpolando linearmente minimos
  #                 qcL    = approx(x = seq_along(qcL)[!is.na(qcL)], 
  #                                 y = qcL[!is.na(qcL)], 
  #                                 xout = seq_along(qcL))$y,
  #                 # devido a falhas, por garantia
  #                 #          qcU    = pmax(qcU, qcL),
  #                 #          qcL    = pmin(qcL, qcU),
  #                 qcU    = ifelse(abs(qcU-qcL) < 10, NA, qcU),
  #                 qcL    = ifelse(abs(qcU-qcL) < 10, NA, qcL),
  #                 #          # interpolando linearmente maximos
  #                 qcU    = approx(x = seq_along(qcU)[!is.na(qcU)], 
  #                                 y = qcU[!is.na(qcU)], 
  #                                 xout = seq_along(qcU))$y,
  #                 #          # interpolando linearmente minimos
  #                 qcL    = approx(x = seq_along(qcL)[!is.na(qcL)], 
  #                                 y = qcL[!is.na(qcL)], 
  #                                 xout = seq_along(qcL))$y,
  #                 # damping factor
  #                 qcU    = qcU * damping,
  #                 qcL    = qcL * damping
  #   )
  
    
    
    
#  }# end if required packages
  
  #head(qc_rcrsd)
  
  return(qc_rcrsd %>% 
           dplyr::transmute(date = date,
                     value = value,
                     qc_flag_rcr = ifelse(test = (value > qcU | value < qcL),
                                          yes = 1,
                                          no = 0),
                     qc_flag_rcr = ifelse(is.na(qc_flag_rcr), 0, qc_flag_rcr),
                     qc_upper = qcU,
                     qc_lower = qcL
           )# transmute
  )#return
}# end function qc_flags_rcr


#########################################################################
## Function quality control using rolling cycle range and step diff
#########################################################################
qc_rcr <- function(hhdata,
                   window.size = 60,
                   mean.function = median,
                   sd.function = mad,
                   weight.sd.function = 0,
                   # fato de amortecimento da amplitude 
                   # (= 1, do not change upper and lower limits)
                   damping = 1, 
                   aux.vars = TRUE){
  # hhdata = select(nee_hh_r, date, NEE);
  # window.size = 60; mean.function = median;sd.function = mad;weight.sd.function = 0;damping = 0.7;aux.vars=TRUE
  ## nome da variavel
  var_name <- names(hhdata)[!names(hhdata) %in% "date"]
  
  rdc_out_data <- roll_daily_cycle(hhd = hhdata, 
                                   win.size = window.size, 
                                   mean.fun = match.fun(mean.function), 
                                   sd.fun = match.fun(sd.function),  
                                   w.sd.fun = weight.sd.function)

    
    qc_rcr_out <- qc_flags_rcr(rdc.out.data = rdc_out_data, damping = damping)
    
    # adicionando nome da variavel 
     names(qc_rcr_out)[names(qc_rcr_out) == "value"] <- var_name
     names(qc_rcr_out)[names(qc_rcr_out) == "qc_flag_rcr"] <- paste0("qc_rcr_", var_name)
   
     # se aux.vars = F, retorna somente as c
     if(!aux.vars) qc_rcr_out %<>% select(date, get(var_name))
  
     return(qc_rcr_out)
  
}# end function qc_rcr
