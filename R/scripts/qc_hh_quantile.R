  ## Funcao para filtrar dados de 30 minutos
  ## usando o ciclo diurno do quantil (q) de 1 e 99%
  ## da variavel ou o criterio do boxplot (bp) para outlier
  ## saida: dataframe de entrada com a adicao das variaveis
  ## qcflagbp (0 = rejeita, 1 aceita) e qcflagq (0 = rejeita, 1 aceita)

  qc_hh_quantile <- function(X                         ,
                                     var.name   = "NEE"        , 
                                     qtls       = c(0.01, 0.99), 
                                     suffix     = NULL         ,
                                     tz         = "GMT"        , 
                                     plot.cycle = FALSE        ,
                                     show.bp    = FALSE) {

  #TEST: X = filter(nee_hh_r_f2, qc_rcr_NEE == 0) #;X = tbl_df(to_oa(nee_hh_r))
  #TEST: var.name = "NEE"
  #TEST: qtls = c(0.001, 0.999)
  #TEST: tz = "GMT"
  #TEST: plot.cycle = TRUE
  #TEST: show.bp = TRUE
  #TEST: suffix = NULL
    
  # select the variable
  Xs <- select(X, date, get(var.name))
  #Xs <- select(X, one_of(c("date", variable)))
  
  #  standard name to the   
  names(Xs)[2] <- "variable"

  ## insure a constant time steps
  Xsr <- regularize(Xs, tz = tz)
  #rm(X, Xs)
  ## strings with hour minute
  h_m <- hm(paste(hour(Xsr$date), minute(Xsr$date), sep = ":"))
  h_m <- ordered(h_m, 
                 levels = as.character(h_m[order(unique(as.duration(h_m)))]))
  #h_m <- factor(as.character(h_m), 
  #              levels =  as.character(h_m[order(unique(as.duration(h_m)))]),
  #              ordered = TRUE)
  Xsr <- mutate(Xsr, 
              hour_min = h_m
              )
  #rm(h_m)
    
  ndays <- unique(table(Xsr$hour_min))
  # ndays * 48 == nrow(X)

  ## obtendo limites inferiores e superiores do boxplot da variavel por horario
  bp <- boxplot(variable ~ hour_min, 
                data = Xsr, 
                plot = show.bp)
   # boxplot(X[,2] ~ hour(X$date),plot = T)
  ## limites estimados usando bp (boxplot)
   bounds_bp <- t(apply(bp$stats, 2, range))
   # bounds_bp <- data.frame(hour = sort(unique(hours(X$date))),bounds_bp)   
   u_hm <- bp$names
   bounds_bp <- data.frame(hour_min = u_hm,
                          bounds_bp)   
   names(bounds_bp) <- c("hour_min","lower","upper")

  ## obtendo o quantil inferior e superior dos dados por hora
  bounds_q <- summarise(group_by(Xsr, hour_min), 
                       lower = quantile(variable, 
                                        prob = c(qtls[1]), 
                                        na.rm = TRUE),
                       upper = quantile(variable, 
                                        prob = c(qtls[2]), 
                                        na.rm = TRUE)
                       )
  bounds_q <- as.data.frame(bounds_q)
  bounds_bp$hour_min <- bounds_q$hour_min
  
  ## limites combinados
  bounds_comb <- data.frame(hour = bounds_bp$hour_min,
                            #hour = u_hm,
                            
                            lower = apply(cbind(bounds_bp[, "lower"], 
                                                bounds_q[, "lower"]), 
                                          MARGIN = 1, 
                                          FUN = min, 
                                          na.rm = TRUE),
                            upper = apply(cbind(bounds_bp[, "upper"], 
                                                bounds_q[, "upper"]),
                                          MARGIN =  1, 
                                          FUN = max, 
                                          na.rm = TRUE)
                            )

  #bounds_comb$hour <- ordered(bounds_comb$hour, levels = levels(h_m))
  #o <- order(bounds_comb[, "hour"])
  #bounds_comb <- bounds_comb[o, ]
  
  ## gráfico
  if(plot.cycle) {
  with(Xsr, 
       plot(hour_min,
            variable, 
            pch = 20, 
            ylab = eval(var.name),
            las = 2))
          
          lines(bounds_comb[, "hour"], 
                bounds_comb[, "lower"], 
                col = 2, 
                lwd = 3)
          text(bounds_comb[, "hour"],
               bounds_comb[, "lower"],
               labels = round(bounds_comb[, "lower"],2),
               col=3, cex = 0.5)
  
          lines(bounds_comb[, "hour"],
                bounds_comb[, "upper"], 
                col = 4, 
                lwd = 3)
          text(bounds_comb[, "hour"],
               bounds_comb[, "upper"],
               labels = round(bounds_comb[, "upper"],2),
               col=3, cex = 0.5)
          
               legend("top",
                      xpd = TRUE,
                      inset=c(0.25,-0.1),
                      horiz=TRUE,
                       lty = c(1, 1),
                       col = c(4, 2),
                       lwd =c(2, 2),
                       legend = c(paste0("quantil superior", 
                                         " (",
                                         qtls[2]*100," %)"),
                                paste0("quantil inferior", 
                                       " (",
                                       qtls[1]*100,
                                       " %)")
                                ),
                       border = NA,
                       bg = "transparent",
                       bty = "n"
                      )
    }
  # readline("press enter to continue:")

  ## series temporais dos limites superiores e inferiores da variavel
  lo <- rep(bounds_comb[,"lower"], ndays)
  up <- rep(bounds_comb[,"upper"], ndays)
   
  qtlsl <- rep(bounds_q[,"lower"], ndays)
  qtlsu <- rep(bounds_q[,"upper"], ndays)
 
  bpl   <- rep(bounds_bp[, "lower"], ndays)
  bpu   <- rep(bounds_bp[, "upper"], ndays)

  ## 1 fora do range (devem ser filtrados) 0 dentro do range (valores aceitos)
  qcflag_q <- ifelse(Xsr$variable <= qtlsu & Xsr$variable >= qtlsl, 0, 1)
  qcflag_bp <- ifelse(Xsr$variable <= bpu & Xsr$variable >= bpl, 0, 1)
  qcflag_comb <- ifelse(Xsr$variable <= up & Xsr$variable >= lo, 0, 1)
  ## casos de NA sao atribuidos zero
  qcflag_q[is.na(qcflag_q)]   <- rep(0, sum(is.na(qcflag_q)))
  qcflag_bp[is.na(qcflag_bp)] <- rep(0, sum(is.na(qcflag_bp)))
  qcflag_comb[is.na(qcflag_comb)] <- rep(0, sum(is.na(qcflag_comb)))
 
  #X <- transform(X, qcflagq = NULL, qcflagbp = NULL)
 
  res <- mutate(Xsr, 
                #qcflag_q = qcflag_q, 
                #qcflag_bp = qcflag_bp,
                qcflag_comb = qcflag_comb,
                hour_min = NULL)
 
   names(res)[names(res) == "variable"] <- var.name
   ## adding suffix
   if(is.null(suffix)){
    names(res)[grep("^qc", names(res))] <- paste0("qc_", 
                                                  var.name,
                                                  "_q",
                                                  qtls[1]*100)
   } else {
     names(res)[grep("^qc", names(res))] <- paste0("qc_", 
                                                   var.name,
                                                   #"_q",
                                                   "_",
                                                   suffix)
   }
  
  rm(Xsr, qcflag_q, qcflag_bp, qcflag_comb, bpu, bpl, 
     qtlsu, qtlsl, up, lo, bounds_comb, bounds_bp,
     bounds_q, bp, ndays)
 
  gc()
  
  return(tbl_df(res))

} # end function DlyCycleBasedFilter


