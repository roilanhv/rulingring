  ##############################################################################
  ## regularize.R                                                              #
  ##############################################################################
  ## Jonatan Dupont Tatsch at LCB/IAG/USP
  ## jdtatsch[at]gmail.com
  ## last update: 19 set 2012
  ##
  ## Function to regularize a data frame based on the first ans last date 
  ## and time. The input dataframe should has a date column (POSIXct object). 
  ## The time step will be defined as the most frequent in the diff(date).
  ## Missing dates will be filled with NA.
  ##
  ######################################################################  
  regularize <- function(X, tz = "GMT") {
    # X <- Xs #tbl_df(to_oa(nee_hh))
    # remove rownames
    X <- unrowname(x = X)
     
     u_dts <- unique(diff(X$date))
     
     if(length(u_dts) > 1){
       message("There are more than one time step in the time series: ")
       tab <- round(prop.table(table(diff(X$date)))*100,2)
       tab_df <- data.frame(time = names(tab),freq = paste(tab, "%"))
       print(tab_df)
     }
     ## timestep
     deltat <- as.numeric(names(which.max(table(diff(X$date)))))
    
    
      ## obtain start and end date
      min.min <- as.integer(as.character(min(minute(X$date))))
      max.min <- as.integer(as.character(max(minute(X$date))))
      h.min   <- as.integer(as.character(min(hour(X$date))))
      h.max   <- as.integer(as.character(max(hour(X$date))))
      
      sdate <- as.POSIXct(paste(min(as.Date(X$date))," ", 
                                #h.min,":",min.min, ":00",
                                0,":",30, ":00",
                                sep = ""), 
                          tz = tz)
      edate <- as.POSIXct(paste(max(as.Date(X$date))," ",
                                #h.max,":",max.min, ":00",
                                0,":",0, ":00",
                                sep = ""), 
                          tz = tz)

      ## time unit
      outstring <- strsplit(capture.output(min(diff(X$date)))," ")[[1]]
      deltatString <- paste(deltat, 
                            outstring[length(outstring)],
                            collapse=" ")
      cat("Dataset time step:", deltatString, "\n")
      rm(outstring)

      ## data frame with a regularized date sequence
      dfdate <- tbl_df(data.frame(date = seq(sdate, edate, by = deltatString)))
      # table(diff(dfdate$date))

      ## dataframe X na sequencia temporal correta e regularmente espacada
      ## Xrts: X regular time series
      Xrts <- join(dfdate, X, by = "date",type = "left")
      # dim(X); dim(dfdate); dim(Xrts)   
  
      return(Xrts)
    
  } # end function regularize

