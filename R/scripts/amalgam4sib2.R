#########################################################################
##   Script para conetar LHOAT-AMALGAM
#   

amalgam4sib2 <- 
    function(params.file = params.file,
             sens.file = sens.file,
             infile2 = paste0("/home/",
                              system("echo $USER",intern = TRUE),
                              "/Dropbox/Dissertacao/TORRE.txt"),
             model.FUN = 'sib2',
             code_dir = "~/Dropbox/Dissertacao/CALIBRACAO/MULTI_OBJ/AMALGAM_R_Code-V1.0/",
             N = 25,
             objectives_functions = 2,
             data_obs = obs,
             total.runs = 100,
             model.FUN.args = model.FUN.args,
             gof.FUN = c("NSEmod","bmax"),
             gof.VAR = c("albedo","albedo"),
             gof.Ini = gof.Ini,
             gof.Fin = gof.Fin,
             gof.FUN.args = list(na.rm = TRUE)
             
    ){
        # 
  actual.dir <- getwd()
  message("You are in this directory:", actual.dir)
  message("Moving to new directory..", code_dir)
  setwd(code_dir)
  #source all files used:
  source('source_allfiles.r')
   # message(paste0(ls(), collapse = "\n "))
  setwd(actual.dir)
  # message(paste0(ls(),collapse = "\t\n"))
  require(data.table)
  require(hydroGOF)
  require(openair)
  require(magrittr)
  require(plyr)
  require(dplyr)
  # Define which algorithms to use in AMALGAM
  Extra<-NULL
  Extra$Alg <- c("GA","PSO","AMS","DE")
  Extra$Functions <- gof.FUN
  Extra$FunctionsArgs <- gof.FUN.args
  Extra$model.FUN <- model.FUN
  Extra$model.FUN.Args <- model.FUN.args
  Extra$gof.FUN <- gof.FUN
  Extra$gof.VAR <- gof.VAR
  Extra$gof.FUN.Args <- gof.FUN.args
  Extra$gof.Start <- gof.Ini
  Extra$gof.End <- gof.Fin
  
  # Define the number of algorithms
  AMALGAMPar<-NULL
  AMALGAMPar$q <- length(Extra$Alg)
  
  Params <- hydroPSO::read.ParameterRanges(ParamRanges.fname = params.file)
  Sens <- read.table(sens.file, header = TRUE)
  NonSensParams.names <- as.vector(Sens$ParameterName[which(Sens$RelativeImportance.Norm == 0.0)])
    
    if(length(NonSensParams.names) >= 1 ){
        message("| Warning: Some parameters are non dominated in this analisys:")
        message(paste("[ Parameters' names   : ", paste(NonSensParams.names, collapse = ", "), " ]", sep = ""))
    Params <- Params[-which(rownames(Params) %in% NonSensParams.names),]
        message("| In calibration run are: ", paste(rownames(Params),collapse = ", ")) 
        
    }
    
    
  AMALGAMPar$n <- nrow(Params)              # Dimension of the problem
  AMALGAMPar$N <- N                         # Size of the population
  AMALGAMPar$nobj <- objectives_functions   # Number of objectives
  AMALGAMPar$ndraw <- total.runs            # Maximum number of function evaluations
  
  # Define the parameter ranges (minimum and maximum values)
  ParRange<-NULL
  
  ParRange$minn <- Params$MinValue
  ParRange$maxn <- Params$MaxValue
  # How is the initial sample created -- Latin Hypercube sampling
  Extra$InitPopulation <- "LHS"
  Extra$ParamsNames <- rownames(Params)
  # Define the measured soil water content data
  Measurement<-NULL
  Measurement$MeasData <- data_obs
  Measurement$Sigma <-NULL
  Measurement$N = length(Measurement$MeasData)
  
  # Define ModelName
  ModelName = model.FUN
  
  # Define the boundary handling
  Extra$BoundHandling <- "Bound" 
  
  # True Pareto front is not available -- real world problem
  Fpareto <-NULL
  
  
  # Store example number in structure Extra
  Extra$example <- example
  Extra$m <- AMALGAMPar$n
  
  # system.time(
  # Run the AMALGAM code and obtain non-dominated solution set
  tmp <- AMALGAM(
    AMALGAMPar = AMALGAMPar
    ,
    ModelName = ModelName
    ,
    ParRange = ParRange
    ,
    Measurement = Measurement
    ,
    Extra = Extra
    ,
    Fpareto = Fpareto
  )
  names(tmp) <- c("output", "ParGen", "ObjVals", "ParSet")

  names2row <- sapply(seq( 1:(AMALGAMPar$ndraw/AMALGAMPar$N)) ,
                      FUN = rep, 
                      times = AMALGAMPar$N)  %>% as.vector
  
  output <- tmp[[1]][[1]] %>% set_colnames(c("Iter",Extra$Alg)) %>% as.data.frame
  # ParGen <- tmp[[2]] 
  # ObjVals<- tmp[[3]]
  ParSet <- tmp[[4]] %>% set_rownames(names2row) %>% set_colnames(c(rownames(Params),"Cg",gof.FUN))
  
  
  
  return(list(ParSet,output))
  
  # tmp <- list(ParSet,output)
  
  
}


