
sib2 <- function(multicamadas = "off",
                 date1="2001010102",
                 date2="2014123124",
                 infile2="/home/hidro2roilan/Dropbox/Dissertacao/TORRE.txt",
                 dtt="3600",
                 itrunk = "20",
                 ilw = "5",
                 ivtype = "2",
                 istype = "3",
                 isnow = "0",
                 ipbl = "1",
                 idirr = "0",         #!
                 decay = "0.85",      #!variando
                 zlong = "-47.63",
                 zlat = "-21.61",
                 poros = 0.458  ,   #! soilpara(1,k)
                 phsat = "-0.2"  ,    #! soilpara(2,k)
                 satco = "3.5E-06",   #! soilpara(3,k)
                 bee = "7.797"   ,    #! soilpara(4,k)
                 slope = "0.03"  ,    #! soilpara(5,k)
                 #!speyield = 0.20  , #! soilpara(6,k)
                 slpp = "1.0",
                 tc = "297.7",
                 tg = "296.7",
                 td = "296.0",
                 capac = c("0.0", "0.0"),
                 snoww = c("0.0", "0.0"),
                 www = c(0.12, 0.14, 0.14),
                 surdep = "0.0",
                 gwdep = "6.0",
                 app = "0.0001",
                 bpp = "20.0",
                 cpp = "0.9999",
                 tc_ini = "297.7",
                 tg_ini = "296.7",
                 td_ini = "296.7",
                 www_ini = c(0.12, 0.14, 0.14),
                 zwind = "21.0",
                 zmet = "11.0",
                 gmudmu = "1.0",
                 green = "0.8",
                 vcover = "0.72",
                 id = "001",
                 verbose = FALSE,
                 vars_out = "",
                 dir_out = paste0("/home/",system("echo $USER",intern = TRUE),"/ADBHM/trash/"),
                 reflv = "0.1" ,
                 refdv = "0.13" ,
                 refln = "0.45" ,
                 refdn = "0.39" ,
                 tranlv = "0.05" ,
                 trandv = "0.001",
                 tranln = "0.25",
                 trandn = "0.001",
                 sorefv = "0.08",
                 sorefn = "0.200",
                 z0s_cst = "0.05",
                 g4_cst = "11.785",
                 g1_cst = "1.449",
                 z2 = "7.0",
                 z1 = "0.143", 
                 zc = "0.334",
                 chil = "0.1",
                 leafw = "0.03",
                 leafl = "0.5",
                 effcon = "0.08",
                 gradm = "8.0", 
                 binter = "0.01",  
                 respcp = "0.015", 
                 atheta = "0.98", 
                 btheta = "0.95", 
                 rootd = "1.5",
                 phc = "-200", 
                 trop = "298.0", 
                 slti = "0.2",  
                 hlti = "288.16", 
                 hhti = "313.16", 
                 vmax = "7.5e-5",
                 sodep = "1.00",
                 rootex = "0.0",
                 anik = "2.0",
                 nlayers = "10",
                 beta = 0.966,
                 dz = 0.0,
                 hr_proc = 1,
                 ksat_pre = 0
                 ){
 
    if(verbose){
        cat(">","\n","Arquivo com dados de entrada:","\n")
        cat(infile2,"\n")
        cat("diretorio de saida: ",dir_out,"\n")
    }
    if(!dir.exists(dir_out))dir.create(dir_out)
  
    infile2 =  paste0("'",infile2,"'") 
    dir_out =  paste0("'",dir_out,"'") 
# Parâmetros de entrada para o DBHM
     paras <- paste(date1, date2, infile2, dtt, itrunk, ilw, ivtype, istype, isnow, 
         ipbl, idirr, decay, zlong, zlat, poros, phsat, satco, bee, slope, slpp, 
         tc, tg, td, capac[1], capac[2], snoww[1], snoww[2], www[1]/poros,www[2]/poros,www[3]/poros, 
         surdep, gwdep, app, bpp, cpp, tc_ini, 
         tg_ini, td_ini, www_ini[1]/poros, www_ini[2]/poros, www_ini[3]/poros, zwind, zmet, gmudmu,
         vcover,green,
         id, dir_out,
         reflv,refdv,refln,refdn,tranlv,trandv,tranln,trandn,sorefv,sorefn,
         z0s_cst, g4_cst, g1_cst, 
         z2, z1, zc, chil, leafw, leafl,
         effcon, gradm, binter, respcp, atheta, btheta, rootd, 
         phc, trop, slti, hlti, hhti, vmax,
         sodep,rootex,anik,nlayers,beta,dz,hr_proc,ksat_pre)
# Linha de comando para rodar o SiB2
    if(multicamadas == "off"){
     command_line <- paste("cd ~/Dropbox/Dissertacao/sib2_offline/; echo",
                     "\"", 
                     paras,
                     "\"",
                     "| time -p ./sib2_offline.out")
    } else {
        command_line <- paste("cd ~/Dropbox/Dissertacao/sib2_offline_multicamada/; echo",
                              "\"", 
                              paras,
                              "\"",
                              "| time -p ./sib2_offline.out")
    }
    # if(id == "666") command_line <- paste("cd ~/Dropbox/Dissertacao/sib2_offline/; echo",
    #                  "\"", 
    #                  paras,
    #                  "\"",
    #                  "| time -p ./sib2_offlinesinsun.out")
     if(verbose){
        cat("==================================================================","\n")
         cat(paras,"\n")
         cat("=================================================================","\n")
         cat(command_line, "\n")
         cat("=================================================================","\n")
     }         
# Chamada para rodada do DBHM
     system(command = command_line,
            intern = verbose,
            ignore.stdout = !verbose,ignore.stderr = !verbose )
# Lendo a saída do DBHM
     date = data.frame(date = seq.POSIXt(from = as.POSIXct(date1,tz = "GMT","%Y%m%d%H")-3600,
                                            to =  as.POSIXct(date2,tz = "GMT","%Y%m%d%H")-3600,
                                            by = "hour"))
    if(verbose) cat("Reading sib2diag.txt","\n")
     
  if(file.exists(gsub(pattern = "'",replacement = "",
                      x = paste0(dir_out,"sib2diag",id,".txt")))){
     header <- read.table(file = gsub(pattern = "'",replacement = "",
                                      x = paste0(dir_out,"sib2diag",id,".txt")),
                          header = TRUE,nrows = 1)
     fww <- read.table(file = gsub(pattern = "'",replacement = "",
                                  x = paste0(dir_out,"sib2diag",id,".txt")),
          header = TRUE,nrows = length(date[,1]),skip = 2)
     
# Calculando variáveis de conteúdo de água no solo [camada 0-1m, 1-2m]
    if(verbose) cat("Calculando conteudo de água nas camadas","\n")
    if( (header$zdepth1[1] + header$zdepth2[1]) >= 1.0){
        fww$swc1 <- header$poros *(fww$www1 * header$zdepth1 + fww$www2 * (1.0 - header$zdepth1))*1000
        fww$swc2 <- header$poros *(fww$www2 * (header$zdepth1+header$zdepth2 - 1.0) +
                                    fww$www3 * (2.0-(header$zdepth1+header$zdepth2)))*1000
    } else {
        fww$swc1 <- header$poros *(fww$www1 * header$zdepth1 + fww$www2 * header$zdepth2 +
                                    (1.0 - header$zdepth1 - header$zdepth2) * fww$www3 )*1000
        fww$swc2 <- header$poros * fww$www3 * header$zdepth3 * 1000
    }
    fww$swc2m=fww$swc1+fww$swc2
    # Componentes de LE em mm/h

    if("LE.mm" %in% vars_out) fww$LE.mm <- fww$LE * 1.4688e-3
    if("ec.mm" %in% vars_out) fww$ec.mm <- fww$ec * 1.4688e-3
    if("eci.mm" %in% vars_out) fww$eci.mm <- fww$eci * 1.4688e-3
    if("ect.mm" %in% vars_out) fww$ect.mm <- fww$ect * 1.4688e-3
    if("eg.mm" %in% vars_out) fww$eg.mm <- fww$eg * 1.4688e-3
    if("egi.mm" %in% vars_out) fww$egi.mm <- fww$egi * 1.4688e-3
    if("egs.mm" %in% vars_out) fww$egs.mm <- fww$egs * 1.4688e-3
    # Selecionando variáveis específicas se for o caso
    # # Para todas as variáveis

    if(vars_out[1] == "") vars_out <- names(fww)
      if(verbose) cat("selecionando variáveis","\n")
         out <- cbind(date,fww[,vars_out])
           names(out) <- c("date",vars_out)
           return(out)
  } else {
      return(NULL)
  }
     
     
   
}












