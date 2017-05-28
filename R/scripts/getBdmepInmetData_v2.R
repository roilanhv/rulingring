## loading packages
require(RCurl)


getBdmepInmetData <- function(stnid   = "83726", 
                              sdate   = "01/01/1930", 
                              edate   = "31/12/2012",
                              myemail = "jdtatsch@gmail.com",
                              passwd  = "d17ulev5",
                              userAgent =  "Mozilla/5.0",
                              download = T) {
  
   ## URL to login 
   myURL <- "http://www.inmet.gov.br/projetos/rede/pesquisa/inicio.php" 
   
   # acess data
   myParams = list( mCod        = myemail  , ### alterar!
                    mSenha      = passwd   , ### alterar!
                    btnProcesso = " Acessar ")
   
   # login 
      myCurl <- getCurlHandle()
       curlSetOpt(cookiejar = "cookies.txt", 
                  useragent = userAgent, 
                  followlocation = TRUE, 
                  curl = myCurl)
   
        login <- postForm(myURL, 
                          .params = myParams, 
                          curl = myCurl)
      
   
   
   ## link data
   urlData <- "http://www.inmet.gov.br/projetos/rede/pesquisa/gera_serie_txt.php?&mRelEstacao=XXXXX&btnProcesso=serie&mRelDtInicio=dd/mm/yyyy&mRelDtFim=DD/MM/YYYY&mAtributos=1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,"
      
    ## replace XXXX, dd/mm/yyyy and DD/MM/YYYY by 
    ##        stnid, sdate and edate, respectively
    iurl <- gsub("XXXXX", stnid, urlData)
     iurl <- gsub("dd/mm/yyyy", sdate, iurl)
      iurl <- gsub("DD/MM/YYYY", edate, iurl)

      ## output file name
      outfile <- paste(stnid, ".txt", sep = "")
      outfile <- paste(getwd(), "/", outfile, sep = "")
   
       ## print outfile
       cat("downloading Met. station data:", outfile,"\n")
 
       ## get data from iurl
       idata <- getURLContent(iurl, curl = myCurl)
       
       ## write file
       if(download) {
         writeLines(idata, outfile) 
         cat("File saved at:", outfile, "\n" )
       }
   
    return(outfile)
       
          
}# end function getBdmepInmetData









## stns info
info <- read.table("infoStnsInmet.txt",head=F,sep="\t", stringsAsFactors=F)
names(info) <- c("codigo", "lon", "lat", "alt", "nome")
stnnames <- info$nome

## selecionando codigos de estacoes do RS
#codigosRS <- as.character(info[grep(pattern="-RS", stnnames),"codigo"])
codigos <- as.character(info[,"codigo"])


## looping para aplicar para estacoes
lapply(seq_along(codigos),
       function(i) {
         cat(round(i/length(codigos)*100, 1), "\n") 
         getBdmepInmetData(stnid=codigos[i],hourly=F)
         flush.console()
         Sys.sleep(3)
       }
       )














