## loading packages
## require(RCurl)


getBdmepInmetData <- function(stnid   = "83726", 
                              sdate   = "01/01/2000", 
                              edate   = "31/12/2014",
                              myemail = "jdtatsch@gmail.com",
                              passwd  = "d17ulev5",
                              userAgent =  "Mozilla/5.0",
                              download = T) {
  
   ## link data
   urlData <- 'http://www.inmet.gov.br/projetos/rede/pesquisa/gera_serie_txt.php?&mRelEstacao=XXXXX&btnProcesso=serie&mRelDtInicio=dd/mm/yyyy&mRelDtFim=DD/MM/YYYY&mAtributos=1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,'
   urlData <- gsub(pattern = "&", "\\\\&", urlData )
   
   ## cat(urlData) 
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
       # system("curl -X POST -c cookies.txt -dusername=jdtatsch@gmail.com&password=d17ulev5 http://www.inmet.gov.br/projetos/rede/pesquisa/inicio.php")
       cmd <- paste("curl -L -b cookies.txt",iurl,">",outfile)
       
       
       ## write file
       if(download) {
         system(cmd,intern = T,wait = T)
         cat("File saved at:", outfile, "\n" )
       }
   
    return(outfile)
       
          
}# end function getBdmepInmetData














