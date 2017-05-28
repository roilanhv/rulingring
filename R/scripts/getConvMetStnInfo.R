## Jonatan Tatsch
## Dep Fisica - UFSM
## 08/10/2013

## informações das estacoes convencionais do INMET do link,
##  http://www.inmet.gov.br/portal/index.php?r=estacoes/estacoesConvencionais,
##  depois clicando no icone de uma estação, abriu a pop-up com informacoes da estacao, 
##  cliquei com botao direito > view frame source e copiei endereco desse link


## lendo conteudo do site
x <- readLines("http://www.inmet.gov.br/sim/sonabra/index.php")

## seleciono as linhas de dados que me interessa
x <- x[77: length(x)]



## extraindo codigos e nomes das estacoes
#searchStr <- "-A[0-9]{3}"
searchStr <- "AUTOMATICA|CONVENCIONAL"
rawCodes <- x[grep(searchStr, x = x, value=F)]
  ## substitui tudo que nao for numero por nada
  codigos <- gsub("[^0-9]", "" ,rawCodes)
  head(codigos)
  length(codigos)


#searchStr <- "createMarker"
#x[grep(searchStr,x=x,value=F)-10]





searchStr <- "createMarker"
 ## substitui todas letras minusculas por 
 rawNames <- gsub("[[:lower:]]","" , 
                  substr(grep(searchStr,x = x, value = T), 1,100))
  ## limpando string
  rawNames <- gsub(" M\\(,","",rawNames)
   rawNames <- gsub("[<>,;]","",rawNames)
    rawNames <- gsub("[\\'\\']","",rawNames)
     ## dividi cada string de acrdo com "-="
     rawNames <- unlist( strsplit(rawNames,"-=")) [ c(T,F) ]
      nomes <- unlist(strsplit(rawNames,"-"))[c(T,F)]
      estado <- gsub(" ","",unlist( strsplit(rawNames,"-"))[c(F,T)])




## extraindo lons das estacoes
searchStr <- "GLatLng"
## posicoes que 
pos <- cbind( grep(searchStr,x=x,value=F),  grep(searchStr,x=x,value=F)+1)

 xy <- gsub("[[:lower:]]","" , x[pos[,1]])
 xy <- gsub("[=GLL\\(\\);]","",xy)

  lons <- as.numeric(unlist(strsplit((xy),",")))[c(F,T)]
   lats <- as.numeric(unlist(strsplit((xy),",")))[c(T,F)]


  

 
## extraindo alts das estacoes
searchStr <- "Altitude:"
## procurando posicoes (pos) do vetor x que tenham a string "Altitude:"
 pos <- grep(searchStr,x=x,value=F) 
 ## altitudes brutas
 rawAlts <- x[ pos ]
  ## dividi a string usando a palavra "Altitude: " 
   ## before and after altitude
   baAltitude <- strsplit(rawAlts, searchStr)
   ## desmancho lista e seleciono elementos 2, 4, 6 ,8, ...
   rawAlts <- unlist(baAltitude)[c(F,T)]
   ## before and after altitude apos limpeza basica
   baAltitude2 <- strsplit(rawAlts, "metros")
    ## desmanchandoa lista baAltitude2
    baAltitude2 <- unlist(baAltitude2)[c(T,F)]
    ## cnverte de character para numeric o vetor baAltitude2
     Alts <- as.numeric(baAltitude2)


  ## criando data frame com todas informacoes extraidas acima
  stnInfo <- data.frame(codigo=codigos, 
                        nome=nomes,
                        estado = estado,
                        lon =as.numeric(lons), 
                        lat=as.numeric(lats),
                        alt = Alts,
                        stringsAsFactors=F)


## escrevendo tabela em um arquivo csv
write.csv(stnInfo, file = "convMetStnInfo.csv",row.names=F)
