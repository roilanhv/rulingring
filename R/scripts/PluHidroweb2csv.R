
## .............................................................................
## Jonatan Dupont Tatsch, LCB/IAG/USP
## jdtatsch@gmail.com
## 1 outubro 2010
##
## Script para leitura e conversao dos dados dos arquivos de precipitacao diaria
## baixados do site hidroweb da ANA. Os arquivos txt sao convertidos em um 
## dataframe com 2 colunas: date e prec que eh salvo como csv.
## .............................................................................

## Configurando opcao para nao considerar character como factor
options(stringAsFactor=F)

###############################################################################
## Function to read precipitation data from ANA and convert it  in a csv file
###############################################################################
PluHidroweb2csv <- function(filename, 
                            dup.first = T, 
                            sdate = "1912-01-01", 
                            edate = "2015-12-31",
                            show.warn = F){
## test parameters
# dup.first = T; sdate = "1920-01-01";edate = "2009-12-31"
# filename <- "txts/CHUVAS1948001.TXT"
# filename <- filesList[2]

## to ensure that file exists
cat("filename:",filename,"\n")
 if(!file.exists(filename)) stop("file do not exists")

## read txt file
 tmp <- read.csv2(filename, skip = 15, head = T)

## corrigindo nome da primeira coluna
  names(tmp)[1] <- "EstacaoCodigo"

## .............................................................................
## TRATAMENTO PARA DATAS DUPLICADAS
## se o parametro dup.first = T, considera-se somente o primeiro elemento da 
## duplicacao e vice-versa
## data frame tmp com dados unicos
if(dup.first)  tmpu <- tmp[!duplicated(tmp[, "Data"]), ] else 
               tmpu <- tmp[!duplicated(tmp[, "Data"], fromLast=T), ]
# dim(tmp); dim(tmpu)

## .............................................................................
## VERIFICACAO DA SEQUENCIA TEMPORAL DAS DATAS
## vetor de datas do arquivo txt original
   tmpuDate <- as.Date(tmpu$Data,"%d/%m/%Y")

## definindo data inicial baseado nos parametros da funcao
    sdate <- as.Date(sdate, "%Y-%m-%d")
## end month and end year
    edate <- as.Date(edate, "%Y-%m-%d")
## gerando serie de datas de referencia para o periodo sdate:edate
## util para checar se tmp nao pula algum mes (falha de sequencia temporal)
    seqDates <- seq(from=sdate, to=edate, by="months")

## ordenar os dados na sequencia temporal correta
    o <- order(tmpuDate)
## ordenando os dados corretamente e gerando novo
#! dataframe temporario porem com datas unicas (tmpu)
    tmpu <- tmpu[o, ]; rownames(tmpu) <- NULL

## caso o tmpDate abranja um periodo anterior ao sdate, tmp tera mais linhas 
## que o tamanho do seqDates, para evitar isso selecionar somente 
#! ANA tem dados ate 2010!!!!!!!
    tmpu <- subset(tmpu, as.Date(tmpu$Data, "%d/%m/%Y") >= sdate & as.Date(tmpu$Data, "%d/%m/%Y") <= edate)

#! tmpDate ordenado e na escala de tempo diaria
    tmpuDateOk  <- as.Date(tmpu$Data,"%d/%m/%Y")
## serie temporal de datas
#    tmpuDlyDate <- seq(tmpuDateOk[1], tmpuDateOk[length(tmpuDateOk)], by = "days") 
     

##...............................................................................  
## PREPARACAO DE MATRIZ DE REFERENCIA
## as dimensoes sao baseadas no periodo de seqDates e as colunas no num.
## de cols de tmp
## os dados dos meses existentes em tmpU sao colocados em ref
   ref <- data.frame( matrix(NA, nrow=length(seqDates), ncol=ncol(tmpu)) )
    names(ref) <- names(tmpu)
#! nao sei pq nesse formato
     ref$Data <- format(seqDates,"%m/%Y")

      select <- which(names(ref)=="Data")
## colocando os dados de tmp em ref considerando as datas coincidentes
      ref[seqDates %in% tmpuDateOk, -c(select)] <- tmpu[,-c(select)]

## aviso para indicar falha de sequencia temporal
   if(sum(seqDates %in% tmpuDateOk) != nrow(tmpu)) 
     cat("arquivo", filename,"apresenta falha de sequencia temporal","\n")

##...............................................................................  
## construindo vetor de characteres para selecao de colunas 
## columns names of daily rainfall data
   dOfM <- 1:31
    dataCols <- paste("Chuva", 
                      ifelse(dOfM <10, paste("0",dOfM,sep=""),dOfM), 
                      sep="" )

## aplicando funcao para gerar serie temporal de chuva
## dataframes com colums: Time and Data
     pluTS  <- wide2long(X = ref, dataCols = c("Data", dataCols))
# plot(pluData, type="l")

## data frame com informacao de qualidade da medida (status) de precip 
## Status: 0 = Branco, 1 = Real, 2 = Estimado, 3 = Duvidoso, 4 = Acumulado
## dataframes com colums: Time and Data  
     qualTS <- wide2long(X = ref, dataCols = c("Data",  paste(dataCols, "Status", sep = "")))
# plot(qualData, type="l"); unique(qualData$Data)


## informacoes de grau de consistencia e metodo de medida de vazao 
## NivelConsistencia: 1 = Bruto, 2 = Consistido 
## TipoMedicaoChuvas: 1 = Pluviometro, 2 = Pluviografo, 3 = Data logger
## dataframes com colums: Time and Data
#     consLevTS <- getAuxData(X = ref, dataCols = c("Data", "NivelConsistencia") )
     consLevTS <- getAuxData(X = ref, dataCols = c("Data", "TotalAnualStatus") )
     pluMethodTS <- getAuxData(X = ref, dataCols = c("Data", "TipoMedicaoChuvas") )

##...............................................................................  
## PREPARACAO DATA FRAME DE SAIDA
     df <- data.frame(date    = pluTS$Time,
                      prec    = pluTS$Data, 
                      Qual    = qualTS$Data, 
                      ConsLev = consLevTS$Data, 
                      MChuva  = pluMethodTS$Data)

      if(show.warn) cat("convertendo para csv arquivo" , filename, "\n")
      flush.console()

## gerando nome para arquivo CSV    
      outFileName <- gsub("\\.TXT","\\.csv",filename)
       #outFileName <- paste(sub("-","_", outFileName )  ,".csv", sep="")
        #outFileName <- gsub("CHUVAS","",outFileName)
         write.csv( df, outFileName, row.names=F, quote=F,na="-99.9")
          rm(tmp, tmpu, ref, dOfM)       
} # end function readHidrowebFile




#####################################################################
## Function to make a vector of dates with 31 days each one 
monthsWith31days <- function(sm, sy, em, ey)
{
## constroi vetor de data todos meses contendo 31 dias   
  x <- expand.grid(d=1:31, m=1:12, y=sy:ey)
  srow <- with(x, which(d==1 & m==sm & y==sy))
  erow <- with(x, which(d==31 & m==em & y==ey))
   x <- x[srow:erow,]
## as.Date coloca NA para data inexistentes    
   xDate <- as.Date(  do.call("paste", c(x[,c(3,2,1)], sep="-")))
   return(xDate)
} # end function monthsWith31days 

#####################################################################
## Funcao para verificacao da sequencia temporal dos dados de vazao
## entrada: dataframe com os dados brutos lidos do arquivo txt da 
## hidroweb (tmp) e um vetor (dataCols) de caracteres com as variaveis
## (colunas) a serem selecionadas
## saida: um data frame (df) com as datas na 1a col. e os dados na 2a col.

wide2long <- function(X, dataCols)
{
## selecionando colunas de interesse   
  X <- X[, names(X) %in% dataCols]

## criando novo tmp baseado no ref
## tmp com as colunas do dia 1  a 31 (coluna data excluuida)
   values <- X[, -1]

## tmp as vector
   valVec <- c(t(values))

#...............................................................................
## range de datas
   rdate <- range(as.Date(paste("01/", X$Data, sep=""), "%d/%m/%Y") )
   sdate <- rdate[1]; edate <- rdate[2]
## vetor de data consistente com tamanho de tmpVec
   nadate <- monthsWith31days(sm = as.integer(format(sdate, "%m")), 
                              sy = as.integer(format(sdate, "%Y")), 
                              em = as.integer(format(edate, "%m")), 
                              ey = as.integer(format(edate, "%Y")))
# length(nadate)

## check 
# valVec[is.na(nadate)] # all values should be NA

## data frame padrao
  df <- data.frame(Time = nadate[!is.na(nadate)], 
                   Data = valVec[!is.na(nadate)])

## arredondamento
  df$Data <- round(df$Data, 3)
  
  return(df)

} # end function

################################################################################
## Funcao para pegar as informacoes auxiliares sobre Nivel de Consistencia
## e o Tipo de Medicao de vazao
## entrada: dataframe sem dados duplicados (tmpu) criado apos leitura do arquivo 
## txt hidroweb
## saida: um data frame (df) com as datas na 1a col, 
## nivels de consistencia (ConsLev) ou Tipo de Med. Chuvas na 2a col

getAuxData <- function(X, dataCols)
{

## selecionando colunas de interesse   
  X <- X[, names(X) %in% dataCols]

## para excluir colunda de data para definicao do vetor de dados de saida 
  select <- which(names(X)=="Data")

## vetor na sequencia temporal
  vec <- rep(X[, -c(select)], each = 31) 

#...............................................................................
## range de datas
   rdate <- range(as.Date(paste("01/", X$Data, sep=""), "%d/%m/%Y") )
   sdate <- rdate[1]; edate <- rdate[2]
## vetor de data consistente com tamanho de tmpVec
   nadate <- monthsWith31days(sm = as.integer(format(sdate, "%m")), 
                              sy = as.integer(format(sdate, "%Y")), 
                              em = as.integer(format(edate, "%m")), 
                              ey = as.integer(format(edate, "%Y")))
#   length(nadate)
#...............................................................................

## dataframe de saida com as duas informacoes
  df <- data.frame(   Time = nadate[!is.na(nadate)],
                      Data =    vec[!is.na(nadate)])

  return(df)

} 
################################################################################


## teste de aplicacao
 filesList <- list.files(path = "~/Dissertação/DADOS_ANA",pattern="txt$",full.names=T)
# source("PluHidroweb2csv.R")
## teste para um arquivo 
sapply(filesList, function(i) PluHidroweb2csv(filename = i))


