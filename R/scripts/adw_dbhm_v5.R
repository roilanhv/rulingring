## sobrotina IDW (Inversedistance.f)
## /home/hidrometeorologista/DBHM/simulations/vacacai/1km/2005/tutorial_interp_intel10_r0_4np/SOURCE/INTERPOLATE/

## Transcrição da subrotina IDW de fortran para R
#IDW(npi, xi, yi, fi, nc, x , nr,  y, CM, NP, R0, FO, KER)
#IDW(NPI, XI, YI, FI, NXO,XO, NYO, YO,CM, NP, R0, FO, KER)

# rm(list=ls())

## Diretório de trab (pc Felipe)
#setwd("/home/guilherme/DBHM/data_process/atm_data/doc/")

# setwd("/home/felipe/DBHM/data_process/atm_data/doc")
## pacotes necessários
require(raster)
require(lattice)
require(plyr) # usa a função mutate para converter os dados das estações para o SI

# install.packages('printr', type = 'source', repos = c('http://yihui.name/xran', 'http://cran.rstudio.com'))


pcks <- c("knitr", "printr", "R.utils", "magrittr", 
          "lubridate","stringr", "plyr", "dplyr", "lattice", "rasterVis")
invisible(sapply(pcks, require, character.only = TRUE, quietly = TRUE))

# configuraçoes knitr
opts_chunk$set(cache = FALSE)

## NP = número de stns (22)
## 


##############################################################################################
## O que está abaixo (Organização dos dados de precipitação) foi escrito em um novo script 
## para diminuir as informação aqui. Estão em "//home/felipe/DBHM/data_process/atm_data/doc" 
##############################################################################################

# ## A tabela abaixo somente será utilizada para extrair os dados de lon e lat das stns pois os arquivos txt que estão
# ## na pasta gauge_13item não contem essa informação.
# ## lendo arquivo .asc para utilizar as coordenadas de lon e lat em metros e não em graus:
# dados_stns_ll <- read.table("../../../simulations/vacacai/1km/2005/ATM_INPUT_DATA/Interpolate2/AVG_2005_STATION.asc",
#                             header = TRUE)
# 
# 
# ##### O que queremos obter abaixo é um dataframe para que a interpolação seja realizada através do ADW em um dia 
# ## aleatório (04/01/05) para teste. 
# 
# ## os arquivos com os dados das estações (stn_data) retirados abaixo estão em: 
# ## "/home/felipe//DBHM/simulations/vacacai/1km/2005/ATM_INPUT_DATA/gauge_13item"
# 
# ## Criando um vetor de 01:22 (número de stns na bacia hidrográfica do Rio Vacacaí) para ser usado no looping abaixo:
# numb <- paste(0, 1:9, sep = "")
# stn_number <- c(numb,10:22)
# 
# ## Looping para ler e colocar os dados de cada estação hidrometeorológica no SI (arquivos de dados da pasta "gauge_13item").
# ## all_stn_data é uma lista com 22 componentes: cada componente representa uma stn pluviométrica com dados diários.
# ## Neste passo, ainda, são selecionados apenas os dados de precipitação (as outras variáveis meteorológicas são negligenciadas).
# all_stn_data <- 
#   
#   lapply(1: length(stn_number), 
#                       function(i_stn){       
#                              # i_stn = 1
#                              ## Dados diários de cada estação hidrometeorológica para o ano de 2005
#                              stn_data_l <- read.table(paste("../../../simulations/vacacai/1km/2005/ATM_INPUT_DATA/gauge_13item/700",
#                                                       stn_number[i_stn], 
#                                                       ".txt",
#                                                       sep = "")
#                                                       )
#                              stn_data <- stn_data_l[,c(1,2,3,4,15)]
#                              ## cria-se col1,col2,col3... correspondente a cada tipo de informação correspondente 
#                              names(stn_data) <- paste0("col", 1:ncol(stn_data))
#                              head(stn_data)
#     
#                              # Abaixo mostra-se o nome da variável correspondente a cada coluna do arquivo.
# #                              var_names <- c("stn_id", "year","month","day","tm","tmax","tmin","um","umin",
# #                              "n_sum","n_lowm","fsm","fmaxx","fmaxs","rsum","d0m","sun","E01",
# #                              "snow","mslp")
#                              var_names <- c("stn_id", "year","month","day","rsum")
#     
#                              ## Com relação aos dados de Mogu, no Vcc foram removidos os dados de "patm" (última coluna) 
#                              names(stn_data) <- var_names
#                              head(stn_data)
#     
#                              ## Abaixo, mostra-se a conversão das variáveis para o unidades SI.
#     
#                              # dados reais
#                              stn_data_r <- stn_data
#                              # substituição de 32766 por NA
#                              stn_data_r[stn_data_r == 32766] <- NA
#                              # conversão para valores
#                              stn_data_r <- mutate(stn_data_r,
# #                                                   tmax = tmax/10,
# #                                                   tm = tm/10,
# #                                                   tmin = tmin/10,
# #                                                   um = um,
# #                                                   fsm = fsm/10,
# #                                                   sun = sun/10,
#                                                   rsum = rsum/10) 
#                              head(stn_data_r)
#                              tail(stn_data_r)
#      
#                         return(stn_data_r)
#                       } # endfun
#   ) # end lapply
# 
# 
# ## Resumindo: no looping acima foram lidos todos os arquivos com dados diários das 22 estações hidrometeorológicas da Bacia do 
# ## Rio Vacacaí e então esses dados foram convertidos para o SI.
# #####
# 
# ## Neste próximo passo, os dados diários que anteriormente estavam organizados por estações meteorológicas, agora serão 
# ## ordenados por dia. Ou seja, será criada uma lista com sublistas para cada dia e, em cada dia, são armazenados dados das
# ## 22 estações pluviométricas da Bacia do Vacacaí:
# all_daily_data <- 
# 
# lapply(1:nrow(all_stn_data[[1]]), 
#           function(i_day){
#                     # i_day = 1
#                     ldply(1: length(all_stn_data),
#                                function (i_stn){ 
#                                           ## i_stn = 1
#                                           stn_um <- all_stn_data[[i_stn]] 
#                                           # stn_um[234,] # dia aleatório com chuva para o teste (22/8/2005)
#                                           # stn_um[277,] # dia aleatório com chuva para o teste (4/10/2005)
#                                           stn_um[i_day,] # dia aleatório com chuva para o teste (4/01/2005)
#                                 } # end fun i_stn
#                     ) # end lapply daily_data   
#            }   # end fun i_day
# )  # end lapply all_daily_data
# 
# 
# 
# 
# ### Dados de lon e lat em laea anexados à todos os dias para todas as estações e colocando as colunas na mesma ordem do 
# ## arquivo lido que contém os valores médios dos dados dados_stns_ll:
# dados_stns <- lapply(1:length(all_daily_data),
#                          function(i_day){
#                             # i_day = 1
#                              ## colando ao longo das colunas os dados de lon
#                               anex_lon_lat <- cbind(all_daily_data[[i_day]], dados_stns_ll[ ,c("ID", "X", "Y")])
#                                order_data <- anex_lon_lat[ ,c("ID", "year", "month", "day", "X", "Y","rsum")]
#                             
#                                 return(order_data) 
#                          } # end fun
#               ) # end lapply
# 
# #############################
# ## Salvando os dados diários em arquivos separadamente
# ## Os arquivos estão em "/home/felipe/Desktop/data_prec_adw"
# lapply(1:length(dados_stns), FUN = function(iday){ 
#                                                   # iday=1
#                                                   write.table(x = dados_stns[[iday]], 
#                                                               file = paste("../../../../Desktop/data_prec_adw/", "dados_prec_dia_", iday, sep = ""))
#                   }
#        )
##############################################################################################

## Lendo os dados de precipitação do ano de 2005
dados_stns <- lapply(1:365, function(iday){
                                           # iday = 1
                                           read.table(file = paste("data_prec_adw/dados_prec_dia_", 
                                                                   iday, 
                                                                   sep = ""))
                                                      }
                    ) 



## extraindo coordenadas X e Y das estações (lon e lat em laea):
## lon
XI0 <- dados_stns[[1]][,"X"]
## lat
YI0 <- dados_stns[[1]][,"Y"]

## Lista de 365 com a precipitação diária observada em cada estação (nesses dados há NA):
FI0 <- lapply(1:length(dados_stns),
               function(i_day){
                          dados_stns[[i_day]][,"rsum"]
               }
      ) 



####

SA <- lapply(1:length(FI0), function(i){
                                          #i=4
                                          if(sum(is.na(FI0[[i]])) > 0) {
                                          which(is.na(FI0[[i]]))
                                            } else{0}
})



### ESTABELECENDO OS PARÂMETROS DO MÉTODO ADW
## Definindo o número de estações na bacia (22) e o número de estações usadas na interpolação (NP): 
NPI0 <- 22 # nrow(dados_stns)
NP <- 5 # parâmetro de entrada
  
  
# Raster (de referência) da bacia:
r <- raster("/home/felipe/DBHM/simulations/vacacai/1km/2005/tutorial_interp_intel10/GEO_INPUT_DATA/wsdemirr.asc")

## Outros dois parâmetros de entrada são: CM (coeficiente de ajuste) e R0 (raio de influência):
## valores default
CM <- 4
R0 <- 40*res(r)[1] # em metros
# R0 <- 200*res(r)[1] # em metros

# num de colunas
(NX0 <- ncol(r))
# num de linhas
(NY0 <- nrow(r))

# coordenadas da grade
coords_grid <- xyFromCell(r, 1:ncell(r))
X0 <- sort(unique(coords_grid[,"x"]))
Y0 <- sort(unique(coords_grid[,"y"]))


## Looping para interpolação diária da precipitação

## teste de interp com 2 dias: 
# FI <- FI[[1]]

##############################################################################################
####################### PARA PASSAR DE LAEA PARA COORDENADA LON, LAT #########################
##############################################################################################

## Precisamos mudar o extent do raster de laea para lon, lat (faz-se dentro do looping)
# dem90 <- raster("/home/guilherme/DBHM/data_process/geo_data/mdet_vacacai.tif")
# extent(dem90)
# dem_laea0 <- raster("/home/guilherme/DBHM/data_process/geo_data/mdet_vacacai_laea_int.tif")
# dem_ll100 <- projectRaster(dem_laea0, crs = projection(dem90))
# 
# ## ponto central da bacia
# centroid_vcc <- c(lon = -53.80, lat = -30.21)

# centroide de MOGU é diferente
# string com argumentos do sist. coords de ref DBHM
# laea <- "+proj=laea +lat_0=LAT +lon_0=LON +x_0=0 +y_0=0 +ellps=sphere"
# laea_vcc <- gsub("LON", as.character(centroid_vcc[["lon"]]), laea) 
# laea_vcc <- gsub("LAT", as.character(centroid_vcc[["lat"]]), laea_vcc)
# laea_vcc
# # Abaixo é lido o arquivo para extrair as coords lon e lat das stns para serem plotados.
# # No arquivo no começo do script as coords estão em laea
# data_stn <- read.csv("/home/guilherme/Desktop/Felipe/interpolacao/IDWparamsEstimates_2/idw_2params_v2/scripts/dadosDiariosVcc/anaPluInfo.csv")
# coords_stn <- data_stn[, c("Longitude", "Latitude")]
# # Coords da bacia do vcc
# coords_vcc <- read.table("../../../simulations/vacacai/1km/2005/ATM_INPUT_DATA/Interpolate2/vacacai.txt",
#                         header = TRUE)
##projectRaster(prec_interp_ok, crs = projection(dem_ll100))
##############################################################################################

## Exemplo de um ponto de grade (myrow, mycol) 
## obtendo as coordenadas de um ponto de grade dado por linha, coluna:
# myrow <- 20
# mycol <- 110
# mycell <- cellFromRowCol(r,rownr = myrow, colnr = mycol)
# mypoint <- xyFromCell(r, mycell)
# 
# plot(r)
# points(XI0, YI0, pch = 20, col = "dark blue")
# 
# points(mypoint, col = "red", pch = 4, cex = 2)
# text(XI0, YI0+5000, labels = dados_stns$rsum, cex = 0.75)

## plotando somente o ponto da linha 8 de dados_stns com prec de 58mm
# points(XI0[21], YI0[21], col = "red")


## Stack com rasters diários
s <- lapply(1:length(FI0),  
# s <- lapply(1:31,  
                  function(iday){  
                    # iday = 274
                    # iday = 92
                    # iday = 4

## Aqui, para cada dia, é feita a análise dos dados de prec das estações. Os dados faltantes (NA) são retirados
## da série para a interpolação.                    
     if(SA[[iday]][1] != 0){
                            ## Tirando das 22 stns as que tem NA
                            NPI <- NPI0-length(SA[[iday]])
                            ## Tirando do vetor XI as posições que estão guardadas em SA (posições NA)
                            XI <- XI0[-SA[[iday]]]
                            YI <- YI0[-SA[[iday]]]
                            ## FI agora é um vetor para cada dia
                            FI <- FI0[[iday]][-SA[[iday]]]
                            } else{ ## se não... continua os mesmos valores (pois não tem NA nesse dia)
                                    NPI <- NPI0
                                    ## Tirando do vetor XI as posições que estão guardadas em SA (posições NA)
                                    XI <- XI0
                                    YI <- YI0
                                    ## FI agora é um vetor para cada dia
                                    FI <- FI0[[iday]]
        
                              }  ## end if                           
                    
                    
## definição de argumentos do looping:

dis <- rep(NA, NPI)  ## distância dos pontos de grade à cada stn
wk <- rep(NA, NP) ## peso original (do IDW) para cada uma das 5 stns em relação a cada ponto de grade  
sum_wl <- 0        ## soma acumulada do peso de cada stn (desconsiderando a stn k, ou seja, das outro 4 stns mais próx)
ak <- rep(0, NP)     ## parâmetro de correção do método IDW (corrige através do ang entre as stns). Para cada estação, 
                     ## temos um valor de correção (ak)
sum_wk <- 0 ## soma acumulada considerando o peso original e a correção para cada stn para cada stn
id <- vector(mode = "integer",length = NPI) ## identificador das stns 
FO <- matrix(0, nrow = NX0, ncol = NY0) ## matriz com os valores interpolados para cada ponto de grade

## imprimindo o dia
cat("dia", iday,"\n")

for (i in 1:NX0) {  
           # i = 110
           for(j in 1:NY0) {  
                    # j = 20
                       ## Looping para calcular a distância entre cada ponto de grade e a stn k
                       for(k in 1:NPI) {
                         # k = 1
                         id[k] = k
                         disk2 = (XI[k]-X0[i])^2.0 + (YI[k]-Y0[j])^2.0
                         if (disk2 > 0.0) {
                           dis[k] <- sqrt(disk2)
                         } else {
                           dis[k] = 0.0
                         } # endif disk2
                       }# end for (OK!) 
             
             o <- order(dis); dis <- dis[o]; id <- id[o]#; FI[id[k]] <- FI[o]

                      kzero = 0
                      ## Looping para calcular, para cada NP, o peso da distância wk (que é dado por uma função exponencial)
                       for(k in 1:NP) { 
                         # k <- 1
                           if (dis[k] > 0.0) {
                                wk[k] = exp(-CM*dis[k]/R0)
                            } else {
                                wk[k] = 1.0
                                kzero = k
                                 ## Ou seja, se a distância entre o ponto de grade e a stn k for maior que 0, o peso da stn sobre o
                                 ## ponto de grade será: wk[k] = exp(-CM*dis/R0)}, se não o peso será 1 (a stn k está sobre o ponto de grade e 
                                 ## o valor no ponto é o próprio valor observado de prec)
                             }
                        }# end for 
                     # 

if (kzero != 0) { 
            for (k in 1:NP) {
                      if (k != kzero) wk[k] = 0.0
             } # end for
} # end if


## Looping para calcular o ajuste ak de cada stn. (calcula distância entre o ponto de grade e a stn k (b2), o pto de grade e a stn
## m (c2) e entre as stns (a2) e o ângulo que as estações k e m fazem com o ponto de grade)
sum_wk <- 0
for(k in 1:NP) {
          # k = 1
          ak[k] = 0.0
          sum_wl = 0.0
          b2 = dis[k]^2
          
          if (b2 == 0.0) { # somente fará esse if se a dist do ponto de grade à stn k for 0
            ak[k] = 0.0 
          } else {
            mzero <- 0.0
            for(m in 1:NP) {
                # m = 2
                if (m != k) {
                  c2 = dis[m]^2.0
                    if (c2 == 0.0) { ## se c2 == 0 pula o else do c2 (abaixo)
                        mzero <- 1
                     } else {
                        a2 = (XI[id[k]] - XI[id[m]])^2 + (YI[id[k]] - YI[id[m]])^2
                        cos1 = (b2+c2-a2)/2.0/sqrt(b2*c2) # lei dos cossenos (deve ser valor entre -1 e 1)
                        ak[k] = ak[k]+wk[m]*(1-cos1) # entre parênteses deve ser valor entre 0 e 2
                      }# endif c2
                  data.frame(dis = dis[1:5], wk = wk, a2 = a2, b2 = b2)
                  ## soma acumulada do peso de cada stn (desconsiderando a stn k, ou seja, das outro 4 stns mais próx):
                  sum_wl = sum_wl+wk[m]
                } # endif m != k
              } # end for m
              
              if (mzero == 1) {
                ak[k] = 0.0
              } else {
                ak[k] = ak[k]/sum_wl
              }# enf if mzero
          }# endif b2
          
       # Aqui, faz-se a soma dos pesos considerando a distância entre o ponto de grade e a stn k e o 
       # peso considerando o ângulo entre as stns k e m (peso este que é atribuido a stn k)  
      wk[k] = wk[k]*(1.0 + ak[k])
      sum_wk = sum_wk + wk[k]
} # end for

## id_ok é o id atualizado onde: se houver NA em alguma das 5 estações mais próximas do ponto, esta não é utilizada
## para a interpolação... pegando assim a sexta estação mais próxima com dado
     
     for(k in 1:NP) {
           # k = 1
          FO[i,j] = FO[i,j]+FI[id[k]]*wk[k]/sum_wk
        } 


} # end for j
} # end for i

## raster de referência
prec_interp <- raster(r)

## transpondo a matriz FO:
FO_t  <- t(FO)

## atribuindo os valores da interpolação que estão na matriz FO no raster prec_interp
prec_interp_r <- setValues(x = prec_interp, values = FO_t)

## invertendo a ordem das linhas (da última para a primeira):
prec_interp_ok <- flip(prec_interp_r, direction = "y")

####### passando de laea para lon, lat
# projection(prec_interp_ok) <- laea_vcc
# prec_interp_ok1 <- projectRaster(prec_interp_ok, crs = projection(dem_ll100))
# 
# # Plot da grade interpolada em coordenadas (lon,lat), do contorno da bacia do vcc e dos pontos
# # das estações pluviométricas: 
# plot(prec_interp_ok1)
# lines(coords_vcc, lwd = 2, col = "black")
# points(coords_stn, lwd = 2, col = "darkblue", pch = 20)
###### 

## salvando em um arquivo raster
writeRaster(x = prec_interp_ok, 
            filename = paste("rasters_laea31/interp_adw_vcc_dia_", 
                             iday, 
                             sep = ""), 
            overwrite = TRUE)

#rm(prec_interp, FO, r)          

                return(prec_interp_ok) # ????????????
                 
} # end fun 
) # end looping em todos os dias 



##########################################################################################
## TESTE PARA UM MÊS (COMPARACAO ENTRE SCRIP E BIN)
s_ok <- ldply(1:365, function(idata){ 
                                           # idata = 9
                                           list.files(path = "/home/felipe/DBHM/data_process/atm_data/doc/rasters_laea31",
                                           pattern = paste("interp_adw_vcc_dia_",
                                                           idata, 
                                                           ".grd", 
                                                           sep = "")) 
                                           } # end fun
         ) # end ldply
##########################################################################################


## Lendo os arquivos rasters diários listados acima:
daily_raster <- lapply(1:nrow(s_ok), function(i_s){ 
                                                 # i_s = 9
                                                 raster(paste("rasters_laea31/", s_ok[i_s,], sep = ""))  
                                     } # end fun
                      ) # end ldply

## Stack dos rasters diários listados acima (temos 365 layers, um para cada dia). Ou seja, os rasters
## diários são colocados em vários layers (neste caso, 365) dentro de apenas um raster:
daily_raster_s <- stack(daily_raster)

## EXEMPLO: Ponto da stn com dado faltante no dia 01/01/2005:
# plot(daily_raster_s[[1]])
# points(XI[21], YI[21], pch = 20, col = "dark blue")
# points(XI[11], YI[11], pch = 20, col = "dark blue")

## Somando a prec diária de cada dia (layer do raster acima), temos o 
## TOTAL ANUAL DE PRECIPITAÇÃO:
anual_prec <- sum(daily_raster_s
                  # , na.rm = TRUE
                  )

# plot(anual_prec)
# levelplot(anual_prec, main = "Prec anual acumulada (mm)*")


################################
## RASTERS DA INTERPOLAÇÃO (BIN)
################################
## Dados do arquivo binário de precipitação para o ano de 2005 na bacia hidrográfica do Rio Vacacaí, com 
## valores dos parâmetros definidos como: R0 = 40km, NP = 5, Método = 1 (ADW);
wsdem <- raster("../../geo_data/wsdem_vacacai.asc")



prec_values <- readBin("../../../simulations/vacacai/1km/2005/ATM_INPUT_DATA/Interpolate2/2005_rsum_IDW.bin",
                       what = double(),
                       n = ncell(wsdem)*365, # 365 é o num de dias
                       size = 4)
# histogram(prec_values)
## intervalo de variação da prec diária no ano de 2005
range(prec_values)

# construindo brick com campos diários da precipitação
r <- raster(file.path("../../../simulations/vacacai/1km/2005/tutorial_interp_intel10/GEO_INPUT_DATA", "wsdemirr.asc"))
prec_b <- brick(r, nl = 365, values = FALSE) # nl é o numero de dias no ano em 2005

# matriz com valores de todos pontos de grade para cada dia ao longo das colunas
prec_mat <- matrix(prec_values, ncol = 365) ## ncol é o numero de dias no ano em 2005

# atribuindo valores ao brick
## Aqui está a prec diária
prec_b <- setValues(prec_b, prec_mat)

## PRECIPITAÇÂO ANUAL (DADOS QUE ESTAO NO BINARIO)
prec_anual_bin <- sum(prec_b)


old.par <- par(mfrow=c(1, 2))
plot(anual_prec, main = "Prec anual (mm) script")
plot(prec_anual_bin, main = "Prec anual (mm) bin")
par(old.par)

# levelplot(prec_anual_bin, main = "Prec anual acumulada (mm) em 2005 (bin)")











## Diferença entre binário e script: 
levelplot(prec_anual_bin - anual_prec, main = "Diferença entre bin e script")


## FINAL DO SCRIP

## ADW FOI CORRETAMENTE IMPLEMENTADO 
## O que o scrip faz é calcular a interpolação pelo ADW para a bacia do Vacacaí.
## Principais pontos: 
## - Os dados NA para cada dia são retirados antes da interpolação começar a ser realizada;
## - Cada dia terá um número de estações NPI disponíveis para a interpolação neste dia;
## - As 5 estações mais próximas (NP) do ponto de grade são selecionadas para a interpolação;
## - Se uma estação com NA está entre as 5 mais próximas do ponto... esta é removida antes mesmo 
## da interpolação começar. Olhar isso nos scripts Fortran:
# -> Interpolate_rsum.f ()
# -> R_Interpolate_data (este script mostra todas as variáveis utilizadas para iniciar a interpolação);







