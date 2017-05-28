## pacotes necessários
 library(httr)
 library(plyr)
 library(dplyr)
 library(magrittr)
 library(stringr)

 
#####################################################################
# Set cboTipoReg value according to option selected 
#####################################################################
get_cboTipoReg <- function(option = "Vazao"){
    
    cboTipoReg_values <- c("Cota" = 8
                         ,"Vazao" = 9
                         ,"Chuva" = 10
                         ,"Qualidade" = 12
                         ,"Resumo" = 13
                         ,"Sedimento" = 14
                         ,"Perfil" = 16)
    sel_value <- 
      cboTipoReg_values %>% 
      names() %>% 
      stringr::str_detect(option) %>%
      `[`(cboTipoReg_values, .) %>%
      t() %>%
      c()
    
  return(sel_value)
}
# get_cboTipoReg(option = "Cota")
# get_cboTipoReg(option = "Qualidade")
# get_cboTipoReg(option = "Resumo")
# get_cboTipoReg(option = "Perfil")


## Set zip file name according to option selected 

zip_file_name <- function(opt) {
  
  switch(opt
         ,Cota = "COTAS.ZIP"
         ,Vazao = "VAZOES.ZIP"
         ,Chuva = "CHUVAS.ZIP"
         ,Qualidade = "QUALAGUA.ZIP"
         ,Resumo = "RESUMODESC.ZIP"
         ,Sedimento = "SEDIMENTOS.ZIP"
         ,Perfil = "PERFIL.ZIP")
}

#####################################################################
# extract coords from station
#####################################################################
extract_coords <- function(cont, verbose = FALSE){
#   r <- base_url %>% 
#     stringr::str_replace("XXXXXXXX", stn) %>%
#     httr::POST(body = option_num_l, encode = "form")
#   cont <- r %>% httr::content(as = "text")
  x <- 
    cont %>%
    textConnection() %>%
    readLines() %>%
    stringr::str_replace("<td valign=\"top\">", "") %>%
    stringr::str_replace("</td>", "")
  closeAllConnections()
  
  # lon, lat, alt, area
  lon <- x[grep("Longitude", x) + 1] %>%
    stringr::str_trim() %>%
    stringr::str_split(":") %>%
    unlist() %>%
    as.numeric()
  lon_sinal <- sign(lon[1])
  lon <-  lon %>%
    abs() %>%
    `/`(c(1, 60, 3600)) %>%
    sum() %>%
    `*`(lon_sinal) %>%
    round(7)
  
  lat <- x[grep("Latitude", x) + 1] %>%
    stringr::str_trim() %>%
    stringr::str_split(":") %>%
    unlist() %>%
    as.numeric()
  lat_sinal <- sign(lat[1])
  lat <-  lat %>%
    abs() %>%
    `/`(c(1, 60, 3600)) %>%
    sum() %>%
    `*`(lat_sinal)%>%
    round(7)
  
  alt  <- x[grep("Altitude", x) + 1]
  if(verbose) print(alt)
  alt <- ifelse("-" %in% unlist(stringr::str_split(alt, "")), NA, as.numeric(alt))
  
  adren  <- x[grep("Drenagem", x) + 1] 
  if(verbose) print(adren)
  adren <- ifelse("-" %in% unlist(stringr::str_split(adren, "")), NA, as.numeric(adren))
  
  stn <- x %>% 
    str_detect(fixed("Código")) %>% 
    which() %>% 
    `[`(1) %>% 
    `+`(1) %>%
    `[`(x, .) %>%
    stringr::str_trim()
  
  stn_name <- x %>% 
    str_detect(fixed("Nome")) %>% 
    which() %>% 
    `[`(1) %>% 
    `+`(1) %>%
    `[`(x, .) %>%
    stringr::str_trim()
  
  river <- x %>% 
    str_detect(fixed("Rio")) %>% 
    which() %>% 
    `[`(1) %>% 
    `+`(1) %>%
    `[`(x, .) %>%
    stringr::str_trim()
  
  # dataframe com resultados
  stn_info <- data.frame(codigo = stn,
                         name = stn_name,
                         river = river,
                         lon = lon,
                         lat = lat,
                         alt = alt,
                         area = adren)
  return(stn_info)
}


#####################################################################
## dowload a station data file from hidroweb
#####################################################################
download_file_hidroweb <- function(#stn = "78400000"
                                   stn = "85437000"
                                   ,option = "Chuva"
                                   ,verbose = TRUE
                                   ,dest.dir = "../zips/"){
  
  base_url <-"http://hidroweb.ana.gov.br/Estacao.asp?Codigo=XXXXXXXX&CriaArq=true&TipoArq=1"
  # lista com cboTipoReg para função POST
  option_num_l <- get_cboTipoReg(option) %>% 
    list() %>% 
    setNames(nm = "cboTipoReg")
  # nome do arquivo zip depende de option
  zfile <- zip_file_name(opt = option)
  
  r <- base_url %>% 
    stringr::str_replace("XXXXXXXX", stn) %>%
    httr::POST(body = option_num_l, encode = "form")
  
  if (r$status_code == 200) {
    cont <- r %>% httr::content(as = "text", encoding = "UTF-8")
    # coordendas da estação
    stn_info <- extract_coords(cont)
    # sufixo para base_url
    zip_file_sufix <- cont %>% 
     # gregexpr("ARQ.+/VAZOES.ZIP", .) %>%
      gregexpr(paste0("ARQ.+/", zfile), .) %>%
      regmatches(cont, .) %>%
      unlist()
  } # end if status_code
  
  if(length(zip_file_sufix) > 0){ 
    # apenda zip file a url
    file_url <- zip_file_sufix %>% paste0(dirname(base_url), "/", .)
    # arquivo de destino 
    dest_file <- file_url %>% 
      basename() %>% 
      gsub(zfile, paste0(dest.dir, stn, "_", option, ".zip"), .)
    # download
    file_url %>% download.file(destfile = dest_file, mode = "wb")
    # avisos
    if(file.exists(dest_file)) {
      if(verbose) cat("Arquivo", stn, "salvo com sucesso.\n")
    } else {
      if(verbose) cat("*** Arquivo", stn, "sem dados de Vazão.***\n")
    }
  } else { 
    if(verbose) cat("*** Arquivo", stn, "sem dados de Vazão.***\n")
    dest_file <- NA
  }
  gc()
  out <- mutate(stn_info, file = dest_file)
  return(out)
  
}# end download_file_hidroweb

#####################################################################
## dowload station data files from hidroweb
#####################################################################


hidroweb_downloader <- function(stns = c("78400000", "77140000", "75550000", "74500000")
                                ,options = c("Vazao", "Cota")
                                ,dest.dir = "../zips/"){
  # options and stn combinations
  eg <- expand.grid(options, stns, stringsAsFactors = FALSE) %>% 
    setNames(nm = c("option", "station"))
  
  downloaded_files <- plyr::ldply(1:nrow(eg),
                                   function(ir){
                                     cat(ir, "\n")
                                     # ir = 1
                                     istn <- eg[ir, "station"] 
                                     iopt <- eg[ir, "option"] 
                                     download_file_hidroweb(stn = istn
                                                            ,option = iopt
                                                            ,dest.dir = dir.destino)
                                   }# function()
  )# end ldply
  downloaded_files %>% 
    return()
}

## carregar diretório destino antes de carregar as funções
dir.destino <- "/home/lissette/ANA/"
## Exemplo download 
sj <- hidroweb_downloader(stns = "79400000"
                          ,options = "Vazao"
                          ,dest.dir = dir.destino)

 
 
 