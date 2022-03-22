mapaBR <- function(df, titulo="Mapa", esquemaCor="Blues", numeroCores=10) {

  # ========================================================================== #
  #                                                                            #
  # sobre: função criada para gerar mapas                                      #
  #                                                                            #
  # data: 08/03/2022 <João Pedro Colantuono>                                   #
  # versão: 08/03/2022                                                         #
  #                                                                            #
  # ========================================================================== #

options(warn = - 1)

  #### Pacotes ####
  if(!require(dplyr)){install.packages("dplyr",repos="http://cran-r.c3sl.ufpr.br");require(dplyr);}
  if(!require(leaflet)){install.packages("leaflet",repos="http://cran-r.c3sl.ufpr.br");require(leaflet);}
  if(!require(RColorBrewer)){install.packages("RColorBrewer",repos="http://cran-r.c3sl.ufpr.br");require(RColorBrewer);}
  if(!require(cartography)){install.packages("cartography",repos="http://cran-r.c3sl.ufpr.br");require(cartography);}
  if(!require(sp)){install.packages("sp",repos="http://cran-r.c3sl.ufpr.br");require(sp);}
  if(!require(tmap)){install.packages("tmap",repos="http://cran-r.c3sl.ufpr.br");require(tmap);}
  if(!require(rgdal)){install.packages("rgdal",repos="http://cran-r.c3sl.ufpr.br");require(rgdal);}

  #### Normalizando o dataset ####
  names(df) <- c('uf', 'total')


  #### Importando shapefile (mapa do Brasil) ####
  shp <- readOGR("Mapa\\.", "BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")

  class(shp)

  #### Importando códigos do IBGE e adicionando ao dataset ####
  ibge <- read.csv("estadosibge.csv", header=T,sep=",", encoding = "UTF-8")

  df <- merge(df,ibge, by.x = "uf", by.y = "UF")

  #### Fazendo a junção entre o dataset e o shapefile ####
  merged_df <- merge(shp,df, by.x = "CD_GEOCUF", by.y = "Codigo.UF")

  #### Tratamento e transformação dos dados ####
  proj4string(merged_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs") # adicionando coordenadas geográficas

  Encoding(merged_df$NM_ESTADO) <- "UTF-8"

  merged_df$total[is.na(merged_df$total)] <- 0 #substituindo NA por 0

  #### Gerando o mapa ####
  # titulo="Mapa"; esquemaCor="Blues"

  pal <- colorBin(esquemaCor,domain = NULL,n=numeroCores) #cores do mapa

  state_popup <- paste0("<strong>Estado: </strong>",
                        merged_df$NM_ESTADO,
                        "<br><strong>Vendas: </strong>",
                        merged_df$total)

  leaflet(data = merged_df) %>%
    # addProviderTiles(providers$CartoDB.Positron) %>%
    # addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
    addTiles() %>%
    addPolygons(fillColor = ~pal(merged_df$total),
                fillOpacity = 0.7,
                color = "#BDBDC3",
                weight = 1,
                popup = state_popup) %>%
    addLegend("bottomright", pal = pal, values = ~merged_df$total,
              title = titulo,
              opacity = 0.5)
}
