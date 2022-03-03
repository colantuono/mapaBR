# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



MapaBR <- function(df) {
  #### Pacotes ####
  library(maptools)
  library(cartography)
  library(tmap)
  library(leaflet)
  library(dplyr)
  library(rgdal)
  library(dplyr)
  library(RColorBrewer)


  #### Importando dataset ####



  names(df) <- c("uf")

  df <- df %>%
    group_by(uf) %>%
    summarise( total = sum(qtd_itens))

  #### Importando shapefile (mapa do Brasil) ####
  shp <- readOGR("Mapa\\.", "BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")

  class(shp)

  #### Importando códigos do IBGE e adicionando ao dataset ####
  ibge <- read.csv("estadosibge.csv", header=T,sep=",", encoding = "UTF-8")

  df <- merge(df,ibge, by.x = "uf", by.y = "UF")

  #### Fazendo a junção entre o dataset e o shapefile ####
  vendas_uf <- merge(shp,df, by.x = "CD_GEOCUF", by.y = "Codigo.UF")

  #### Tratamento e transformação dos dados ####
  proj4string(vendas_uf) <- CRS("+proj=longlat +datum=WGS84 +no_defs") # adicionando coordenadas geográficas

  Encoding(vendas_uf$NM_ESTADO) <- "UTF-8"

  vendas_uf$total[is.na(vendas_uf$total)] <- 0 #substituindo NA por 0

  #### Gerando o mapa ####
  # display.brewer.all() # mostra as cores disponiveis

  pal <- colorBin("Blues",domain = NULL,n=5) #cores do mapa

  state_popup <- paste0("<strong>Estado: </strong>",
                        vendas_uf$NM_ESTADO,
                        "<br><strong>Vendas: </strong>",
                        vendas_uf$total)

  leaflet(data = vendas_uf) %>%
    # addProviderTiles(providers$CartoDB.Positron) %>%
    # addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
    addTiles() %>%
    addPolygons(fillColor = ~pal(vendas_uf$total),
                fillOpacity = 0.7,
                color = "#BDBDC3",
                weight = 1,
                popup = state_popup) %>%
    addLegend("bottomright", pal = pal, values = ~vendas_uf$total,
              title = "Vendas por UF",
              opacity = 0.5)

}
