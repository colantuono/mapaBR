mapaBR <- function(df, titulo="Mapa", esquemaCor="Blues") {
  #### Pacotes ####
  require(maptools)
  require(cartography)
  require(tmap)
  require(leaflet)
  require(dplyr)
  require(rgdal)
  require(dplyr)
  require(RColorBrewer)


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
  pal <- colorBin(esquemaCor,domain = NULL,n=5) #cores do mapa

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
