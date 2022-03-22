library(dplyr)

ibge <- read.csv("estadosibge.csv", header=T,sep=",", encoding = "UTF-8")
d <- read.csv('df.csv')

d <- d %>%
  group_by(uf_estado) %>%
  summarise( total = sum(qtd_itens))

names(d) <- c('UF', 'total')

d <- left_join(d, ibge, by ='UF')

# display.brewer.all(n=10, exact.n=FALSE) # colors

source(here::here('R', 'mapaBR.R'))
mapaBR(d)
mapaBR(d, titulo = "Vendas por Estado")
mapaBR(d, titulo = "Vendas por Estado", esquemaCor = 'YlGnBu')
mapaBR(d, titulo = "Vendas por Estado", esquemaCor = 'YlGnBu', numeroCores = 10)

