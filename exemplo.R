
source(here::here('R', 'mapaBR.R'))

dados <- read.csv('fotos.csv')

dados <- dados %>%
  group_by(uf_estado) %>%
  summarise( total = sum(qtd_itens))

# display.brewer.all() # mostra as cores disponiveis

mapaBR(dados, titulo="Vendas por Estado", esquemaCor="BuPu")
mapaBR(dados)


