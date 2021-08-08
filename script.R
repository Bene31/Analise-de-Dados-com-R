##################################################################################
#                                 AULA 1                                         #
##################################################################################

pacotes <- c("rgdal", "sf", "crul", "raster", "geobr", "fields", "ggplot2", "ggspatial")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


cars


ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point(col = 'red', size = 4)


##################################################################################
#                                 AULA 2                                         #
##################################################################################

setwd('C:/Users/Arthur/Desktop/semana analise de dados com r/mapa_temperatura')

dados.temp <- read.csv('dados/dados_temperatura.csv')

relevo.mg <- raster('dados/relevo_minas_gerais.tif')

mg <- read_state(code_state = 'MG')

plot(mg$geom) 

modelo <- lm(formula = temp ~ lon + lat + alt, data = dados.temp) # modelo linear

#Estatisticas
summary(modelo)

plot(relevo.mg)

#Converter os dados para um data frame
relevo.df <- as.data.frame(relevo.mg, xy = T)

#Omitir os valores N/A
relevo.df <- na.omit(relevo.df)

#Renomear as colunas
names(relevo.df) <- c("lon", "lat", "alt")

#Calcular o tempo
relevo.df$temp <- 23.49 - 0.25 * relevo.df$lon + 0.48 * relevo.df$lat - 0.0053 * relevo.df$alt

View(relevo.df)

#Calcular o tempo com a função predict
relevo.df$temp2 <-predict(modelo, relevo.df)

##################################################################################
#                                 AULA 3                                         #
##################################################################################

#Plotando o gráfico
ggplot(relevo.df) + 
  geom_raster(aes(x = lon, y = lat, fill = temp)) +
  geom_sf(data = mg, fill = 'NA') +
  scale_fill_gradientn(colors = tim.colors(10)) +
  annotation_scale() +
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  labs(x = NULL, y = NULL, fill = '[ºC]', title = 'Temperatura do ar') +
  theme_minimal()
