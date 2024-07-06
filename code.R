library(dplyr)
library(xlsx)
full_data <- data.frame();
k <- 5
for(year in 2023:2008) {
  print(year)
  setwd( paste0("microdados_ed_basica_",year,"/dados/"))
  
  raw_data <- read.csv2(paste0("microdados_ed_basica_",year,".csv"), sep=";" , dec=".", encoding="latin1")
  
  cities <- unique(raw_data$CO_MUNICIPIO)
  results <- data.frame();
  for(city in cities) {
    
    mun <- raw_data[raw_data$CO_MUNICIPIO == city & raw_data$TP_SITUACAO_FUNCIONAMENTO == 1,]
    escolas <- unique(mun$CO_ENTIDADE)
    
    mun <- mun %>%
      mutate(QT_MAT_SNEE = mun$QT_MAT_BAS - mun$QT_MAT_ESP)
    
    mun$QT_MAT_SNEE[is.na(mun$QT_MAT_SNEE)] <- 0
    mun$QT_MAT_ESP[is.na(mun$QT_MAT_ESP)] <- 0
    
    mun <- mun %>%
      mutate(QT_MAT_ID = abs((mun$QT_MAT_SNEE/sum(mun$QT_MAT_SNEE)) 
                             - (mun$QT_MAT_ESP/sum(mun$QT_MAT_ESP))) )
    
    
    results <- rbind(results, data.frame(unique(mun$NO_MUNICIPIO),
                                         unique(mun$CO_MUNICIPIO),
                                         unique(mun$SG_UF),
                                         sum(mun$QT_MAT_ID)/2 ))
    
  }
  if(year < 2023) {
    full_data <- full_data %>% left_join(results, by = c('unique.mun.CO_MUNICIPIO.' = 'unique.mun.CO_MUNICIPIO.'))
  }
  else
    full_data <- results
}


###MAPS CODE

data <- read.csv("raw-dissimilarity.csv")

states <- unique(temp$UF)

state <- "DF"

temp <- as.data.frame(data)

all_muni <- read_municipality(
  code_muni = state, 
  year= 2019,
  showProgress = FALSE
)

ms <- temp[temp$UF == state,]

dissimilarity <- dplyr::left_join(all_muni, ms, by = c("code_muni" = "CO_MUNICIPIO"))

subtitle <- "São Paulo"
palette <- "Reds"
name <- "Segregation"


a2008 <- ggplot() +
  geom_sf(data=dissimilarity, aes(fill=X2008), color=NA, size=.15) +
  labs(subtitle="2008", size=8) +
  scale_fill_distiller(palette = palette, direction = 1, name = name, limits = c(0,1)) +
  theme_minimal() +
  no_axis  

a2009 <- ggplot() +
  geom_sf(data=dissimilarity, aes(fill=X2009), color=NA, size=.15) +
  labs(subtitle="2009", size=8) +
  scale_fill_distiller(palette = palette, direction = 1, name = name, limits = c(0,1)) +
  theme_minimal() +
  no_axis  

a2010 <- ggplot() +
  geom_sf(data=dissimilarity, aes(fill=X2010), color=NA, size=.15) +
  labs(subtitle="2010", size=8) +
  scale_fill_distiller(palette = palette, direction = 1, name = name, limits = c(0,1)) +
  theme_minimal() +
  no_axis  

a2011 <- ggplot() +
  geom_sf(data=dissimilarity, aes(fill=X2011), color=NA, size=.15) +
  labs(subtitle="2011", size=8) +
  scale_fill_distiller(palette = palette, direction = 1, name = name, limits = c(0,1)) +
  theme_minimal() +
  no_axis  

a2012 <- ggplot() +
  geom_sf(data=dissimilarity, aes(fill=X2012), color=NA, size=.15) +
  labs(subtitle="2012", size=8) +
  scale_fill_distiller(palette = palette, direction = 1, name = name, limits = c(0,1)) +
  theme_minimal() +
  no_axis  

a2013 <- ggplot() +
  geom_sf(data=dissimilarity, aes(fill=X2013), color=NA, size=.15) +
  labs(subtitle="2013", size=8) +
  scale_fill_distiller(palette = palette, direction = 1, name = name, limits = c(0,1)) +
  theme_minimal() +
  no_axis  

a2014 <- ggplot() +
  geom_sf(data=dissimilarity, aes(fill=X2014), color=NA, size=.15) +
  labs(subtitle="2014", size=8) +
  scale_fill_distiller(palette = palette, direction = 1, name = name, limits = c(0,1)) +
  theme_minimal() +
  no_axis  

a2015 <- ggplot() +
  geom_sf(data=dissimilarity, aes(fill=X2015), color=NA, size=.15) +
  labs(subtitle="2015", size=8) +
  scale_fill_distiller(palette = palette, direction = 1, name = name, limits = c(0,1)) +
  theme_minimal() +
  no_axis  

a2016 <- ggplot() +
  geom_sf(data=dissimilarity, aes(fill=X2016), color=NA, size=.15) +
  labs(subtitle="2016", size=8) +
  scale_fill_distiller(palette = palette, direction = 1, name = name, limits = c(0,1)) +
  theme_minimal() +
  no_axis  

a2017 <- ggplot() +
  geom_sf(data=dissimilarity, aes(fill=X2017), color=NA, size=.15) +
  labs(subtitle="2017", size=8) +
  scale_fill_distiller(palette = palette, direction = 1, name = name, limits = c(0,1)) +
  theme_minimal() +
  no_axis  

a2018 <- ggplot() +
  geom_sf(data=dissimilarity, aes(fill=X2018), color=NA, size=.15) +
  labs(subtitle="2018", size=8) +
  scale_fill_distiller(palette = palette, direction = 1, name = name, limits = c(0,1)) +
  theme_minimal() +
  no_axis  

a2019 <- ggplot() +
  geom_sf(data=dissimilarity, aes(fill=X2019), color=NA, size=.15) +
  labs(subtitle="2019", size=8) +
  scale_fill_distiller(palette = palette, direction = 1, name = name, limits = c(0,1)) +
  theme_minimal() +
  no_axis  

a2020 <- ggplot() +
  geom_sf(data=dissimilarity, aes(fill=X2020), color=NA, size=.15) +
  labs(subtitle="2020", size=8) +
  scale_fill_distiller(palette = palette, direction = 1, name = name, limits = c(0,1)) +
  theme_minimal() +
  no_axis  

a2021 <- ggplot() +
  geom_sf(data=dissimilarity, aes(fill=X2021), color=NA, size=.15) +
  labs(subtitle="2021", size=8) +
  scale_fill_distiller(palette = palette, direction = 1, name = name, limits = c(0,1)) +
  theme_minimal() +
  no_axis  

a2022 <- ggplot() +
  geom_sf(data=dissimilarity, aes(fill=X2022), color=NA, size=.15) +
  labs(subtitle="2022", size=8) +
  scale_fill_distiller(palette = palette, direction = 1, name = name, limits = c(0,1)) +
  theme_minimal() +
  no_axis  

a2023 <- ggplot() +
  geom_sf(data=dissimilarity, aes(fill=X2023), color=NA, size=.15) +
  labs(subtitle="2023", size=8) +
  scale_fill_distiller(palette = palette, direction = 1, name = name, limits = c(0,1)) +
  theme_minimal() +
  no_axis  

plot_grid(a2008, a2009, a2010, a2011, a2012, a2013, a2014, a2015, a2016, a2017, a2018, a2019, a2020, a2021, a2022, a2023, labels = NULL, ncol = 4)
