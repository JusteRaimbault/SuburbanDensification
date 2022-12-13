
setwd(paste0(Sys.getenv('CS_HOME'),'/SuburbanDensification/Models/CaseStudySelection'))

library(dplyr)
library(readr)
library(sf)
library(ggplot2)

fua <- st_read(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/FUA_INSEE/metropole_2021/fua_FM_2021/'), 'FUA_FRANCEMETRO_2021')

# pb Strasbourg
# ggplot(fua[fua$fua=='FR009F',])+geom_sf()
# 

dvf2018 <- read_csv(paste0(Sys.getenv('CS_HOME'),'/Data/DVF/DVFGeoloc/2018/full.csv'))
dvf2018sf <- st_as_sf(dvf2018[!is.na(dvf2018$longitude)&!is.na(dvf2018$latitude),], coords=c("longitude","latitude"),crs=4326)
dvf2018sf <- st_transform(dvf2018sf,st_crs(fua))
# export for vis
# st_write(dvf2018sf,dsn=paste0(Sys.getenv('CS_HOME'),'/Data/DVF/DVFGeoloc/2018/'),layer='full_epsg2154',driver='ESRI Shapefile')
dvf2018fua = st_join(dvf2018sf, fua)
rm(dvf2018,dvf2018sf);gc()

# filter - following https://journals.openedition.org/cybergeo/39583
#  - within fuas
dvf2018fua = dvf2018fua[!is.na(dvf2018fua$fua),] %>% filter(nature_mutation=='Vente')

#  - mutation monovente + multivente coherente (meme surface et meme superficie)
#sdvf2018 = dvf2018fua %>% group_by(id_mutation) %>% summarise(count=n(),prix=mean(valeur_fonciere),surface=mean(surface_reelle_bati),deltaprix = max(valeur_fonciere)-min(valeur_fonciere),deltasurface = max(surface_reelle_bati)-min(surface_reelle_bati), fua = libfua[1])
#rm(dvf2018fua);gc()
#sdvf2018 = sdvf2018 %>% filter(!is.na(prix)&!is.na(surface)&deltaprix==0&deltasurface==0)

# too long -> directly on fuas (approx)
sdvf2018 = dvf2018fua %>% group_by(fua) %>% summarise(count=n(), totprix = sum(valeur_fonciere,na.rm = T), totsurface=sum(surface_reelle_bati,na.rm=T))
rm(dvf2018fua);gc()

#ggplot(sdvf2018)+geom_sf(aes(color=count))
# sdvf2018[,c("count","fua")] %>% arrange(count)

dvf2021 <- read_csv(paste0(Sys.getenv('CS_HOME'),'/Data/DVF/DVFGeoloc/2021/full.csv'))
dvf2021sf <- st_as_sf(dvf2021[!is.na(dvf2021$longitude)&!is.na(dvf2021$latitude),], coords=c("longitude","latitude"),crs=4326)
dvf2021sf <- st_transform(dvf2021sf,st_crs(fua))
dvf2021fua = st_join(dvf2021sf, fua)
rm(dvf2021,dvf2021sf);gc()


#dvf2021fua = dvf2021fua[!is.na(dvf2021fua$fua),]
#sdvf2021 = dvf2021fua %>% filter(nature_mutation=='Vente') %>% group_by(id_mutation) %>% summarise(count=n(),prix=mean(valeur_fonciere),surface=mean(surface_reelle_bati),deltaprix = max(valeur_fonciere)-min(valeur_fonciere),deltasurface = max(surface_reelle_bati)-min(surface_reelle_bati), fua = libfua[1]) %>% filter(!is.na(prix)&!is.na(surface)&deltaprix==0&deltasurface==0)
sdvf2021 = dvf2021fua %>% group_by(fua) %>% summarise(count=n(), totprix = sum(valeur_fonciere,na.rm = T), totsurface=sum(surface_reelle_bati,na.rm=T))
rm(dvf2021fua);gc()

# 


all = left_join(as_tibble(sdvf2018),as_tibble(sdvf2021),by=c('fua'='fua'))


# number of transactions growth rate
all$deltatransactions = (all$count.y - all$count.x) / all$count.x

sdvf2018$delta = all$deltatransactions
ggplot(sdvf2018)+geom_sf(aes(color=delta))

deltas = left_join(sdvf2018,data.frame(fua$fua[!duplicated(fua$fua)],fua$libfua[!duplicated(fua$fua)]),by=c('fua'='fua.fua..duplicated.fua.fua..'))

deltas[deltas$delta==min(deltas$delta),]
deltas[deltas$delta<quantile(deltas$delta,c(0.1)),]

deltas[deltas$delta==max(deltas$delta),]
deltas[deltas$delta>quantile(deltas$delta,c(0.9)),]

# comparable in terms of size?
#  Perpignan -0.181 ; 7.7Mrd eur en 2018 ; 21762 transactions (prix moyen 350,000 ) ; pop 2015 : 0.38M
#  Toulouse  1.19 ; 6.5Mrd eur ; 20955 transactions ; pop 2015 : 1.3M
# (Strasbourg : no data : ?)

