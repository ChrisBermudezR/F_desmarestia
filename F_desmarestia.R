if(!require("spocc"))install.packages("spocc")
if(!require("tidyverse"))install.packages("tidyverse")
if(!require("robis"))install.packages("robis")

spp= c("Firoloida desmarestia")
F_desmarestia = spocc::occ(query = c('Firoloida desmarestia', 
                                     'Cyclostrema minutum', 
                                     'Cyclostrema solutum',
                                     'Firola gaimardi',
                                     'Firola lesueurii',
                                     'Firola liguriae',
                                     'Firolella gracilis',
                                     'Firolella vigilans',
                                     'Firoloida aculeata',
                                     'Firoloida blainvilleana',
                                     'Firoloida demarestia',
                                     'Firoloida kowalewskyi',
                                     'Firoloida liguriae',
                                     'Pterotrachea lesueri'), 
                            from = c('gbif', 
                                     'inat',
                                     'bison',
                                     'obis',
                                     'idigbio'), 
limit = 50000)

head(F_desmarestia)

Occurrencias_F_desmarestia=spocc::occ2df(F_desmarestia)

Occurrencias_F_desmarestia_df<-as.data.frame(Occurrencias_F_desmarestia)

Occurrencias_F_desmarestia_df$name<-as.factor(Occurrencias_F_desmarestia_df$name)

write.table(Occurrencias_F_desmarestia_df, "./OccData/F_desmarestia.csv",dec = ".", sep = ",", col.names = TRUE, row.names = FALSE, na="NA")

#####

OccObis<-robis::occurrence(scientificname = c('Firoloida desmarestia',
                                              'Cyclostrema minutum',
                                              'Cyclostrema solutum',
                                              'Firola gaimardi',
                                              'Firola lesueurii',
                                              'Firola liguriae',
                                              'Firolella gracilis',
                                              'Firolella vigilans',
                                              'Firoloida aculeata',
                                              'Firoloida blainvilleana',
                                              'Firoloida demarestia',
                                              'Firoloida kowalewskyi',
                                              'Firoloida liguriae',
                                              'Pterotrachea lesueri'))

map_leaflet(OccObis)

selectObis<-as.data.frame(cbind(OccObis$scientificName,OccObis$decimalLongitude,OccObis$decimalLatitude,OccObis$prov,OccObis$eventDate,OccObis$id))

prov<-rep('Obis',82)

selectObis$prov<-prov
colnames(selectObis)<-c('name','longitude','latitude','date','key','prov')
selectObis<-selectObis[c('name','longitude','latitude','prov','date','key')]
rbind(Occurrencias_F_desmarestia_df,selectObis)
colnames(selectObis)
colnames(Occurrencias_F_desmarestia_df)

##Extraer columnas del Occurrence de Darwin Core
OccObis<-robis::occurrence(scientificname = c('Firoloida desmarestia',
                                              'Cyclostrema minutum',
                                              'Cyclostrema solutum',
                                              'Firola gaimardi',
                                              'Firola lesueurii',
                                              'Firola liguriae',
                                              'Firolella gracilis',
                                              'Firolella vigilans',
                                              'Firoloida aculeata',
                                              'Firoloida blainvilleana',
                                              'Firoloida demarestia',
                                              'Firoloida kowalewskyi',
                                              'Firoloida liguriae',
                                              'Pterotrachea lesueri'),
                           fields=c("scientificName","decimalLongitude","decimalLatitude","eventDate","year","month","day","id"))

write.table(OccObis, "OccObis.csv", na="NA", row.names = FALSE, col.names = TRUE, sep = ",")

OccObis<-read.table("OccObis.csv", sep = ",", dec = ".", header = TRUE)

class(OccObis$year)


##Convertir números a fecha y concatenar las columnas 




#OccObis$date2<-as.Date((OccObis$date), "%y-%m-%d")
OccObis$year<-  as.numeric(format(as.Date(OccObis$year), "%Y"))
OccObis$month<-as.numeric(format(as.Date(OccObis$month), "%m"))
OccObis$day<-as.numeric(format(as.Date(OccObis$day), "%d"))

OccObis$Fecha<-  with(OccObis, paste(format(as.Date(OccObis$year), "%Y"), 
                      format(as.Date(OccObis$month), "%m"), 
                      format(as.Date(OccObis$day), "%d"),
                      sep="-"))

OccObis$Fecha[OccObis$Fecha == 'NA-NA-NA']<-NA
OccObis$Fecha<-as.Date(OccObis$Fecha)
OccObis$prov<-"obis"
colnames(OccObis)<-c("latitude", "longitude", "name", "key", "day", "eventDate ", "month", "year", "Fecha", "prov")
obisT<-as.data.frame(cbind(OccObis$name, OccObis$longitude, OccObis$latitude, OccObis$prov, format(as.Date(OccObis$Fecha), "%Y-%m-%d") , OccObis$key))
colnames(obisT)<-colnames(Occurrencias_F_desmarestia)
  
Occurrencias_F_desmarestia$date<-as.character(Occurrencias_F_desmarestia$date)


total<-rbind(obisT, Occurrencias_F_desmarestia)
total$date<-as.Date(total$date)
class(total$date)
colnames(total)<-c("name", "decimalLongitude", "decimalLatitude", "prov", "day", "key")

total$decimalLatitude<-as.numeric(total$decimalLatitude)
total$decimalLongitude<-as.numeric(total$decimalLongitude)


total_info<-duplicated(total)
total_Sin_Dupli<-total[!total_info,]

map_leaflet(total_Sin_Dupli)
write.table(total, "Fdesmarestia_Occ_totales.csv", col.names = TRUE, row.names = FALSE, sep=",", dec=".")
