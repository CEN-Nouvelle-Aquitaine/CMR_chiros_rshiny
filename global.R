library(broom)
library(ggplot2)
library(ggthemes)
library(maps)
library(mapdata)
library(viridis)
library(sf)
library(sp)
library(mapview)
library(poptrend)
library(plyr)
library(geosphere)
library(tidyr)
library(tidyverse)
library(wesanderson)
library(ggtext)
library(sfheaders)
library(leaflet)
library(reshape2)
library(leafem)
library(DT)
library(readxl)
library(shiny)
library(formattable)
library(tidyr)
library(shinydashboard)
library(shinycssloaders)
library(RPostgres)
library(pool)
library(DBI)

dbname_id="YOURDBNAMEID"
host_id="YOURHOSTID"
user_id="YOURUSERID"
password="YOURPASSWORD"

pool <- dbPool(
  drv = dbDriver("Postgres"),
  dbname = dbname_id,
  host = host_id,
  user = user_id,
  password = password
)

# Chargement des données 
Data <- dbGetQuery(pool, "SELECT * from public.chiro_test")
# Préprartion des données
Data <- Data[!is.na(Data$ID_PIT),]
Data$ESPECE <- paste0('*', Data$ESPECE, '*')
Data$NOM_LATIN <- Data$ESPECE
Data$ESPECE <- ifelse(Data$ESPECE == "*Rhinolophus ferrumequinum*", "Grand Rhinolophe", as.character(Data$ESPECE))
Data$ESPECE <- ifelse(Data$ESPECE == "*Myotis emarginatus*", "Murin à oreilles échancrées", as.character(Data$ESPECE))
Data$ESPECE <- ifelse(Data$ESPECE == "*Rhinolophus euryale*", "Rhinolophe euryale", as.character(Data$ESPECE))
Data$ESPECE <- ifelse(Data$ESPECE == "*Miniopterus schreibersii*", "Minioptère de Schreibers", as.character(Data$ESPECE))
Data$ESPECE <- ifelse(Data$ESPECE == "*Myotis myotis*", "Grand Murin", as.character(Data$ESPECE))
Data$NOM_FULL <- paste0(Data$ESPECE, ' (', Data$NOM_LATIN, ')')
Data$DATE <- as.Date(Data$DATE , format = "%d/%m/%Y")
Data$LAT_L93 <- as.numeric(Data$LAT_L93)
Data$LONG_L93 <- as.numeric(Data$LONG_L93)



DataT <- subset(Data, Data$ACTION == "T")
a <- as.data.frame(table(DataT$ESPECE,DataT$ANNEE))
a <- spread(a, Var1, Freq)
a_m <- reshape2::melt(a)


names(a)[names(a) == 'Var2'] <- "Année"

# Transposition des données
transposed_data <- a %>%
  pivot_longer(cols = -Année, names_to = "Espèces", values_to = "Count") %>%
  pivot_wider(names_from = Année, values_from = Count) %>%     #ajout d'une colonne "Moyenne" et "evolution" 
  mutate(
    Moyenne = round(rowMeans(
      cbind(`2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`), na.rm = TRUE
    ), 2),
    Evolution = round(ifelse(`2016` != 0, (`2022` - `2016`) / `2016` * 100, NA), 2)
  )



DataC <-  subset(Data, Data$ACTION == "C")
c <- as.data.frame(table(DataC$ESPECE,DataC$ANNEE))
c <- spread(c, Var1, Freq)
names(c)[names(c) == 'Var2'] <- "Année"

# Transposition des données
transposed_data2 <- c %>%
  pivot_longer(cols = -Année, names_to = "Espèces", values_to = "Count") %>%
  pivot_wider(names_from = Année, values_from = Count) %>%     #ajout d'une colonne "Moyenne" et "evolution" 
  mutate(
    Moyenne = round(rowMeans(
      cbind(`2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`), na.rm = TRUE
    ), 2),
    Evolution = round(ifelse(`2016` != 0, (`2022` - `2016`) / `2016` * 100, NA), 2)
  )

b1 <-  spread(as.data.frame(table(table(DataC$ESPECE,DataC$ID_PIT))), Var1, Freq)


DataR <-  subset(Data, Data$ACTION == "R")
d <- as.data.frame(table(DataR$ESPECE,DataR$ANNEE, DataR$COND_REP))


DataT <-  subset(Data, Data$ACTION %in% c("T") )

DataT <- DataT[, c("COMMUNE", "LIEU_DIT", "DEPARTEMENT", "LONG_L93", "ESPECE", "LAT_L93")]
DataT <-dplyr::add_count(DataT, LIEU_DIT)

a <- ddply(DataT, .(LIEU_DIT), summarize, ESPECE=paste(unique(ESPECE), collapse=", "))
DataT <- merge(DataT, a, by="LIEU_DIT", all=T)
names(DataT)[names(DataT) == 'ESPECE.y'] <- 'Espece(s) présente(s)'
DataT <- DataT[ , -which(names(DataT) %in% c("ESPECE.x"))]


DataT <- DataT[!duplicated(DataT[,c("LONG_L93", "LAT_L93")]),]
names(DataT)[names(DataT) == 'n'] <- 'Nombre de transpondages'

coordinates(DataT) <- ~ LONG_L93 + LAT_L93

DataT <- DataT %>% 
  st_as_sf(coords = c("LONG_L93", "LAT_L93"), crs=2154) %>% 
  st_set_crs(2154)

pal <-  mapviewPalette("mapviewSpectralColors")
DataT<-st_transform(DataT, 4326)
MapDataT <- st_as_sf(DataT, coords = c("LONG_L93", "LAT_L93"), crs=4326)

# Extraction des données "latitude" et "longitude" de la colonne "géométrie"
coordinates <- st_coordinates(MapDataT$geometry)
MapDataT$latitude <- coordinates[, 2]
MapDataT$longitude <- coordinates[, 1]




DataR <-  subset(Data, Data$ACTION %in% c("R", "T", "C") )
DataR <-  subset(Data, !(Data$LIEU_DIT %in% c("Blockhaus")) )
DataR <- DataR[, c("DATE", "COMMUNE", "LIEU_DIT", "DEPARTEMENT", "LONG_L93", "LAT_L93", "ESPECE", "ID_PIT" )]
DataR <-DataR[!is.na(DataR$ID_PIT),]
DataR <- DataR[with(DataR, order(ID_PIT, DATE)),]

coordinates(DataR) <- ~ LONG_L93 + LAT_L93

DataR <- DataR %>% 
  st_as_sf(coords = c("LONG_L93", "LAT_L93"), crs=2154) %>% 
  st_set_crs(2154)

DataR<-st_transform(DataR, 4326)

seal_coords <- do.call(rbind, st_geometry(DataR)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
DataR <- cbind(DataR, seal_coords)
DataR_sub <- subset(DataR, duplicated(ID_PIT)|duplicated(ID_PIT, fromLast = TRUE))

DataR_sub <- as.data.frame(DataR_sub) %>%
  group_by(ID_PIT) %>%
  dplyr::mutate(dist = c(NA, geosphere::distVincentyEllipsoid(cbind(lon, lat))/1000))  %>%
  ungroup()

# Remove 0 distance (individus controlés sur le site de marquage)
DataR_subNoZero <- subset(DataR_sub, DataR_sub$dist > 0 & DataR_sub$dist < 500)




a <- unique(subset(Data$LIEU_DIT, Data$ACTION=="T"))

DataR_sub <- DataR_sub[with(DataR_sub, order(ID_PIT, DATE)),]

TrajSite <- DataR_sub

TrajSite <- TrajSite %>% dplyr::group_by(LIEU_DIT) %>% dplyr::mutate(CodeSite = cur_group_id())

TrajSite <- TrajSite[order( TrajSite$ID_PIT, TrajSite$CodeSite),]

TrajSite <- sfheaders::sf_linestring(obj = TrajSite, x = "lon", y = "lat", linestring_id = "ID_PIT", keep = F)

Data_Small <- Data[, c("DATE", "COMMUNE", "LIEU_DIT", "DEPARTEMENT", "ESPECE", "SEXE", "AGE",  "ID_PIT")]

TrajSite <- merge(TrajSite, Data_Small, by="ID_PIT", all=F)


DataSelLines <-  subset(Data, Data$ACTION %in% c("R", "T", "C") )
DataSelLines <-  subset(DataSelLines, !(Data$LIEU_DIT %in% c("Blockhaus")) )
DataSelLines <- DataSelLines[, c("DATE", "COMMUNE", "LIEU_DIT", "DEPARTEMENT", "LONG_L93", "LAT_L93", "ESPECE", "ID_PIT" )]
DataSelLines <-DataSelLines[!is.na(DataSelLines$ID_PIT),]

coordinates(DataSelLines) <- ~ LONG_L93 + LAT_L93

DataSelLines <- DataSelLines %>% 
  st_as_sf(coords = c("LONG_L93", "LAT_L93"), crs=2154) %>% 
  st_set_crs(2154)

DataSelLines<-st_transform(DataSelLines, 4326)

seal_coords <- do.call(rbind, st_geometry(DataSelLines)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
DataSelLines <- cbind(DataSelLines, seal_coords)
DataSelLines <- subset(DataSelLines, duplicated(ID_PIT)|duplicated(ID_PIT, fromLast = TRUE))

DataSelLines <- as.data.frame(DataSelLines) %>%
  group_by(ID_PIT) %>%
  dplyr::mutate(dist = c(NA, geosphere::distVincentyEllipsoid(cbind(lon, lat))/1000))  %>%
  ungroup()

##

DataSelLines <- tibble::rowid_to_column(DataSelLines, "ID")
DataSelLines <- DataSelLines[order(DataSelLines$ID_PIT),]

DataSelLines <- sfheaders::sf_linestring(obj = DataSelLines, x = "lon", y = "lat", linestring_id = "ID_PIT", keep = F)



notificationsIds <- c(notif1 = "#notif1", notif2 = "#notif2", notif3 = "#notif3")
tachesIds <- c(taches1 = "#taches1", taches2 = "#taches2", taches3 = "#taches3")


dbHeader <- dashboardHeader(title = "Tableau de bord Transpondages Chiro",
                            titleWidth = 350,
                            tags$li(a(href = 'https://reseau-cen.org/fr/decouvrir-le-reseau/les-conservatoires',
                                      img(src = 'https://lh3.googleusercontent.com/proxy/asJNUJCpraKzCoyg1GnfGsoX8cUIuWQBMkKTyTLOSlByqa4OIN_3VdbCO4-H_dM27O3nHUJcwIabh73njDIjK2ORVioygN4nNKr9jIQTK5J_Xg',
                                          title = "logo_CEN", height = "30px"),
                                      img(src = 'https://pbs.twimg.com/profile_images/1220300092957241344/NorgrYwT_400x400.jpg',
                                          title = "logo_CEN", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px; align:left"),
                                    class = "dropdown"),
                            dropdownMenu(type = "notifications", badgeStatus = "warning",
                                         notificationItem(icon = icon("users"), status = "info",
                                                          "Coordonnées du site Blockhaus pas à jour",
                                                          href = notificationsIds[["notif1"]]
                                         ),
                                         notificationItem(icon = icon("warning"), status = "danger",
                                                          "Problème de style pour discriminer espèces de chiro sur les cartes leaflet",
                                                          href = notificationsIds[["notif2"]]
                                                          
                                         )
                            ),
                            
                            # Adapter ce menu de notifications de manière à le rendre dynamique et interactif:
                            dropdownMenu(type = "tasks", badgeStatus = "danger",
                                         taskItem(value = 10, color = "aqua",
                                                  "Refactoriser le code",
                                                  href = tachesIds[["taches1"]]
                                                  
                                         ),
                                         taskItem(value = 90, color = "green",
                                                  "Améliorer le design de l'application",
                                                  href = tachesIds[["taches2"]]
                                                  
                                         ),
                                         taskItem(value = 50, color = "yellow",
                                                  "Corriger les bugs plus ou moins mineurs",
                                                  href = tachesIds[["taches3"]]
                                         )
                            ))


