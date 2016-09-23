# loading the required packages
library(ggplot2)
library(ggmap)

crimeFilePath <- '../Data/Crime/Crimes_-_2001_to_present.csv'
crimeData <- read.csv(crimeFilePath)

crimeData <- crimeData[!is.na(crimeData$Latitude) & !is.na(crimeData$Longitude),]

mapgilbert4 <- get_map(location = c(lon = mean(crimeData$Longitude, na.rm=TRUE), lat = mean(crimeData$Latitude, na.rm=TRUE)), zoom = 4,
                      maptype = "terrain", scale = 2)

mapgilbert14 <- get_map(location = c(lon = mean(crimeData$Longitude, na.rm=TRUE), lat = mean(crimeData$Latitude, na.rm=TRUE)), zoom = 14,
                      maptype = "terrain", scale = 2)

mapgilbert10 <- get_map(location = c(lon = mean(crimeData$Longitude, na.rm=TRUE), lat = mean(crimeData$Latitude, na.rm=TRUE)), zoom = 10,
                        maptype = "terrain", scale = 2)

mapgilbert12 <- get_map(location = c(lon = mean(crimeData$Longitude, na.rm=TRUE), lat = mean(crimeData$Latitude, na.rm=TRUE)), zoom = 12,
                        maptype = "terrain", scale = 2)

mapgilbert11 <- get_map(location = c(lon = mean(crimeData$Longitude, na.rm=TRUE), lat = mean(crimeData$Latitude, na.rm=TRUE)), zoom = 11,
                        maptype = "terrain", scale = 2)

loMin <- min(crimeData$Longitude)
loMax <- max(crimeData$Longitude)
laMin <- min(crimeData$Latitude)
laMax <- max(crimeData$Latitude)

ggmap(mapgilbert4)
ggmap(mapgilbert14)
ggmap(mapgilbert12)

ggmap(mapgilbert10)+
  scale_x_continuous(limits = c(loMin, loMax), expand = c(0, 0)) +
  scale_y_continuous(limits = c(laMin, laMax), expand = c(0, 0))

ggmap(mapgilbert12) +
  geom_point(data = crimeData, aes(x = Longitude, y = Latitude, fill = Primary.Type, alpha = 0.8), size = 1, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

crimeData$Primary.Type
crimeTypes <- levels(crimeData$Primary.Type)
crimeTypes

accomodationCrimes <- subset(crimeData, Primary.Type %in% c(crimeTypes[2], crimeTypes[4], crimeTypes[6]))

ggmap(mapgilbert12) +
  geom_point(data = accomodationCrimes, aes(x = Longitude, y = Latitude, alpha = 0.8, color = Primary.Type), size = 1) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

results <- kmeans(accomodationCrimes[, c('Longitude', 'Latitude')], 6, 
                  algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
results

results <- kmeans(accomodationCrimes[, c('Longitude', 'Latitude')], 20, 
                  algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))

ac1 <- accomodationCrimes[!is.na(accomodationCrimes$Primary.Type),]
ac1$Primary.Type <- as.double(ac1$Primary.Type)
ac1$Domestic <- as.double(ac1$Domestic)
ac1$Arrest <- as.double(ac1$Arrest)

results <- kmeans(ac1[, c('Longitude', 'Latitude', 'Primary.Type')], 10, 
                  algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))

centers <- as.data.frame(results$centers)

colours <- factor(results$cluster + 1)

ggmap(mapgilbert11) +
  geom_point(data = accomodationCrimes, aes(x = Longitude, y = Latitude, alpha = 0.8, color = colours), size = 1) +
  geom_point(data = centers, aes(x = Longitude, y = Latitude, alpha = 0.8), size = 1.5) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

ggmap(mapgilbert11) +
  geom_point(data = ac1, aes(x = Longitude, y = Latitude, alpha = 0.8, color = colours), size = 1) +
  geom_point(data = centers, aes(x = Longitude, y = Latitude, alpha = 0.8), size = 1.5) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)
