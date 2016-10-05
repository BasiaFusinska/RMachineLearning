# Read data
crimeFilePath <- '../Data/Crime/Crimes_-_2001_to_present.csv'
crimeData <- read.csv(crimeFilePath)

# Explore data
head(crimeData)
colnames(crimeData)

head(crimeData[,c('Latitude', 'Longitude')])

# Only data with location
crimeData <- crimeData[!is.na(crimeData$Latitude) & !is.na(crimeData$Longitude),]

# Types of crimes
crimeData$Primary.Type
levels(crimeData$Primary.Type)

# loading the required packages
library(ggplot2)
library(ggmap)

# Get map from Google
map_g <- get_map(location = c(lon = mean(crimeData$Longitude, na.rm=TRUE), lat = mean(crimeData$Latitude, na.rm=TRUE)), zoom = 11,
                maptype = "terrain", scale = 2)
# Display map
ggmap(map_g)

# Display crimes
ggmap(map_g) +
  geom_point(data = crimeData, aes(x = Longitude, y = Latitude, alpha = 0.8), size = 1, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

# Display crimes by type
ggmap(map_g) +
  geom_point(data = crimeData, aes(x = Longitude, y = Latitude, fill = Primary.Type, alpha = 0.8), size = 1, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

# Select only Assault, Burglary, Crim Sexual Assult
crimeTypes <- levels(crimeData$Primary.Type)
crimeTypes
selectedCrimes <- subset(crimeData, Primary.Type %in% c(crimeTypes[2], crimeTypes[4], crimeTypes[6]))

ggmap(map_g) +
  geom_point(data = selectedCrimes, aes(x = Longitude, y = Latitude, fill = Primary.Type, alpha = 0.8), size = 1, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

# Clustering
dataColumns <- selectedCrimes[, c('Longitude', 'Latitude')]
originalData <- selectedCrimes
clusterSize <- 6

clusterResult <- kmeans(dataColumns, clusterSize, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
centers <- as.data.frame(clusterResult$centers)
clusterColours <- factor(clusterResult$cluster)
ggmap(map_g) +
  geom_point(data = originalData, aes(x = Longitude, y = Latitude, alpha = 0.8, color = clusterColours), size = 1) +
  geom_point(data = centers, aes(x = Longitude, y = Latitude, alpha = 0.8), size = 1.5) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

clusterSize <- 10
clusterResult <- kmeans(dataColumns, clusterSize, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
centers <- as.data.frame(clusterResult$centers)
clusterColours <- factor(clusterResult$cluster)
ggmap(map_g) +
  geom_point(data = originalData, aes(x = Longitude, y = Latitude, alpha = 0.8, color = clusterColours), size = 1) +
  geom_point(data = centers, aes(x = Longitude, y = Latitude, alpha = 0.8), size = 1.5) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)


# Clusters using Primary.Type 
selectedCrimes$Primary.Type <- as.double(selectedCrimes$Primary.Type)

dataColumns <- selectedCrimes[, c('Longitude', 'Latitude', 'Primary.Type')]
clusterResult <- kmeans(dataColumns, clusterSize, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
clusterColours <- factor(clusterResult$cluster)
ggmap(map_g) +
  geom_point(data = originalData, aes(x = Longitude, y = Latitude, alpha = 0.8, color = clusterColours), size = 1) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)
