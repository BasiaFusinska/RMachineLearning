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
install.packages('ggmap')
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

# Select only Assault, Burglary types
crimeTypes <- levels(crimeData$Primary.Type)
crimeTypes
selectedCrimes <- subset(crimeData, Primary.Type %in% c(crimeTypes[2], crimeTypes[4]))

ggmap(map_g) +
  geom_point(data = selectedCrimes, aes(x = Longitude, y = Latitude, fill = Primary.Type, alpha = 0.8), size = 1, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

# Clustering - kmeans (k = 6)
clusterResult <- kmeans(selectedCrimes[, c('Longitude', 'Latitude')], 6)

# Get cluster information
centers <- as.data.frame(clusterResult$centers)
clusterColours <- factor(clusterResult$cluster)

ggmap(map_g) +
  geom_point(data = selectedCrimes, aes(x = Longitude, y = Latitude, alpha = 0.8, color = clusterColours), size = 1) +
  geom_point(data = centers, aes(x = Longitude, y = Latitude, alpha = 0.8), size = 1.5) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)
