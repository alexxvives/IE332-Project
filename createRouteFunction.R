library(gridExtra)
library(e1071)
library(spaa)
library(igraph)
library(ggmap)
library(RMySQL)


createRoute()


createRoute <- function() {
  ggmap::register_google(key = "AIzaSyCeIq2MLIyLrhWVjDMS6hq85G0pCj4cPnc")
  edgesFile <- 'edges.csv'
  nodesFile <- 'nodes.csv'
  chargerStop <- 0
  
  #Query SQL for user Input and format it
  connection <- dbConnect(MySQL(), user='g1114008', password='Group8!', dbname='g1114008', host='mydb.itap.purdue.edu')
  stmnt <- sprintf("SELECT * FROM Trips WHERE tripID = (SELECT MAX(tripID) FROM Trips)")
  values <- dbGetQuery(connection, stmnt)
  dbDisconnect(connection)
  
  HotelPreference <- values[1, 2]
  FoodPreference <- values[1, 3]
  startCity <- values[1, 4]
  endCity <- values[1, 5]
  ChargerPreference <- values[1, 6]
  MaxWaitTime <- values[1, 7]
  
  if (HotelPreference == 'Yes') {
    HotelPreference = TRUE
  } else {
    HotelPreference = FALSE
  }
  
  if (FoodPreference == 'Yes') {
    FoodPreference = TRUE
  } else {
    FoodPreference = FALSE
  }
  
  if (ChargerPreference == 'Quick') {
    ChargerPreference = 'fast'
  } else if (ChargerPreference == 'Slow') {
    ChargerPreference = 'slow'
  } else {
    ChargerPreference = 'mixed'
  }
  #End Input Section
  
  stationList <- read.csv(file = 'stationList.csv')
  edges <- read.csv(file = edgesFile)
  nodes <- read.csv(file = nodesFile)
  edges <- edges[, 2:ncol(edges)]
  nodes <- nodes[, 2:ncol(nodes)]
  edgesGraph1 <- edges[,c(3, 4, 9)]
  edgesGraph2 <- edges[,c(4, 3, 9)]
  colnames(edgesGraph2) <- c("from", "to", "length")
  edgesGraph <- rbind(edgesGraph1, edgesGraph2)
  edgesDist <- as.matrix(list2dist(edgesGraph))
  
  
  #Convert input city to node
  startNode <- nodes$node.id[which(nodes$name == startCity)]
  endNode <- nodes$node.id[which(nodes$name == endCity)]
  
  #Create data frame with start/end node coordinates
  destPath <- data.frame(matrix(ncol = 2, nrow = 2))
  colnames(destPath) <- c("lon", "lat")
  destPath[1,1] <- nodes$lon[which(nodes$node.id == startNode)]
  destPath[1,2] <- nodes$lat[which(nodes$node.id == startNode)]
  destPath[2,1] <- nodes$lon[which(nodes$node.id == endNode)]
  destPath[2,2] <- nodes$lat[which(nodes$node.id == endNode)]  
  
  #__________________________________________________ROUTE 1 CALC __________________________________________
  
  #Find shortest path given distance matrix, start, and end destination
  z <- allShortestPaths(edgesDist)
  path <- extractPath(z, match(startNode, colnames(edgesDist)), match(endNode, colnames(edgesDist)))
  #Obtain nodes which the user will pass
  nodePath <- rep(0, length(path))
  i <- 1
  while (i<=(length(path))){
    nodePath[i] = as.numeric(colnames(edgesDist)[path[i]])
    i = i+1
  }
  #Citation
  #Find Shortest Paths Between All Nodes In A Directed Graph. (n.d.). 
  #Retrieved from https://www.rdocumentation.org/packages/e1071/versions/1.7-3/topics/allShortestPaths 
  
  #Calculate distance of route
  distance <- 0
  i <- 1
  while (i<=(length(path)-1)){
    distance = distance + edgesDist[path[i], path[i+1]]
    i = i+1
  }
  
  #Check to see if charger is on users' route
  i <- 1
  j <- 1
  chargerStop <- vector()
  while (i<length(nodePath)){
    while (j<=nrow(stationList)){
      if (((nodePath[i] == stationList[j,9]) && (nodePath[i+1] == stationList[j,10])) || ((nodePath[i+1] == stationList[j,9]) && (nodePath[i] == stationList[j,10]))){
        chargerStop[i] = as.numeric(rownames(stationList)[j])
        j = j+1
      }
      else{
        j = j +1
      }
    }
    j=1
    i=i+1
  }
  chargerStop <- as.vector(na.omit(chargerStop))

  #Convert node path to city path
  cityList <- as.matrix(nodes$name)
  i <- 1
  cityPath <- vector()
  while (i <= length(nodePath)){
    cityPath[i] <- cityList[which(nodes$node.id == nodePath[i])]
    i = i+1
  }
  
  #Convert node path to coordinates
  i <- 1
  coordPath <- data.frame(matrix(ncol = 2, nrow = length(nodePath)))
  colnames(coordPath) <- c("lon", "lat")
  while (i <= length(nodePath)){
    coordPath[i,1] <- nodes$lon[which(nodes$node.id == nodePath[i])]
    coordPath[i,2] <- nodes$lat[which(nodes$node.id == nodePath[i])]
    i = i+1
  }
  
  #_____________CONVERT ROUTE 1 DIRECTIONS TO IMAGE _______________
  i <- 1
  dfCity <- data.frame(matrix(ncol = 3, nrow = length(nodePath)))
  colnames(dfCity) <- c("Cities en Route 1", "lon", "lat")
  while (i <= length(nodePath)){
    dfCity[i,1] <- cityList[which(nodes$node.id == nodePath[i])]
    dfCity[i,2] <- nodes$lon[which(nodes$node.id == nodePath[i])]
    dfCity[i,3] <- nodes$lat[which(nodes$node.id == nodePath[i])]
    i = i+1
  }
  
  if (length(chargerStop)>0){
    i <- 1 
    dfChargers <- data.frame(matrix(ncol = 9, nrow = length(chargerStop)))
    colnames(dfChargers) <- c("Address of Charger", "lon", "lat", "# of Fast", "Wait Time for Fast", "# of Slow","Wait Time for Slow", "Hotel Rooms", "Food")
    while (i <= length(chargerStop)){
      dfChargers[i,1] <- revgeocode(as.numeric(c(stationList$lon[chargerStop[i]],stationList$lat[chargerStop[i]])),output="address")
      dfChargers[i,2] <- stationList$lon[chargerStop[i]]
      dfChargers[i,3] <- stationList$lat[chargerStop[i]]
      dfChargers[i,4] <- stationList$numFast[chargerStop[i]]
      dfChargers[i,5] <- stationList$Average.Wait.Fast[chargerStop[i]]
      dfChargers[i,6] <- stationList$numSlow[chargerStop[i]]
      dfChargers[i,7] <- stationList$Average.Wait.Slow[chargerStop[i]]
      dfChargers[i,8] <- stationList$rooms[chargerStop[i]]
      dfChargers[i,9] <- stationList$store[chargerStop[i]]
      i <- i + 1
    }
    png("directions.png", height = 1000, width = 1000)
    grid.arrange(tableGrob(dfCity), tableGrob(dfChargers))
    dev.off()
  }
  
  if (length(chargerStop) == 0){
    png("directions.png", height = 1000, width = 1000)
    grid.table(dfCity)
    dev.off()
  }
  
  
  #__________________________________________________END ROUTE 1 CALC __________________________________________
  
  #___________________________________________CHECK IF ROUTE 1 PREFERENCES WERE MET ____________________________
  
  #Check hotel preference
  i <- 1
  hotelMet <- 0
  while (i <= length(chargerStop)){
    if (HotelPreference == 'no'){
      if (stationList$rooms[chargerStop[i]] > 1){
        hotelMet <- 1
      }
    }
    if (HotelPreference == 'no'){
      if (stationList$rooms[chargerStop[i]] < 1){
        hotelMet <- 1
      }
    }
    i = i+1
  }
  if ((length(chargerStop)==0) && (HotelPreference == 'no')){
    hotelMet <- 1
  }
  
  #Check food preference
  i <- 1
  foodMet <- 0
  while (i <= length(chargerStop)){
    if (FoodPreference == 'yes'){
      if (stationList$store[chargerStop[i]] > 1){
        foodMet <- 1
      }
    }
    if (FoodPreference == 'no'){
      if (stationList$store[chargerStop[i]] < 1){
        foodMet <- 1
      }
    }
    i = i+1
  }
  
  if ((length(chargerStop)==0) && (FoodPreference == 'no')){
    foodMet <- 1
  }
  
  #Check charger preference
  i <- 1
  chargerMet <- 0
  while (i <= length(chargerStop)){
    if (ChargerPreference == 'fast'){
      if (stationList$numFast[chargerStop[i]] > 0){
        chargerMet <- 1
      }
    }
    if (ChargerPreference == 'standard'){
      if (stationList$numSlow[chargerStop[i]] > 0){
        chargerMet <- 1
      }
    }
    if (ChargerPreference == 'mixed'){
      if (length(chargerStop) > 0){
        chargerMet <- 1
      }
    }
    i = i+1
  }
  
  
  #Check wait time preference
  i <- 1
  waitMet <- 1
  while (i <= length(chargerStop)){
    if ((ChargerPreference == 'fast') && (MaxWaitTime <= stationList$Average.Wait.Fast[chargerStop[i]])){
      waitMet <- 0
    }
    if ((ChargerPreference == 'standard') && (MaxWaitTime <= stationList$Average.Wait.Slow[chargerStop[i]])){
      waitMet <- 0
    }
    if ((ChargerPreference == 'mixed') && (MaxWaitTime <= stationList$Average.Wait.Slow[chargerStop[i]])){
      waitMet <- 0
    }
    if ((ChargerPreference == 'mixed') && (MaxWaitTime <= stationList$Average.Wait.Fast[chargerStop[i]])){
      waitMet <- 0
    }
    i = i+1
  }
  
  #__________________________________________________ROUTE 2 CALC __________________________________________
  edgesDist2 <- edgesDist
  nodePath2 = NULL
  coordPath2 = NULL
  eC <- 2
  if ((hotelMet == 0) || (foodMet == 0) || (chargerMet == 0) || (waitMet == 0)){
    while (eC < length(nodePath)){
      #Check if edge can be removed
      bridge <- 1
      while (bridge == 1){
        edgesDist2 <- edgesDist
        edgesDist2[which(rownames(edgesDist)==nodePath[eC-1]), which(colnames(edgesDist)==nodePath[eC])] <- NA
        edgesDist2[which(rownames(edgesDist)==nodePath[eC]), which(colnames(edgesDist)==nodePath[eC-1])] <- NA
        #Extract new path
        z2 <- allShortestPaths(edgesDist2)
        path2 <- extractPath(z2, match(startNode, colnames(edgesDist2)), match(endNode, colnames(edgesDist2)))
        #Calculate distance of route
        checkDist <- 0
        i <- 1
        while (i<=(length(path2)-1)){
          checkDist[i] = edgesDist2[path2[i], path2[i+1]]
          i <- i +1
        }
        if (any(is.na(checkDist))){
          bridge <- 1
          eC <- eC + 1
        }
        else {
          bridge <- 0
        }
        if (eC >= length(nodePath)){
          bridge <- 0
          print("Alternate path not possible")
        }
      }
  
      #Remove edge in order to reroute
      edgesDist2[which(rownames(edgesDist)==nodePath[eC-1]), which(colnames(edgesDist)==nodePath[eC])] <- NA
      edgesDist2[which(rownames(edgesDist)==nodePath[eC]), which(colnames(edgesDist)==nodePath[eC-1])] <- NA
      eC <- eC + 1
      #Extract new path
      z2 <- allShortestPaths(edgesDist2)
      path2 <- extractPath(z2, match(startNode, colnames(edgesDist2)), match(endNode, colnames(edgesDist2)))
      #Obtain nodes which the user will pass
      nodePath2 <- rep(0, length(path2))
      i <- 1
      while (i<=(length(path2))){
        nodePath2[i] = as.numeric(colnames(edgesDist2)[path2[i]])
        i = i+1
      }
      #Convert node path to city path
      i <- 1
      cityPath2 <- vector()
      while (i <= length(nodePath2)){
        cityPath2[i] <- cityList[which(nodes$node.id == nodePath2[i])]
        i = i+1
      }
      #Calculate distance of route
      distance2 <- 0
      i <- 1
      while (i<=(length(path2)-1)){
        distance2 = distance2 + edgesDist2[path2[i], path2[i+1]]
        i = i+1
      }
      #Check to see if charger is on users' route
      i <- 1
      j <- 1
      chargerStop2 <- vector()
      while (i<length(nodePath2)){
        while (j<=nrow(stationList)){
          if (((nodePath2[i] == stationList[j,9]) && (nodePath2[i+1] == stationList[j,10])) || ((nodePath2[i+1] == stationList[j,9]) && (nodePath2[i] == stationList[j,10]))){
            chargerStop2[i] = as.numeric(rownames(stationList)[j])
            j = j+1
          }
          else{
            j = j +1
          }
        }
        j=1
        i=i+1
      }
      chargerStop2 <- as.vector(na.omit(chargerStop2))
      #Convert node path to coordinates
      i <- 1
      coordPath2 <- data.frame(matrix(ncol = 2, nrow = length(nodePath2)))
      colnames(coordPath2) <- c("lon", "lat")
      while (i <= length(nodePath2)){
        coordPath2[i,1] <- nodes$lon[which(nodes$node.id == nodePath2[i])]
        coordPath2[i,2] <- nodes$lat[which(nodes$node.id == nodePath2[i])]
        i = i+1
      }
      #Check to see if route 2 meets user's preference
      i <- 1
      hotelMet <- 0
      foodMet <- 0
      while (i <= length(chargerStop2)){
        #Check hotel preference
        if (HotelPreference == 'yes'){
          if (stationList$rooms[chargerStop2[i]] > 1){
            hotelMet <- 1
          }
        }
        if (HotelPreference == 'no'){
          if (stationList$rooms[chargerStop2[i]] < 1){
            hotelMet <- 1
          }
        }
        #Check food preference
        if (FoodPreference == 'yes'){
          if (stationList$store[chargerStop2[i]] >= 1){
            foodMet <- 1
          }
        }
        if (FoodPreference == 'no'){
          foodMet <- 1
        }
        i = i+1
        if ((eC == length(nodePath)) && ((hotelMet == 0))){
          hotelMet <- 1
          print("Hotel preferences not met")
        }
        if ((eC == length(nodePath)) && ((foodMet == 0))){
          foodMet <- 1
          print("Food preferences not met")
        }
      }
      #Check charger preference
      i <- 1
      chargerMet <- 0
      while (i <= length(chargerStop2)){
        if (ChargerPreference == 'fast'){
          if (stationList$numFast[chargerStop2[i]] > 0){
            chargerMet <- 1
          }
        }
        if (ChargerPreference == 'standard'){
          if (stationList$numSlow[chargerStop2[i]] > 0){
            chargerMet <- 1
          }
        }
        if (ChargerPreference == 'mixed'){
          if (length(chargerStop2) > 0){
            chargerMet <- 1
          }
        }
        i = i+1
        if ((eC == length(nodePath)) && ((chargerMet == 0))){
          foodMet <- 1
          print("Charger type preferences not met")
        }
      }
      #Check wait time preference
      i <- 1
      waitMet <- 1
      while (i <= length(chargerStop2)){
        if ((ChargerPreference == 'fast') && (MaxWaitTime <= stationList$Average.Wait.Fast[chargerStop2[i]])){
          waitMet <- 0
        }
        if ((ChargerPreference == 'standard') && (MaxWaitTime <= stationList$Average.Wait.Slow[chargerStop2[i]])){
          waitMet <- 0
        }
        if ((ChargerPreference == 'mixed') && (MaxWaitTime <= stationList$Average.Wait.Slow[chargerStop2[i]])){
          waitMet <- 0
        }
        if ((ChargerPreference == 'mixed') && (MaxWaitTime <= stationList$Average.Wait.Fast[chargerStop2[i]])){
          waitMet <- 0
        }
        i = i+1
        if ((eC >= length(nodePath)) && ((waitMet == 0))){
          waitMet <- 1
          print("Wait time preferences not met")
        }
      }
  
    }
  }
  
  
  #_____________CONVERT ROUTE 2 DIRECTIONS TO IMAGE _______________
  dfChargers2 <- NULL
  if (!is.null(nodePath2)) {
    
    i <- 1
    dfCity2 <- data.frame(matrix(ncol = 3, nrow = length(nodePath2)))
    colnames(dfCity2) <- c("Cities en Route 2", "lon", "lat")
    while (i <= length(nodePath2)){
      dfCity2[i,1] <- cityList[which(nodes$node.id == nodePath2[i])]
      dfCity2[i,2] <- nodes$lon[which(nodes$node.id == nodePath2[i])]
      dfCity2[i,3] <- nodes$lat[which(nodes$node.id == nodePath2[i])]
      i = i+1
    }
    
    if (length(chargerStop2)>0){
      i <- 1 
      dfChargers2 <- data.frame(matrix(ncol = 9, nrow = length(chargerStop2)))
      colnames(dfChargers2) <- c("Address of Charger", "lon", "lat", "# of Fast","Wait Time for Fast", "# of Slow", "Wait Time for Slow", "Hotel Rooms", "Food")
      while (i <= length(chargerStop2)){
        dfChargers2[i,1] <- revgeocode(as.numeric(c(stationList$lon[chargerStop2[i]],stationList$lat[chargerStop2[i]])),output="address")
        dfChargers2[i,2] <- stationList$lon[chargerStop2[i]]
        dfChargers2[i,3] <- stationList$lat[chargerStop2[i]]
        dfChargers2[i,4] <- stationList$numFast[chargerStop2[i]]
        dfChargers2[i,5] <- stationList$Average.Wait.Fast[chargerStop2[i]]
        dfChargers2[i,6] <- stationList$numSlow[chargerStop2[i]]
        dfChargers2[i,7] <- stationList$Average.Wait.Slow[chargerStop2[i]]
        dfChargers2[i,8] <- stationList$rooms[chargerStop2[i]]
        dfChargers2[i,9] <- stationList$store[chargerStop2[i]]
        i <- i + 1
      }
      png("directions2.png", height = 1000, width = 1000)
      grid.arrange(tableGrob(dfCity2), tableGrob(dfChargers2))
      dev.off()
    }
    
    if (length(chargerStop2) == 0){
      png("directions2.png", height = 1000, width = 1000)
      grid.table(dfCity2)
      dev.off()
    }
    
  }
  
  
  #__________________________________________________END ROUTE #2_______________________________________
  
  #Map the route on a google map for testing
  
  mn.lon <- mean(stationList[,8])
  mn.lat <- mean(stationList[,7])
  
  r <- ggmap(get_googlemap(center = c(lon = mn.lon, lat = mn.lat),
                           zoom = 5, scale = 2,
                           maptype ='terrain',
                           color = 'color'))
  
  
  fr <- r + geom_path(aes(x = lon, y = lat), data = coordPath, size = 2.5, color = 'green') 
  
  if (length(chargerStop)>0){
    fr <- fr + geom_point(aes(x = lon, y = lat), data = dfChargers, size = 1.75, color = 'red')
    fr <- fr + geom_point(aes(x = lon, y = lat), data = destPath, size = 1.75, color = 'blue')
    
  }
  if (!is.null(coordPath2) & !is.null(dfChargers2)) {
    fr <- fr + geom_path(aes(x = lon, y = lat), data = coordPath2, size = 2.5, color = 'yellow') + theme(legend.position="bottom")
    fr <- fr + geom_point(aes(x = lon, y = lat), data = dfChargers2, size = 1.75, color = 'red') 
    fr <- fr + geom_point(aes(x = lon, y = lat), data = destPath, size = 1.75, color = 'blue')
  } else {
    unlink('directions2.png')
  }
  
  ggsave("routes.png")
}
