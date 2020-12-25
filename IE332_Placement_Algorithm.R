#Packages that need to be installed
#install.packages(c("readxl", "tidyverse", "dplyr", "ggmap", "tictoc", "RMySQL", "ROCR", "e1071", "tree", "caret", "pROC"))
#Files that must be included
#     dot_traffic_2015.txt.gz
#     dot_traffic_stations_2015.txt.gz
#     CityData.xls
#User Input
#The input is an edge network and node network, these files must also be in the folder with this R Script
#     edges - .csv file containing the edges: Example input 'midwest.edges.csv'
#     nodes - .csv file name containg the nodes: Example input 'midwest.nodes.csv'
#     budget - The budget in USD for the project
#No output directly to R, the functions output is as follows
#     Populates SQL to various tables
#     Saves images with the predicted traffic by day of the week
#All included libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(ggmap)
library(tictoc)
library(RMySQL) 
library(ROCR)
library(e1071)
library(tree)
library(caret)
library(pROC)
library(RColorBrewer)

tic()
main('west.edges.csv', 'west.nodes.csv', 50000000)
toc()

main <- function(edges, nodes, budget) {
  #Assumptions made about user behaviors
  pctSlow = .0016 #Percent chance a user stops to use a slow charger i.e. 5/10000
  pctFast = .014 #Percent chance a user stops to use a fast charger i.e. 10/10000
  pctStore = .01 #Percent chance a user stops at a convience store i.e. 1/100
  hotelUseRate = .65 #Average number of rooms in use at a hotel on average
  slowUsage = 24 #Average number of hours a slow charge gets used on a day
  fastUsage = 12 #Average number of charges a fast charger gets used on a day
  ####################################
  
  ###Ordered Routes Function###
  #Part 1: Reading in data & ordering routes based on priority of placing chargers
  #Inputs: Edges in a region, nodes in a region, top 1,000 cities 
  #in USA by population (pre-determined)
  #Outputs: Routes dataframe with all route edges,
  #ordered by a ratio of population to distance (ordered_routes)
  #############################
  orderRoutes <- function() {
    #Input data
    edges <- read.csv(file = edges)
    nodes <- read.csv(file =  nodes)
    
    #Reference population data
    CityData <- read_excel("CityData.xls", sheet = 1, col_names = TRUE, trim_ws = TRUE)
    
    #Finding total population of start & end locations of routes
    #Note: if city name not found in top 1,000 cities reference data, population assigned 0 
    #(due to population not being in top 1,000 or exact syntatical match not identified)
    #Note: if multiple cities with the same name in top 1,000 cities reference data is 
    #found, the population of the highest option is used
    edges <- cbind(edges, total_pop = c(rep(0,nrow(edges))))
    
    for (i in 1:(nrow(edges))){
      from_pop <- if(length(CityData$Population[which(CityData$City == 
                                                      (nodes$name[which(edges$from[i] == 
                                                                        nodes$node.id)]))])>0)
      {CityData$Population[which(CityData$City == (nodes$name[which(edges$from[i] == 
                                                                      nodes$node.id)]))]} else {0}
      to_pop <- if(length(CityData$Population[which(CityData$City == 
                                                    (nodes$name[which(edges$to[i] == 
                                                                      nodes$node.id)]))])>0)
      {CityData$Population[which(CityData$City == (nodes$name[which(edges$to[i] == 
                                                                      nodes$node.id)]))]} else{0}
      edges$total_pop[i] <- from_pop[1] + to_pop[1]
    }
    
    #Creating a standardized measure of population & route distance to compare routes
    edges <- cbind(edges, rating = c(rep(0,nrow(edges))))
    
    for (j in 1:(nrow(edges))){
      edges$rating[j] <- edges$total_pop[j] / edges$length[j]
    }
    
    #Reordering routes based on created ratings
    #Note: routes with high population to distance ratios prioritized
    ordered_routes <- edges %>% arrange(desc(edges$rating))
    ordered_routes <- ordered_routes[,c(2,3,4,5,6,7,8,9,10,11)] 
    
    return(ordered_routes)
  }
  
  ###Traffic Data Extraction Function###
  #This functions combines all traffic data on each indivudal locaiton and averages the amount
  #Inputs: None
  #Outputs: Dataframe containing the traffic average at each locaiton and the coordinates
  ######################################
  trafficDataExtraction <- function() {
    ###Vector with the columns of all the hourly traffic data
    hr.sq1 = paste0("traffic_volume_counted_after_0",0:8,"00_to_0",1:9,"00") 
    hr.sq2 = "traffic_volume_counted_after_0900_to_1000" 
    hr.sq3 = paste0("traffic_volume_counted_after_",10:23,"00_to_",11:24,"00") 
    hr.sqs = c(hr.sq1,hr.sq2,hr.sq3)
    
    ###Extracting data from gzfiles
    traffic_data <- gzfile('dot_traffic_2015.txt.gz')
    station_data <- gzfile('dot_traffic_stations_2015.txt.gz')
    
    ###Assigning data to datasets from the .gz files
    rawDF1 <- read.table(traffic_data, header = TRUE, fill = TRUE, sep = ",")
    rawDF2 <- read.table(station_data, header = TRUE, fill = TRUE, sep = ",")
    
    ###Uses inner join to pair all of the station_id and fips_state_code pairs with infromation from other table
    id.coordinates=unique(rawDF2[,c("station_id","latitude","longitude", "fips_state_code")])
    join.df=merge(rawDF1,id.coordinates,by=c("station_id", "fips_state_code"))
    assign("join.df", join.df, .GlobalEnv)
    
    ###Combines all traffic data on singular locations and eventually sums them
    mean.by.id <- aggregate(join.df[, hr.sqs], by=list(join.df$station_id, join.df$fips_state_code), mean)
    colnames(mean.by.id)[1] <- "station_id"
    colnames(mean.by.id)[2] <- "fips_state_code"
    mean.by.id$total.volume = rowSums(mean.by.id[,2:25])
    
    ###Removes unnesecary information from mean.by.id
    mean.by.id <- mean.by.id[,c("station_id", "fips_state_code", "total.volume")]
    
    ###Removes any duplicates due to direction of traffic or lane and puts latitude into a usable format
    final <- inner_join(mean.by.id, rawDF2, by = c("station_id", "fips_state_code"))
    final <- final[!duplicated(final[,1:2]),]
    final <- final[,c("total.volume", "latitude", "longitude", "fips_state_code","fips_county_code", 
                      "station_location")]
    final[,3] <- final[,3] * -1
    
    return(final)
  }
  
  ####Check if a point is within a given Region####
  #This function accepts a two column vector of latitude longitude
  #coordinates and also all vertices of the given region.  It then
  #returns whether or not that point is within the region
  #################################################
  checkCoordinates <- function(pointX, pointY, shapeX, shapeY) {
    test = sp::point.in.polygon(pointX, pointY, shapeX, shapeY)
    if(test == 0) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  
  ###Average Traffic for a Region Function###
  #This function calculates the average traffic between two given points assuming an approxiamtely
  #20 mile buffer on each side.  The output is simply the average traffic
  ###########################################
  averageTrafficBetweenPoints <- function(startLon, startLat, endLon, endLat, totalDistance) {
    slope <- (endLat - startLat) / (endLon - startLon) #Slope between the two cities
    deg <- atan(slope) #The degree angle of the line
    pct = 10 / totalDistance #The percentage that 10 miles is of the entire distance, this is the buffer used to determine the city buffer
    
    startLon = (1-pct)*startLon + pct * (endLon)
    startLat = (1-pct)*startLat + pct * (endLat)
    endLon = (1-pct)*endLon + pct * (startLon)
    endLat = (1-pct)*endLat + pct * (startLat)
    
    corner1Lon = startLon + sin(deg) * .3
    corner2Lon = startLon - sin(deg) * .3
    corner1Lat = startLat - cos(deg) * .3
    corner2Lat = startLat + cos(deg) * .3
    
    corner3Lon = endLon + sin(deg) * .3
    corner4Lon = endLon - sin(deg) * .3
    corner3Lat = endLat - cos(deg) * .3
    corner4Lat = endLat + cos(deg) * .3
    
    shapeX = c(corner1Lon, corner2Lon, corner3Lon, corner4Lon)
    shapeY = c(corner1Lat, corner2Lat, corner3Lat, corner4Lat)
    count = 0
    sum = 0
    
    for (i in 1:nrow(trafficByLocation)) {
      testPoint = c(trafficByLocation[i, 3], trafficByLocation[i, 2])
      if (checkCoordinates(testPoint[1], testPoint[2], shapeX, shapeY) == TRUE) {
        sum = sum + trafficByLocation[i, 1]
        count = count + 1
      }
    }
    
    if (count == 0) {
      return(0)
    } else {
      return(sum/count)
    }
  }
  
  ###Charger Selection Function###
  #This function calculates the optimal number of chargers for a route and any partnerships
  #Inputs: dailyTraffic, routeLength, budget, startLon, startLat, endLon, endLat
  #Outputs: Matrix where number of Rows are number of charging stations in the region
  #[numFast, numSlow, HotelRooms, ConvienceStore, remainingBudget]
  ################################
  chargerSelection <- function(dailyTraffic, routeLength, budget, startLon, startLat, endLon, endLat, node1, node2) {
    
    maxFast = dailyTraffic * pctFast / 12 #max number of fast chargers is the budget divided by the cost to install a fast charger
    maxSlow = dailyTraffic * pctSlow / 2.4 #max number of standard chargers is the budget divided by the cost to install a standard charger
    stations = floor(routeLength/200) + 1 #Number of stations along the route
    finalStations = NULL
    pct <- 1 / (stations + 1)
    currentPct <- 0
    
    #For loop running through all combinations of chargers
    for(z in 1:stations){
      numFast = 0 #number of fast chargers 
      numSlow = 0 #number of standard chargers
      revenue = 0 #the maximum revenue that will be calculated for the location
      hotelRooms = 0 #number of rooms in the hotel if it was used in optimal solution for max revenue
      store = 0 #binary variable if store partnership was used in optimal solution for max revenue
      for(i in 0:maxFast) {
        for(j in 0:maxSlow) {
          if(budget >= 5000 * j + 50000 * i) {
            if(((((i * fastUsage * 7.5 * 365) + (j * slowUsage * 365)) * 5) - (i * 50000 + j * 5000)) > revenue) {
              numFast = i
              numSlow = j
              revenue = ((numFast * fastUsage * 7.5 * 365) + (numSlow * slowUsage * 365)) * 5 - 50000 * numFast - 5000 * numSlow
            }
          }
        }
      }
      budget = budget - (5000 * numSlow + 50000 * numFast)
      
      #Determining if a store or hotel is approriate after the chargers are selected
      if (numSlow + numFast >= 5) {
        storeRevenue = (dailyTraffic * pctStore * 10 * 365) - 144000
        storeRevenue = storeRevenue * .1
      } else {
        storeRevenue = 0
      }
      hotelRooms = (numSlow + numFast) * 5
      hotelRevenue = (hotelRooms * hotelUseRate * 60 * 365) - (hotelRooms * 10000)
      hotelRevenue = hotelRevenue * .2
      
      if (hotelRevenue > storeRevenue && hotelRevenue > 0) {
        store = 0
      } else if(hotelRevenue < storeRevenue && storeRevenue > 0) {
        hotelRooms = 0
        store = 1
      }
      
      #This is used to set location of the charging stations
      currentPct = currentPct + pct
      
      lon <- (1-currentPct)*startLon + currentPct * (endLon)
      lat <- (1-currentPct)*startLat + currentPct * (endLat)
      
      if (is.null(finalStations) && (numFast + numSlow) > 0) {
        finalStations = c(numFast, numSlow, hotelRooms, store, budget, lat, lon, node1, node2, dailyTraffic)
      } else if ((numFast + numSlow) > 0) {
        finalStations = rbind(finalStations, c(numFast, numSlow, hotelRooms, store, budget, lat, lon, node1, node2, dailyTraffic))
      }
    }
    
    return(finalStations)
    
  }
  
  
  ###5 Year Revenue Calculator###
  #This function calculates the predicted Revenue over a 5 year period
  #Inputs: stationList
  #Outputs: revenue
  ################################
  revenueCalc <- function(stationList) {
    #There will be a loop for each category, fast charger, slow charger, hotel, convience store
    #Fast Charger Loop
    fastRevenue <- 0
    for (i in 1:nrow(stationList)) {
      if (stationList[i,1] > 0) {
        customers <- stationList[i,10] * pctFast
        customersPerFast <- (customers / stationList[i,1])
        if (customersPerFast > 12) {
          customersPerFast = 12
        }
        fastRevenue = fastRevenue + (customersPerFast * stationList[i,1] * 7.5)
      }
    }
    fastRevenue = fastRevenue * 365 * 5
    
    #Slow Charger Loop
    slowRevenue <- 0
    for (i in 1:nrow(stationList)) {
      if (stationList[i,2] > 0) {
        customers <- stationList[i,10] * pctSlow
        customersPerSlow <- (customers / stationList[i,1])
        if (customersPerSlow > 2.4) {
          customersPerSlow = 2.4
        }
        slowRevenue = slowRevenue + (customersPerSlow * stationList[i,2] * 10)
      }
    }
    slowRevenue = slowRevenue * 365 * 5
    
    #Hotel Loop
    hotelRevenue <- 0
    for (i in 1:nrow(stationList)) {
      if (stationList[i,3] > 0) {
        hotelRevenue = hotelRevenue + (stationList[i,3] * hotelUseRate) * 60 * .2
      }
    }
    hotelRevenue = hotelRevenue * 365 * 5
    
    #Store Loop
    storeRevenue <- 0
    for (i in 1:nrow(stationList)) {
      if (stationList[i,4] > 0) {
        storeRevenue = storeRevenue + (stationList[i,10] * pctStore) * 10 * .1
      }
    }
    storeRevenue = storeRevenue * 365 * 5
    
    return(slowRevenue + fastRevenue + hotelRevenue + storeRevenue)
  }
  
  #Wait Time Simulation#
  #Inputs
  #     stationList
  #Outputs
  #     stationList: outputs an updated stationList with average wait times in the last two columns
  waitSimulation <- function(stationList) {
    hourlyAverage = mean(join.df[,14])
    for (i in 15:37) {
      hourlyAverage <- rbind(hourlyAverage, mean(join.df[,i]))
    }
    totalAvg <- sum(hourlyAverage)
    hourlyAverage <- hourlyAverage[,1] / totalAvg
    finalSlow <- 0
    finalFast <- 0
    
    for (i in 1:nrow(stationList)) {
      tempSlow = 0
      tempFast = 0
      for (l in 1:30) {
        fastCustomers = trunc(stationList[i,10] * pctFast)
        slowCustomers = trunc(stationList[i,10] * pctSlow)
        arrivalFast <- sample(0:23, fastCustomers, prob = hourlyAverage, replace = TRUE)
        arrivalSlow <- sample(0:23, slowCustomers, prob = hourlyAverage, replace = TRUE)
        arrivalFast <- sort(arrivalFast)
        arrivalSlow <- sort(arrivalFast)
        
        if (stationList[i, 1] != 0) {
          arrivalCount = 1
          nextAvailable = 0
          hoursWaited = 0
          for (j in 0:23) {
            numHourCustomers = 0
            while (arrivalFast[arrivalCount] <= j & arrivalCount <= fastCustomers) {
              numHourCustomers = numHourCustomers + 1
              arrivalCount = arrivalCount + 1
            }
            if (nextAvailable > j) {
              hoursWaited = hoursWaited + (nextAvailable - j) * numHourCustomers
            }
            nextAvailable = nextAvailable + (numHourCustomers / stationList[i,1]) * 2
          }
          
          if (l == 1) {
            tempFast = ((hoursWaited / fastCustomers)*60)
          } else {
            tempFast = rbind(tempFast, (hoursWaited / fastCustomers)*60)
          }
        } else {
          tempFast = 0
        }
        
        if (stationList[i, 2] != 0) {
          arrivalCount = 1
          nextAvailable = 0
          hoursWaited = 0
          for (j in 0:23) {
            numHourCustomers = 0
            while (arrivalSlow[arrivalCount] <= j & arrivalCount <= slowCustomers) {
              numHourCustomers = numHourCustomers + 1
              arrivalCount = arrivalCount + 1
            }
            if (nextAvailable > j) {
              hoursWaited = hoursWaited + (nextAvailable - j) * numHourCustomers
            }
            nextAvailable = nextAvailable + (numHourCustomers / stationList[i,2]) * 10
          }
          
          if (l == 1) {
            tempSlow = ((hoursWaited / slowCustomers)*60)
          } else {
            tempSlow = rbind(tempFast, (hoursWaited / slowCustomers)*60)
          }
        } else {
          tempSlow = 0
        }
       
      }
      
      if (i == 1) {
        finalFast = trunc(mean(tempFast))
      } else {
        finalFast <- rbind(finalFast, trunc(mean(tempFast)))
      }
      
      if (i == 1) {
        finalSlow = trunc(mean(tempSlow))
      } else {
        finalSlow <- rbind(finalSlow, trunc(mean(tempSlow)))
      }
    }
    stationList <- cbind(stationList, finalFast, finalSlow)
    return(stationList)
  }
  
  ###FUNCTION CALLS AND EXECUTION OF PLACEMENT ALGORITHM###
  orderedRoutes <- orderRoutes()
  trafficByLocation <- trafficDataExtraction()
  startBudget <- budget
  stationList <- NULL #Sets station list to NULL
  ##For loop continues to try and add new stations until the budget is less than $5000
  for(i in 1:nrow(orderedRoutes)) {
    if (budget < 5000) {
      break
    }
    dailyTraffic <- averageTrafficBetweenPoints(orderedRoutes[i, 4], orderedRoutes[i,5], orderedRoutes[i, 6],
                                                orderedRoutes[i,7], orderedRoutes[i,8])
    routeLength <- orderedRoutes[i,8]
    if (is.null(stationList)) {
      newRow = chargerSelection(dailyTraffic, routeLength, budget, orderedRoutes[i, 4], orderedRoutes[i,5], 
                                orderedRoutes[i, 6], orderedRoutes[i,7], orderedRoutes[i,2], orderedRoutes[i,3])
      if (!is.null(newRow)) {
        stationList = newRow
        budget = stationList[5]
      }
    } else {
      newRow = chargerSelection(dailyTraffic, routeLength, budget, orderedRoutes[i, 4], orderedRoutes[i,5], 
                                orderedRoutes[i, 6], orderedRoutes[i,7], orderedRoutes[i,2], orderedRoutes[i,3])
      if (!is.null(newRow)) {
        stationList = rbind(stationList, newRow)
        budget = tail(stationList, 1)
        budget = budget[5]
      }
    }
  }
  
  ###END PLACEMENT ALGORITHM###
  
  ###SIMULATION FOR REVENUE CALCULATION and Wait Time###
  revenue <- revenueCalc(stationList)
  profit <- revenue - startBudget + stationList[nrow(stationList), 5]
  stationList <- waitSimulation(stationList)
  ###END REVENUE CALCULATION###
  
  
  ####################################
  #Creates map with stations and nodes
  #Map saved as a png, called stationMap.png
  #Red dots are nodes and black dots are charging stations
  #Transform stationList into a dataframe
  stationList <- matrix(stationList, ncol = 12)
  stationList <- as.data.frame(stationList)
  colnames(stationList) <- c("numFast", "numSlow", "rooms", "store", "budget", "lat", "lon", "startNode", "endNode", "traffic", "Average Wait Fast", "Average Wait Slow")
  
  #Map the charging locations on a google map for testing
  ggmap::register_google(key = "AIzaSyCeIq2MLIyLrhWVjDMS6hq85G0pCj4cPnc")
  cities <- read.csv(file =  nodes, stringsAsFactors = FALSE)
  
  avg.lon <- mean(orderedRoutes$lon)
  avg.lat <- mean(orderedRoutes$lat)
  
  p <- ggmap(get_googlemap(center = c(lon = avg.lon, lat = avg.lat),
                           zoom = 5, scale = 2,
                           maptype ='terrain',
                           color = 'color'), legend = "none", extent = "device")
  
  
  p + geom_point(aes(x = lon, y = lat), data = stationList, size = 1, color = "red") + 
    theme(legend.position="bottom") + geom_point(aes(x = lon, y = lat), data = cities, size = 1, color = "black")
  
  ggsave("stationmap.png")
  ###End of stationMap.png creation###
  ####################################
  
  ####################################
  #Code for Machine Learning#
  totalPerDay <- rowSums(join.df[,14:37])
  toScale <- join.df[, c(5,39, 40)]
  
  totalPerDay <- cbind(totalPerDay, toScale)
  
  count = 0
  i = 101
  current = totalPerDay[1,]
  monday <- c(0,0,0)
  tuesday <- c(0,0,0)
  wednesday <- c(0,0,0)
  thursday <- c(0,0,0)
  friday <- c(0,0,0)
  saturday<- c(0,0,0)
  sunday <- c(0,0,0)
  while (i < nrow(totalPerDay)) {
    prev = i - 100
    if (totalPerDay[i,3] == totalPerDay[prev, 3]) {
      current = rbind(current, totalPerDay[prev:i,])
    } else {
      count = count + 1
      current <- as.data.frame(current)
      days <- current[1:7,]
      days[1,2] <- 1
      days[2,2] <- 2
      days[3,2] <- 3
      days[4,2] <- 4
      days[5,2] <- 5
      days[6,2] <- 6
      days[7,2] <- 7
      tree <- tree::tree(totalPerDay~.,data=current)
      treePredict <- predict(tree, days)
      monday <- rbind(monday, c(treePredict[1], current[1,3], current[1,4]))
      tuesday <- rbind(tuesday, c(treePredict[2], current[1,3], current[1,4]))
      wednesday <- rbind(wednesday, c(treePredict[3], current[1,3], current[1,4]))
      thursday <- rbind(thursday, c(treePredict[4], current[1,3], current[1,4]))
      friday <- rbind(friday, c(treePredict[5], current[1,3], current[1,4]))
      saturday <- rbind(saturday, c(treePredict[6], current[1,3], current[1,4]))
      sunday <- rbind(sunday, c(treePredict[7], current[1,3], current[1,4]))
      current = totalPerDay[i,]
    }
    i = i + 100
  }
  
  c.lon = -97
  c.lat = 40
  #Mapping Monday Traffic
  #Dividing the traffic into different sections
  colnames(monday) <- c('Daily Traffic Volume', 'lat', 'lon')
  monday <- as.data.frame(monday)
  monday[,3] <- monday[,3] * -1
  noTraffic <- subset(monday, monday[,1] <= 1000)
  variable <- subset(monday, monday[,1] < 55300 & monday[,1] > 1000)
  maxTraffic <- subset(monday , monday[,1] >= 55300)
  
  #Mapping the Traffic Flow
  entireUS <- ggmap(get_googlemap(center = c(lon = c.lon, lat = c.lat), zoom = 4, scale = 2, maptype ='roadmap', color = 'color', region = ".us"), extent = "device")
  entireUS + geom_point(aes(x=lon, y = lat, color = `Daily Traffic Volume`), data = variable, size = .25) + scale_color_gradient2(low = "dark green", mid = "yellow", high = "dark red", midpoint = 25000) +
    geom_point(aes(x= lon, y = lat), data = noTraffic, size = .25, color = "dark green") + geom_point(aes(x= lon, y = lat), data = maxTraffic, size = .25, color = "red")
  ggsave("mondayMap.png")
  
  #TUESDAY
  colnames(tuesday) <- c('Daily Traffic Volume', 'lat', 'lon')
  tuesday <- as.data.frame(tuesday)
  tuesday[,3] <- tuesday[,3] * -1
  noTraffic <- subset(tuesday, tuesday[,1] <= 1000)
  variable <- subset(tuesday, tuesday[,1] < 55300 & tuesday[,1] > 1000)
  maxTraffic <- subset(tuesday , tuesday[,1] >= 55300)
  
  #Mapping the Traffic Flow
  entireUS + geom_point(aes(x=lon, y = lat, color = `Daily Traffic Volume`), data = variable, size = .25) + scale_color_gradient2(low = "dark green", mid = "yellow", high = "dark red", midpoint = 25000) +
    geom_point(aes(x= lon, y = lat), data = noTraffic, size = .25, color = "dark green") + geom_point(aes(x= lon, y = lat), data = maxTraffic, size = .25, color = "red")
  ggsave("tuesdayMap.png")
  
  #WEDNESDAY
  colnames(wednesday) <- c('Daily Traffic Volume', 'lat', 'lon')
  wednesday <- as.data.frame(wednesday)
  wednesday[,3] <- wednesday[,3] * -1
  noTraffic <- subset(wednesday, wednesday[,1] <= 1000)
  variable <- subset(wednesday, wednesday[,1] < 55300 & wednesday[,1] > 1000)
  maxTraffic <- subset(wednesday , wednesday[,1] >= 55300)
  
  #Mapping the Traffic Flow
  entireUS + geom_point(aes(x=lon, y = lat, color = `Daily Traffic Volume`), data = variable, size = .25) + scale_color_gradient2(low = "dark green", mid = "yellow", high = "dark red", midpoint = 25000) +
    geom_point(aes(x= lon, y = lat), data = noTraffic, size = .25, color = "dark green") + geom_point(aes(x= lon, y = lat), data = maxTraffic, size = .25, color = "red")
  ggsave("wednesdayMap.png")
  
  #THURSDAY
  colnames(thursday) <- c('Daily Traffic Volume', 'lat', 'lon')
  thursday <- as.data.frame(thursday)
  thursday[,3] <- thursday[,3] * -1
  noTraffic <- subset(thursday, thursday[,1] <= 1000)
  variable <- subset(thursday, thursday[,1] < 55300 & thursday[,1] > 1000)
  maxTraffic <- subset(thursday , thursday[,1] >= 55300)
  
  #Mapping the Traffic Flow
  entireUS + geom_point(aes(x=lon, y = lat, color = `Daily Traffic Volume`), data = variable, size = .25) + scale_color_gradient2(low = "dark green", mid = "yellow", high = "dark red", midpoint = 25000) +
    geom_point(aes(x= lon, y = lat), data = noTraffic, size = .25, color = "dark green") + geom_point(aes(x= lon, y = lat), data = maxTraffic, size = .25, color = "red")
  ggsave("thursdayMap.png")
  
  #FRIDAY
  colnames(friday) <- c('Daily Traffic Volume', 'lat', 'lon')
  friday <- as.data.frame(friday)
  friday[,3] <- friday[,3] * -1
  noTraffic <- subset(friday, friday[,1] <= 1000)
  variable <- subset(friday, friday[,1] < 55300 & friday[,1] > 1000)
  maxTraffic <- subset(friday , friday[,1] >= 55300)
  
  #Mapping the Traffic Flow
  entireUS + geom_point(aes(x=lon, y = lat, color = `Daily Traffic Volume`), data = variable, size = .25) + scale_color_gradient2(low = "dark green", mid = "yellow", high = "dark red", midpoint = 25000) +
    geom_point(aes(x= lon, y = lat), data = noTraffic, size = .25, color = "dark green") + geom_point(aes(x= lon, y = lat), data = maxTraffic, size = .25, color = "red")
  ggsave("fridayMap.png")
  
  #SATURDAY
  colnames(saturday) <- c('Daily Traffic Volume', 'lat', 'lon')
  saturday <- as.data.frame(saturday)
  saturday[,3] <- saturday[,3] * -1
  noTraffic <- subset(saturday, saturday[,1] <= 1000)
  variable <- subset(saturday, saturday[,1] < 55300 & saturday[,1] > 1000)
  maxTraffic <- subset(saturday , saturday[,1] >= 55300)
  
  #Mapping the Traffic Flow
  entireUS + geom_point(aes(x=lon, y = lat, color = `Daily Traffic Volume`), data = variable, size = .25) + scale_color_gradient2(low = "dark green", mid = "yellow", high = "dark red", midpoint = 25000) +
    geom_point(aes(x= lon, y = lat), data = noTraffic, size = .25, color = "dark green") + geom_point(aes(x= lon, y = lat), data = maxTraffic, size = .25, color = "red")
  ggsave("saturdayMap.png")
  
  #SUNDAY
  colnames(sunday) <- c('Daily Traffic Volume', 'lat', 'lon')
  sunday <- as.data.frame(sunday)
  sunday[,3] <- sunday[,3] * -1
  noTraffic <- subset(sunday, sunday[,1] <= 1000)
  variable <- subset(sunday, sunday[,1] < 55300 & sunday[,1] > 1000)
  maxTraffic <- subset(sunday , sunday[,1] >= 55300)
  
  #Mapping the Traffic Flow
  entireUS + geom_point(aes(x=lon, y = lat, color = `Daily Traffic Volume`), data = variable, size = .25) + scale_color_gradient2(low = "dark green", mid = "yellow", high = "dark red", midpoint = 25000) +
    geom_point(aes(x= lon, y = lat), data = noTraffic, size = .25, color = "dark green") + geom_point(aes(x= lon, y = lat), data = maxTraffic, size = .25, color = "red")
  ggsave("sundayMap.png")
  ###End Traffic History Mapping###
  #################################
  
  #Database Entries#
  locations <- rbind(stationList[, c('lat', 'lon')], cities[, c('lat', 'lon')])
  
  locations<- cbind(locations, 1:nrow(locations))
  colnames(locations) <- c("lat", "lon", "locationID")
  
  connection <- dbConnect(MySQL(), user='g1114008', password='Group8!', dbname='g1114008', host='mydb.itap.purdue.edu')
  #Remove Data from Chargers, Locations, Cities, Hotel, Stores, Charging Data
  
  dbGetQuery(connection, "DELETE FROM Chargers")
  dbGetQuery(connection, "DELETE FROM Cities")
  dbGetQuery(connection, "DELETE FROM Hotel")
  dbGetQuery(connection, "DELETE FROM Locations")
  dbGetQuery(connection, "DELETE FROM Stores")
  #Inserts all locations, chargers and cities
  for (i in 1:nrow(locations)) {
    dbGetQuery(connection, sprintf("INSERT INTO Locations VALUES (%d, %.4f, %.4f)", locations[i,3], locations[i,1], locations[i,2]))
  }
  
  #Inserts all cities
  for (i in 1:nrow(cities)) {
    stmnt <- sprintf("SELECT LocationID FROM Locations WHERE Latitude = %.4f AND Longitude = %.4f", cities[i, 3], cities[i, 2])
    value <- dbGetQuery(connection, stmnt)
    stmnt <- sprintf("INSERT INTO Cities VALUES (%d, '%s')", value[1,1], cities[i, 5])
    dbGetQuery(connection, stmnt)
  }
  
  #Inserts all chargers
  for (i in 1:nrow(stationList)) {
    numFast <- stationList[i, 1]
    numSlow <- stationList[i, 2]
    hotelCharger <- stationList[i, 3] / 5
    roomCharger <- stationList[i, 4] * 5
    if (numFast > 0) {
      for (j in 1:numFast) {
        stmnt <- sprintf("SELECT LocationID FROM Locations WHERE Latitude = %.4f AND Longitude = %.4f", stationList[i, 6], stationList[i, 7])
        value <- dbGetQuery(connection, stmnt)
        if (hotelCharger > 0) {
          stmnt <- sprintf("INSERT INTO Chargers VALUES (%d, 0, '%s', '%s', %d)", value[1,1], 'Hotel', 'Fast', stationList[i, 11])
        } else if (roomCharger > 0) {
          stmnt <- sprintf("INSERT INTO Chargers VALUES (%d, 0, '%s', '%s', %d)", value[1,1], 'Store', 'Fast', stationList[i, 11])
        } else {
          stmnt <- sprintf("INSERT INTO Chargers VALUES (%d, 0, '%s', '%s', %d)", value[1,1], 'None', 'Fast', stationList[i, 11])
        }
        dbGetQuery(connection, stmnt)
      }
    }
    if (numSlow > 0) {
      for (j in 1:numSlow) {
        stmnt <- sprintf("SELECT LocationID FROM Locations WHERE Latitude = %.4f AND Longitude = %.4f", stationList[i, 6], stationList[i, 7])
        value <- dbGetQuery(connection, stmnt)
        if (hotelCharger > 0) {
          stmnt <- sprintf("INSERT INTO Chargers VALUES (%d, 0, '%s', '%s', %d)", value[1,1], 'Hotel', 'Standard', stationList[i, 12])
        } else if (roomCharger > 0) {
          stmnt <- sprintf("INSERT INTO Chargers VALUES (%d, 0, '%s', '%s', %d)", value[1,1], 'Store', 'Standard', stationList[i, 12])
        } else {
          stmnt <- sprintf("INSERT INTO Chargers VALUES (%d, 0, '%s', '%s', %d)", value[1,1], 'None', 'Standard', stationList[i, 12])
        }
        dbGetQuery(connection, stmnt)
      }
    }
  }
  
  #Inserts Hotels
  for (i in 1:nrow(stationList)) {
    if (stationList[i, 3] > 0) {
      stmnt <- sprintf("SELECT LocationID FROM Locations WHERE Latitude = %.4f AND Longitude = %.4f", stationList[i, 6], stationList[i, 7])
      value <- dbGetQuery(connection, stmnt)
      stmnt <- sprintf("INSERT INTO Hotel VALUES (%d, %d)", value[1,1], stationList[i,3])
      dbGetQuery(connection, stmnt)
    }
  }
  
  #Inserts Convience Stores
  for (i in 1:nrow(stationList)) {
    if (stationList[i, 4] > 0) {
      stmnt <- sprintf("SELECT LocationID FROM Locations WHERE Latitude = %.4f AND Longitude = %.4f", stationList[i, 6], stationList[i, 7])
      value <- dbGetQuery(connection, stmnt)
      stmnt <- sprintf("INSERT INTO Stores VALUES (%d)", value[1,1])
      dbGetQuery(connection, stmnt)
    }
  }
  
  dbDisconnect(connection)
  #End of Database Entries#
  
  #Save Station List as a csv
  write.csv(stationList, "stationList.csv")
  
  ### Data Analytics ###
  ## Output is a pdf with three plots (financials, charger type, and partnerships)
  #####################
  
  pdf("plots.pdf")
  
  col <- brewer.pal(7, "BuPu") #color palette
  
  #Financial Graph 
  
  financials <- c(startBudget/1000000, (startBudget/1000000)-(budget/1000000), revenue/1000000)
  
  par(mar=c(7,5,7,4))
  financialsPlot <- barplot(financials, main = "Financial Data", horiz=TRUE, las = 1, names.arg = c("Budget","Cost","Revenue"),col=col, xaxt='n')
  text(financials-4, financialsPlot, paste('$',round(financials, digits = 1),'M', sep=""))
  
  #Charger Type Graph
  
  chargerCounts <- c(sum(stationList$numFast), sum(stationList$numSlow))
  
  par(mar=c(4,9,4,4))
  chargersPlot <- barplot(chargerCounts, main = "Chargers Placed",horiz=TRUE, las = 1, names.arg=c("Fast Chargers","Standard Chargers"), col=col, xaxt='n')
  text(chargerCounts-15, chargersPlot, paste(round(chargerCounts, digits = 1), sep=""))
  
  #Partnerships Graph
  
  partnershipCounts <- c(length(which(stationList$rooms>0)),length(which(stationList$store>0)))
  
  par(mar=c(7,5,7,4))
  partnershipPlot <- barplot(partnershipCounts, main = "Partnership Counts",horiz=TRUE, las = 1, names.arg=c("Hotels","Stores"), col=col, xaxt='n')
  text(partnershipCounts-1, partnershipPlot, paste(partnershipCounts, sep=""))
  
  dev.off()
  #END#
  
  ###
  #Reads nodes and edges files and saves them for use in the createRoute Function#
  edges <- read.csv(file = edges)
  nodes <- read.csv(file =  nodes)
  write.csv(edges, file = "edges.csv")
  write.csv(nodes, file = "nodes.csv")
  ###
}
