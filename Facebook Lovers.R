##############################################################################################
# In this code I want to map out where in the world are my friends who are in relationships. #
# Do you think it's New York?                                                                #
##############################################################################################

# Step 1: Get your Facebook token and ID
# Go to https://developers.facebook.com/tools/explorer and paste your ID and token below:
id <- 16410468
token <- 'CAACEdEose0cBAPlT4J5aY1ZADsLhpeXRNOJhCEUweenRVGvIjLZCFmXQSZBnDkCBBuNZAchhI8BnPkoIlcMTnujkvrrGJpYqNPkCbJAGYt6LZA3wCCx5sKho56NoZB7jp9DwduEVvLqLa4WOetX7UiPN4ULUFNpoZAZBIcBZCe9n3xIMiFJZApZBBLb1wbldh6dHYQZD'

# Step 2: Get a list of my friends and return information about their location and their significant other.
library(RCurl)
library(RJSONIO)
myfriendsrelationshipsurl <- paste0('https://graph.facebook.com/',id,'?fields=id,name,relationship_status,significant_other,friends.fields(name,location,relationship_status,significant_other)&access_token=',token) #When you are doing testing, use your Facebook ID.
myfriendsrelationshipsinfo <- getURL(myfriendsrelationshipsurl)
raw <-fromJSON(myfriendsrelationshipsinfo)

# Step 3: Create a matrix of the location and relationship status of my friends.

okcupid <- function() {
  i <- 1
  j <- 1
  lovers_vec <- matrix(rep('NA',length(raw$friends$data)*2),length(raw$friends$data),2) #The number of rows in the matrix is at most the number of friends I have. We remove the NA columns later.
  for(i in 1:length(raw$friends$data)) {
    if (is.null(raw$friends$data[[i]]$location$id) == TRUE |
        is.null(raw$friends$data[[i]]$relationship_status) == TRUE) {
      i <- i+1
    } else {
      lovers_vec[j,] <- c(raw$friends$data[[i]]$location$name,raw$friends$data[[i]]$relationship_status)
      j <- j+1
      i <- i+1
    }
  }
  return (lovers_vec)
}
okcupid()
rel_vec <- okcupid()
rel_vec2 <- matrix(rel_vec[rel_vec[,1]!="NA"],ncol=2,byrow=F) #This is where we remove the NA columns
rel_vec2

# Step 4 - Map latitude, longitude of each location

#### This script uses RCurl and RJSONIO to download data from Google's API:
#### Latitude, longitude, location type (see explanation at the end), formatted address
#### Notice ther is a limit of 2,500 calls per day

library(RCurl)
library(RJSONIO)
library(plyr)

getGeoCodes <- function(location_vec) {
  url <- function(address, return.call = "json", sensor = "false") { #This is the function to get the URL of each address (city name)
    root <- "http://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
    return(URLencode(u))
  }
  geoCode <- function(address,verbose=FALSE) { #This is the function to get the latitude and longitude of each address
    if(verbose) cat(address,"\n")
    u <- url(address)
    doc <- getURL(u)
    x <- fromJSON(doc)
    if(x$status=="OK") {
      lat <- x$results[[1]]$geometry$location$lat
      lng <- x$results[[1]]$geometry$location$lng
      location_type <- x$results[[1]]$geometry$location_type
      formatted_address <- x$results[[1]]$formatted_address
      return(c(lat,lng))
    } else {
      return(c(NA,NA))
    }
  }
  i <- 1
  lat_long_vec <- matrix(rep('NA',length(rel_vec2)),length(rel_vec2)/2,2)
  for (i in 1:length(location_vec[,1])) {
    lat_long_vec[i,] <- geoCode(location_vec[i,1])
  } 
  return(lat_long_vec)
}

location_vec <- getGeoCodes(rel_vec2)
final_vec <- as.data.frame(cbind(rel_vec2,location_vec))
names(final_vec) <- c('city','relationship_status','lan','lon')
final_vec$V4 <- as.double(final_vec$V4)
final_vec$V3 <- as.double(final_vec$V3)

# Step 5 - Plot in Google Maps

lovers_map <- ggmap(get_googlemap(center = c(lon = 0, lat = 0), zoom=1, maptype='hybrid')) +
  geom_point(data=final_vec,aes(x=V4,y=V3,factor=V2))+ 
  theme(legend.position = "none")
lovers_map

# I'd like to make the points in the world map proportional to the number of friends I have in each city. Could I make the map scrollable?

# Where in the world are the highest % of single friends? (Given a location has at least 5 friends) 