##############################################################################################
# In this code I want to map out where in the world are my friends who are in relationships. #
# Do you think it's New York?                                                                #
##############################################################################################

# Step 1: Get your Facebook token and ID
# Go to https://developers.facebook.com/tools/explorer and paste your ID and token below:
id <- 16410468
token <- 'CAACEdEose0cBAMpprfNR0mxV5D5UE6g2yqvXmK3I6MIZCV3JAWRTPuCAHb6oPJ6ZA0z0lvIgESNto4R05pFcs5oYQNaNesSzsbFmkZCcr7D72nZC9XNNmaSwVJKNftQsnOfwAdumz3Jc9MVbLWZCvJhIUMGoprYrVynHZBuDy0jitukJy9FWbGXRNcmpsXyesZD'

# Step 2: Get a list of my friends and return information about their location and their significant other.
library(RCurl)
library(RJSONIO)
myfriendsrelationshipsurl <- paste0('https://graph.facebook.com/',id,'?fields=id,name,relationship_status,significant_other,friends.fields(name,location,relationship_status,significant_other)&access_token=',token) #When you are doing testing, use your Facebook ID.
myfriendsrelationshipsinfo <- getURL(myfriendsrelationshipsurl)
raw <-fromJSON(myfriendsrelationshipsinfo)

# Step 3: Collect the city and relationship status of all my friends
# Note: If someone does not post their relationship status or location, they will be excluded from the collection.

relationshipmapping <- function() {
  i <- 1
  j <- 1
  lovers_vec <- matrix(rep('NA',length(raw$friends$data)*2),length(raw$friends$data),2) #We remove the NA columns later.
  if (nrow(as.data.frame(raw$friends$data[1])) == 1) { #Like before, it is possible data will be in one row.
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
    } else { #If the results are in two columns
      for(i in 1:length(raw$friends$data)) {
        if (is.null(as.data.frame(raw$friends$data[i])["name","location"]) == TRUE |
            is.null(as.data.frame(raw$friends$data[i])["name","relationship_status"]) == TRUE) {
          i <- i+1
        } else {
          lovers_vec[j,] <- c(raw$friends$data[[i]]$location['name'],
                              raw$friends$data[[i]]$relationship_status)
          j <- j+1
          i <- i+1
        }
      }
    }
    return (lovers_vec)
}
relationshipmapping()
rel_vec <- relationshipmapping()
rel_vec2 <- matrix(rel_vec[rel_vec[,1]!="NA"],ncol=2,byrow=F) #This is where we remove the NA columns
rel_vec2

# Step 4 - Map latitude, longitude of each location

#### This script uses RCurl and RJSONIO to download data from Google's API:
#### Latitude, longitude, location type (see explanation at the end), formatted address
#### Notice ther is a limit of 2,500 calls per day

library(RCurl)
library(RJSONIO)
library(plyr)

getGeoCodes <- function(relationship_vec) { #This is the function to get the URL of each address in Google Maps
  url <- function(address, return.call = "json", sensor = "false") {
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
      lat <- x$results[[1]]$geometry$location['lat']
      lng <- x$results[[1]]$geometry$location['lng']
      return(c(lat,lng))
    } else {
      return(c(NA,NA))
    }
  }
  i <- 1
  lat_long_vec <- matrix(rep('NA',length(relationship_vec)*2),nrow=length(relationship_vec),ncol=2)
  for (i in 1:length(relationship_vec)) { #Here's the loop where we collect latitude and longitude
    lat_long_vec[i,] <- geoCode(relationship_vec[i])
    i <- i+1
  } 
  for (i in 1:length(relationship_vec)) { #Unfortunately, there are many NAs (missing data) collected the first time around.
    if(is.na(lat_long_vec[i,1]) == FALSE) { #Repeating this four loop several times is the easiest solution to collect data.
      i <- i+1
    } else {
      lat_long_vec[i,] <- geoCode(relationship_vec[i])
      i <- i+1
    }
  }
  for (i in 1:length(relationship_vec)) {
    if(is.na(lat_long_vec[i,1]) == FALSE) {
      i <- i+1
    } else {
      lat_long_vec[i,] <- geoCode(relationship_vec[i])
      i <- i+1
    }
  }
  for (i in 1:length(relationship_vec)) {
    if(is.na(lat_long_vec[i,1]) == FALSE) {
      i <- i+1
    } else {
      lat_long_vec[i,] <- geoCode(relationship_vec[i])
      i <- i+1
    }
  }
  for (i in 1:length(relationship_vec)) {
    if(is.na(lat_long_vec[i,1]) == FALSE) {
      i <- i+1
    } else {
      lat_long_vec[i,] <- geoCode(relationship_vec[i])
      i <- i+1
    }
  }
  for (i in 1:length(relationship_vec)) {
    if(is.na(lat_long_vec[i,1]) == FALSE) {
      i <- i+1
    } else {
      lat_long_vec[i,] <- geoCode(relationship_vec[i])
      i <- i+1
    }
  }
  for (i in 1:length(relationship_vec)) {
    if(is.na(lat_long_vec[i,1]) == FALSE) {
      i <- i+1
    } else {
      lat_long_vec[i,] <- geoCode(relationship_vec[i])
      i <- i+1
    }
  }
  for (i in 1:length(relationship_vec)) {
    if(is.na(lat_long_vec[i,1]) == FALSE) {
      i <- i+1
    } else {
      lat_long_vec[i,] <- geoCode(relationship_vec[i])
      i <- i+1
    }
  }
  for (i in 1:length(relationship_vec)) {
    if(is.na(lat_long_vec[i,1]) == FALSE) {
      i <- i+1
    } else {
      lat_long_vec[i,] <- geoCode(relationship_vec[i])
      i <- i+1
    }
  }
  for (i in 1:length(relationship_vec)) {
    if(is.na(lat_long_vec[i,1]) == FALSE) {
      i <- i+1
    } else {
      lat_long_vec[i,] <- geoCode(relationship_vec[i])
      i <- i+1
    }
  }
  return(lat_long_vec)
}
location_vec <- getGeoCodes(rel_vec2[,1])
final_vec <- as.data.frame(cbind(rel_vec2,location_vec)) #Combine the relationship vector with the latitude and longitude vector
names(final_vec) <- c('city','relationship_status','lat','lon')
levels(final_vec$relationship_status)
levels(final_vec$city)

# Step 5 - Use Shiny to plot data
install.packages("shiny")
library(shiny)
shiny::runGitHub('shiny_example', 'hahaha88')
https://github.com/jcheng5/leaflet-shiny/tree/master/inst/example