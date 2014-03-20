################################################################################################
# Facebook's Graph API allows you to access all your information about you and your friends.   #
# Let's get started by retrieving some basic information about myself.                         #
################################################################################################

# Get your Facebook token and ID
# Go to https://developers.facebook.com/tools/explorer and paste your ID and token below:
id <- 16410468
token <- 'CAACEdEose0cBAEZA5cuUoPyPFqKNUuSdRdHRrxwZBy3LYP4LFLxMCjgIxZCpx2FhgQKB19SjeDN17zqMekBmiGzWLEZAT86ZBZAt5MZASIER3CDhZCr3IFgBzG51tE3eUqr3vL7Jy17RS1E7DHnND4xxZAiyXzsIasi02QfywZCeg1WCjLB6EyFM6Oy4B9en2LZACUZD'

# Now let's pull this information from the Facebook Graph API into R
library(RCurl)
library(RJSONIO)
mybasicinfourl <- paste0('https://graph.facebook.com/',id,'?fields=id,name,relationship_status,gender,location&access_token=',token)
mybasicinfo <- getURL(mybasicinfourl)
raw <-fromJSON(mybasicinfo)
raw

################################################################################################
# My fiancé's roommate always asks me if I can introduce her to my single, male friends in NYC.#
# In this code I want to return a list of all my Facebook friends who meet these criteria.     #
################################################################################################

# Step 1: Get your Facebook token and ID
# Go to https://developers.facebook.com/tools/explorer and paste your ID and token below:
id <- 16410468
token <- 'CAACEdEose0cBAF18o3LFJGlA8Q9NO5NuRYw9Ai7BgUJU1iwezBTki3mkkPTjbZBuA7BtFJRVoX1uwoUugBqaoW412BXWOKrZBOS3Teso0HQyFUDMFqYNZCveuJ5MdBZCvRz70Tw9sGZBLREs3ZBI2stUMXm9oKg5u2UuYcjkOsjPgkK89yGSqIIZBo3heZBoiCgZD'

# Step 2: Get a list of my friends and return their basic information
myfriendsinfourl <- paste0('https://graph.facebook.com/',id,'?fields=id,name,friends.fields(name,relationship_status,gender,location)&access_token=',token)
myfriendsinfo <- getURL(myfriendsinfourl)
raw <-fromJSON(myfriendsinfo)

# Step 3: Run the function erinspicks to determine who my single male friends in NYC are.
# Note: If someone does not post their relationship status on Facebook, they will be excluded from the list.
erinspicks <- function() {
  i <- 1
  j <- 1
  erin_vec <- vector(mode = "character")
  if(nrow(as.data.frame(raw$friends$data[1])) == 1) { #Facebook's API changes constantly, sometimes the results are one column.
    for(i in 1:length(raw$friends$data)) {
      if (is.null(as.data.frame(raw$friends$data[i])$gender) == TRUE | 
          is.null(as.data.frame(raw$friends$data[i])$location.name) == TRUE | 
          is.null(as.data.frame(raw$friends$data[i])$relationship_status) == TRUE) {
        i <- i+1
      }
      else if (as.data.frame(raw$friends$data[i])$gender == 'male' & 
               as.data.frame(raw$friends$data[i])$location.name == 'New York, New York' & 
               as.data.frame(raw$friends$data[i])$relationship_status == 'Single') {
        erin_vec[j] <- c(raw$friends$data[[i]]$name)
        j <- j+1
        i <- i+1
      }
      else {
        i <- i+1
      }   
    }
  } else { 
    for(i in 1:length(raw$friends$data)) { #If the results are in two columns
      if (is.null(as.data.frame(raw$friends$data[i])["name","gender"]) == TRUE | 
          is.null(as.data.frame(raw$friends$data[i])["name","location"]) == TRUE | 
          is.null(as.data.frame(raw$friends$data[i])["name","relationship_status"]) == TRUE) {
        i <- i+1
      }
      else if (as.data.frame(raw$friends$data[i])["name","gender"] == 'male' &
               as.data.frame(raw$friends$data[i])["name","location"] == 'New York, New York' &
               as.data.frame(raw$friends$data[i])["name","relationship_status"] == 'Single') {
          erin_vec[j] <- c(raw$friends$data[[i]]$name)
          j <- j+1
          i <- i+1
        }
      }
    }
  return(erin_vec)
}
erinspicks()