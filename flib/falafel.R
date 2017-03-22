falafel <- function(query,location,state,apikey) {
  
  gpapi_Mass<- function(query,location,state,apikey) {
  library(stringr)
  library(jsonlite)
  #Split the query object by whitespace
  Lquery <- paste(location,state,sep=",")
  zq <- paste(query, Lquery, sep=",")
  seop <- gsub(" ","+",zq)
  stich<- paste("https://maps.googleapis.com/maps/api/place/textsearch/json?query=",
seop,"&key=",apikey,sep="")
  stich
  }

eox <- gpapi_Mass(query,location,state,apikey)

leox <- lapply(as.list(eox),fromJSON)
ajax <- lapply(leox, function(x) {x$results$formatted_address})
ajid <- lapply(leox, function(x) {x$results$place_id})
ajna <- lapply(leox, function(x) {x$results$name})

clena <- data.table(Address=unlist(ajax),
  Place_ID=unlist(ajid),
  Name=unlist(ajna))

clena <- clena[!duplicated(clena$Place_ID),]
return(clena)
}

#Description:  Falafel takes a query, a vector of place locations 
#(city/town names), a state, and the api key and returns a table
#Consisting of the results from that query in a formatted table.  

#BUGS:  
#Note that currently the table will likely contain duplicates - shred the duplicates
#FIXED.


