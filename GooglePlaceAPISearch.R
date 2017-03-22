#Adding to Fwrap

#Stabb - state abbreviation is a string - passed through to resclean
GPAPI <- function(query, apikey) { 
  gpapi_Mass<- function(query, apikey) {
    seop <- gsub(" ","+",query)
    stich<- paste("https://maps.googleapis.com/maps/api/place/textsearch/json?query=",
                  seop,"&key=",apikey,sep="")
    stich
  }
  
  eox <- lapply(as.list(query),gpapi_Mass, apikey=apikey)
  library(jsonlite)
  
  leox <- lapply(as.list(eox),fromJSON)
  ajax <- lapply(leox, function(x) {x$results$formatted_address})
  ajid <- lapply(leox, function(x) {x$results$place_id})
  ajna <- lapply(leox, function(x) {x$results$name})
  
  
  clena <- data.frame(Address=as.character(unlist(ajax)),
                      Place_ID=as.character(unlist(ajid)),
                      Name=as.character(unlist(ajna)))
  
  return(clena)
}

#Pulling school name to see 
pex <- paste0(unique(SC$School.Text), ",Georgia")

#Passing it 100 entries from pex
ppex <- pex[1:100]
goop <- GPAPI(ppex, apikey)
#Crap , should have returned a list.  I didnt remove duplicates.  
write.table(goop, "C:/git/dbhdd/School_Address_1-100.csv",
            sep=",",col.names=T,row.names=F)

jsform <- fromJSON("https://maps.googleapis.com/maps/api/place/textsearch/json?query=Georgia&Capitol&Building&key=AIzaSyDBCTZKsSZPIv6va7eT_k8tzdLI6x439Ok")
# OH Shit I need to do something to get my quota back up . 
#With the upgraded account I get 150,000 (15,000 free queries per day) according to https://developers.google.com/places/web-service/usage
#We ask for your credit card purely to validate your identity. Your card will not be charged for use of the Google Places API Web Service.
#So this should be 23,590 queries to get the full pex

pp2 <- pex[101:length(pex)]
goop2 <- GPAPI(pp2, apikey)
#Crap!  I forgort to grab the x,y 
#Well it wont matter
write.table(goop2, "C:/git/dbhdd/data/School_Address.csv", sep=",", row.names=F)
#This link says use the ggmap library to geocode: http://gis.stackexchange.com/questions/163356/how-do-you-geocode-addresses-in-r
 
#GREAT!   So let's 
  #A) See if we can pull the x,y directly into the output object
  #Remove duplicate place IDs from the output object. 
  


#TO-DO - Compile and test the falafel cleaner.  

