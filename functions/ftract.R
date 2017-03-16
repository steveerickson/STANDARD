#Ftract.  This should take a vector of tracts and pad them appropriately.  
ftract <- function(tracts) {
  a <- "0"
  b <- "00"
  c <- "000"
  d <- "0000"
  e <- "00000"
  mt <- tracts
  v <- c(a,b,c,d,e)
  
  z <- function(tract,v) {
    x <- nchar(tract)
    n5 <- paste0(v[5],tract)
    n4 <- paste0(v[4],tract)
    n3 <- paste0(v[3],tract)
    n2 <- paste0(v[2],tract)
    n1 <- paste0(v[1],tract)
    
    if (x==1) {return(n5)}
    if (x==2) {return(n4)}
    if (x==3) {return(n3)}
    if (x==4) {return(n2)}
    if (x==5) {return(n1)}
    if (x==6) {return(tract)}
  }
  
  
  pull <- function(tracts, v) {
    k <- list()
    for (i in 1:length(tracts)) {
      zotracts <- z(tracts[i], v)
      k[[i]] <- zotracts
    }
    return(k)
  }
  
  rval <- pull(tracts=tracts,v=v)
  return(rval)
  
}
