fdtract <- function(tracts) {
  library(stringr)
  a <- "0"
  b <- "00"
  c <- "000"
  d <- "0000"
  e <- "00000"
  mt <- tracts
  v <- c(a,b,c,d,e)
  smt <- str_split(mt, "\\.")
  spli <- lapply(smt, function(x) x[1])
  spl2 <- lapply(smt, function(x) x[2])
  us <- unlist(spl2)
  us[is.na(us)] <- "00"
  
  
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
  
  tracts <- unlist(spli)
  rval <- pull(tracts=tracts,v=v)
  ur <- unlist(rval)
  pur <- paste(ur, us, sep=".")
  return(pur)
  
}