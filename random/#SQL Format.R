
current <- readLines("C:/git/dissp/copycurrent.sql")

fcurry <- function(currentstatement, nreplacements) {
  l <- list()
  x <- seq(2,nreplacements)
  repz <- paste0("Seq_",seq(1,length(nreplacements)))
  for (i in x) {
    l[[i]] <- gsub("Seq_1",repz[i], currentstatement)
  }
  return(l)
}

fblast <- fcurry(currentstatment=current, nreplacements=118)

