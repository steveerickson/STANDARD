ACSQuery  <- function(search_key) {
  source("C:/git/standard/sfunctions.R")
  library(dplyr)
  library(tibble)
  library(data.table)
  
sttn <- function(search_key="population") {
tl <- plasma("G:/CENSUS/acs2010_5yr/Table_Lookup.csv")
tl$Table_Title <- tolower(tl$Table_Title)
tl$Subject_Area <- tolower(tl$Subject_Area)
sklocator <- grep(search_key,tl$Subject_Area)
subt <- tl[sklocator,] 
dfu <- data.frame(Name=unique(subt$Table_Title))
dfu <- rownames_to_column(subt,var="rowname")
fin <- data.frame(Title=subt$Table_Title,Row=dfu$rowname)
fip <- paste(fin$Row,fin$Title,sep=":)  ")
cat(fip,"\n Please select the desired variables", sep="\n")
  x <- readLines(con=stdin(),1)
  pui <- function(ui) {
  gui <- strsplit(ui,split=" ")
  as.numeric(unlist(gui))
  }
  
  rv <- pui(x)
  subtable <- tl[rv,]
  return(subtable)
}
filename_constructor <- function(clookup_value) {
  xtr <- readLines("C:/git/std/SEQUENCE_val.txt")
  sequence_numbers <- unique(clookup_value$Sequence_Number)
  start_positions <- unique(clookup_value$Start_Position)
  fn <- rep(0,length(sequence_numbers))
  for (i in 1:length(sequence_numbers)) {
    fn[i] <- paste("e20105st","0",xtr[sequence_numbers[i]],"000",sep="")
  }
  fn
}
file_puller <- function(sequence_number) {
lar <- list.files("G:/CENSUS/acs2010_5yr/summaryfile/2006-2010_ACSSF_By_State_By_Sequence_Table_Subset",full.names=T,recursive=T)
glf <- grep(sequence_number,lar)
larglf <- lar[glf]
gargle <- grep("_Only",larglf)
xgarg <- larglf[-gargle]
xgarg
}
nforay <- function(nopt) {
  for (i in 1:length(nopt)) {
    nopt[[i]] <- lapply(as.list(nopt[[i]]),fread)
    nopt[[i]] <- rbindlist(nopt[[i]])
  }
  nopt
}
user_enters_searchkey <- sttn(search_key) #Save this object to preserve info on the start position.  
filename_fetch <- filename_constructor(user_enters_searchkey)
nmt <- strsplit(filename_fetch,"st")
sequence_identifier <- lapply(as.list(nmt),function(x) {x <- x[2]})
list_of_filenames <- lapply(sequence_identifier,file_puller)
nfex <- nforay(list_of_filenames)
  #FAILED:  #ctp <- pull_columns(user_enters_searchkey)
#nfex$columns <- ctp
#INSERT Column extractor:  SEE PROBLEM with 
  #TESTING:  Attempting to return the column values along with the nfex object
return(nfex)
} 

#Tested - returns a list of two tables.  
#ele <- MODDED_ACSQuery("poverty")
#Excellent!