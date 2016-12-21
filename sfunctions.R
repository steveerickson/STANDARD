############DNA TEST - SELF ASSESSMENT DATABASE BUILDER

#DNALIST Takes a DIRECTORY, scans it for SPSS files, opens them up in a labelled list
#NEXT STEP IS to execute a successful rbind after melt with these guys, which I think may require unfactoring county
  #OR ~ WE Could still use the factors but just standardize them to be the same
  #Not sure how that works thought.  
DNALIST<- function(filepath) {
  lfs<- list.files(filepath,recursive=T,full.names=T)
  sns<- grep("\\.sav",lfs)
  son<- lfs[sns]
  sloan<- as.list(son)
  pappy<- lapply(sloan,Phaser)
  #Grab the file names Gsub the whitepsace 
  #Then call pappy those names
  sfs<- list.files(filepath)
  sfns<- grep("\\.sav",sfs)
  shortson<- sfs[sfns]
  #These are the anmes
  shortso<- gsub("\\.sav","",shortson)
  shorts<- gsub(" ","_",shortso)
  short<- gsub("-","_",shortso)
  names(pappy) <- short
  pappy
}

#EMHANCEMENT DNAX:  Automatic melting and rbind
DNAX<- function(filepath) {
  lfs<- list.files(filepath,recursive=T,full.names=T)
  sns<- grep("\\.sav",lfs)
  son<- lfs[sns]
  sloan<- as.list(son)
  pappy<- lapply(sloan,Phaser)
  #Grab the file names Gsub the whitepsace 
  #Then call pappy those names
  sfs<- list.files(filepath)
  sfns<- grep("\\.sav",sfs)
  shortson<- sfs[sfns]
  #These are the anmes
  shortso<- gsub("\\.sav","",shortson)
  shorts<- gsub(" ","_",shortso)
  short<- gsub("-","_",shortso)
  names(pappy) <- short
mbold<- function(x) {melt(x, id="county")}
detest<- lapply(pappy,mbold)
detest
}
#######FAILED.

############ATOMIZER - FILE REPORTING 
ATOM<- function(dir) {
  lfd<- list.files(dir, 
    full.names=T,
    recursive=T,
    include.dirs=T)
  
  fin<- file.info(lfd)
  fin$mb<- fin$size*1e-6
  bigfin<- subset(fin,mb>=2000)
  biglocator<- row.names(bigfin)
  #Problem here: How do I recover the file names for the prinout?
  
  #Find different data file types
  savl<- grep("\\.sav",lfd)
  csvl<- grep("\\.csv",lfd)
  datl<- grep("\\.dat",lfd)
  dbfl<- grep("\\.dbf",lfd)
  #Summary printout
  savnum<- paste(length(savl),".sav files identified.\n")
  csvnum<- paste(length(csvl),".csv files identified.\n")
  datnum<- paste(length(datl),".dat files identified.\n")
  dbfnum<- paste(length(dbfl),",dbf files identified.\n")
  
  printout<- c(savnum,csvnum,datnum,dbfnum)
  #Printout is an inchoate summary of the contents. 
  #One thing I would like to include is to return an object like
  #data$csv which includes a vector of those files 
  
  sealone<- list(CSV=lfd[csvl],
    SAV=lfd[savl],
    DAT=lfd[datl],
    DBF=lfd[dbfl],
    BIGDATA=bigfin,
    FULL_LIST=fin)
  #It'd be nice to display file size alongside it for this obj
sealone
#NOTE:  Add text file detector.
}




########################################

#Works
#A wrapper for write.table to csv would be cool, one that dumps into a default directory 
#With either tha object as fn or random name.

#ATOMIZER
  #SMASHES DATA Into elementary pieces
  #Descriptives, missing data, etc.


#1.0
Judo<- function(df,vnum) {
gbomb<- names(df)
dv<- gbomb[vnum]
    rv<- df[ , !(names(df) %in% dv)]
rv
} 

#Phaser<- function(fp) {
#  psav<- grep("\\.sav",fp)
#  pcsv<- grep("\\.csv",fp)
#  pdbf<- grep("\\.dbf",fp)
#  dval<- if (length(psav)>0) {library(memisc)
#    data.frame(as.data.set(spss.system.file(fp),stringsAsFactors=F))}
#    if (length(psav)>0) return(dval)
#  cval<-   if (length(pcsv)>0) (read.csv(fp,sep=",",header=T,as.is=T))
#    if (length(pcsv)>0) return(cval)
#  dbfl<- if (length(pdbf>0)) {library(foreign)
#    read.dbf(fp,as.is=T)}
#    if (length(pdbf)>0) return(dbfl)
#}
#
#'
#'
#DEPRECATING PHASER in favor of Plasma
plasma<- function(fp) {
  psav<- grep("\\.sav",fp)
  pcsv<- grep("\\.csv",fp)
  pdbf<- grep("\\.dbf",fp)
  dval<- if (length(psav)>0) {library(memisc) 
    library(data.table)
    as.data.table(as.data.set(spss.system.file(fp),stringsAsFactors=F))}
    if (length(psav)>0) return(dval)
  cval<-   if (length(pcsv)>0) (read.csv(fp,sep=",",header=T,as.is=T))
    if (length(pcsv)>0) return(cval)
  dbfl<- if (length(pdbf>0)) {library(foreign)
    read.dbf(fp,as.is=T)}
    if (length(pdbf)>0) return(dbfl)
}
##################################################################
##################################################################
##################################################################
#####################END PLASMA FUNCTION##########################
##################################################################

#HSPAM takes a dataframe and rips out the numeric variables
#Then produces a bunch of histograms for each one dumps it onto HDD Data
 hspam<- function(df) {
   library(ggplot2)
  nums<- sapply(df,is.numeric)
  nt<- which(nums=="TRUE")
  dfnt<- df[,nt]
 HS <- function(x, na.rm = TRUE, ...) {
  nm <- names(x)
  for (i in seq_along(nm)) {
plots <-ggplot(x,aes_string(x = nm[i])) + geom_histogram(fill = "tomato") +
  theme_bw() + 
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
ggsave(plots,filename=paste("G:/DATA/",nm[i],".png",sep=""))
  }
 }
 HS(dfnt)
}

#For nice-looking numeric citations in text, twopct takes a count and a total to create a string value representing the percent
twopct<- function(subset,total) {
  paste(round(((subset/total)*100),2),"%",sep="")
}
#Clean percent takes same arguments and returns number only, suitable for tables
clnpct<- function(subset,total) {
  round(((subset/total)*100),2)
}

#DESCENDING SORT of a data frame based on a single column value var.
  #NOTE The var has to be references explicitly as $var
DSORT<- function(df,var) {df[with(df, order(-var)), ]}
ASORT<- function(df,var) {df[with(df, order(var)), ]}

#Spiderman takes a dataframe and a vector of column identifiers
#And converts directly to a sparse matrix.
Spiderman<- function(data,dcols) {
library(Matrix)
nre<- nrow(data)
nce<- length(dcols)
mat<- matrix(0,nrow=nre,ncol=nce)
for (i in dcols) {
  mat[,dcols[i]]<- data[,dcols[i]]
}
smat<- Matrix(mat,sparse=T)
smat
}

#' Convert a list of vectors to a data frame.
#' 
#' This function will convert a list of vectors to a data frame. This function
#' will handle three different types of lists of vectors. First, if all the elements
#' in the list are named vectors, the resulting data frame will have have a number
#' of columns equal to the number of unique names across all vectors. In cases
#' where some vectors do not have names in other vectors, those values will be
#' filled with \code{NA}.
#' 
#' The second case is when all the vectors are of the same length. In this case,
#' the resulting data frame is equivalent to applying \code{rbind} across all elements.
#' 
#' The third case handled is when there are varying vector lengths and not all the
#' vectors are named. This condition should be avoided. However, the function will
#' attempt to convert this list to a data frame. The resulting data frame will have
#' a number of columns equal to the length of the longest vector. For vectors with
#' length less than this will fill the row with \code{NA}s. Note that this function
#' will print a warning if this condition occurs.
#' 
#' @author Jason Bryer <<jason@@bryer.org>>
#' @references \url{http://stackoverflow.com/questions/4227223/r-list-to-data-frame}
#' @param x a list to convert to a data frame.
#' @param row.names a vector equal to \code{length(x)} corresponding to the row names.
#'        If \code{NULL}, the row names will be set to \code{names(x)}.
#' @param optional not used.
#' @param ... other parameters passed to \code{\link{data.frame}}.
#' @return a data frame.
#' @S3method as.data.frame list
#' @export
#' @examples
#'     test1 <- list( c(a='a',b='b',c='c'), c(a='d',b='e',c='f'))
#'     as.data.frame(test1)
#'     
#'     test2 <- list( c('a','b','c'), c(a='d',b='e',c='f'))
#'     as.data.frame(test2)
#'     
#'     test3 <- list('Row1'=c(a='a',b='b',c='c'), 'Row2'=c(var1='d',var2='e',var3='f'))
#'     as.data.frame(test3)
#'     
#'     test4 <- list('Row1'=letters[1:5], 'Row2'=letters[1:7], 'Row3'=letters[8:14])
#'     as.data.frame(test4)
#'     
#'     test5 <- list(letters[1:10], letters[11:20])
#'     as.data.frame(test5)
#'     
#'     test6 <- list(list(letters), letters)
#'     as.data.frame(test6)
as.data.frame.list <- function(x, row.names=NULL, optional=FALSE, ...) {
	if(!all(unlist(lapply(x, class)) %in% 
				c('raw','character','complex','numeric','integer','logical'))) {
		warning('All elements of the list must be a vector.')
		NextMethod(x, row.names=row.names, optional=optional, ...)
	}
	allequal <- all(unlist(lapply(x, length)) == length(x[[1]]))
	havenames <- all(unlist(lapply(x, FUN=function(x) !is.null(names(x)))))
	if(havenames) { #All the vectors in the list have names we can use
		colnames <- unique(unlist(lapply(x, names)))
		df <- data.frame(matrix(
			unlist(lapply(x, FUN=function(x) { x[colnames] })),
			nrow=length(x), byrow=TRUE))
		names(df) <- colnames
	} else if(allequal) { #No names, but are of the same length
		df <- data.frame(matrix(unlist(x), nrow=length(x), byrow=TRUE), ...)
		hasnames <- which(unlist(lapply(x, FUN=function(x) !is.null(names(x)))))
		if(length(hasnames) > 0) { #We'll use the first element that has names
			names(df) <- names(x[[ hasnames[1] ]])
		}
	} else { #No names and different lengths, we'll make our best guess here!
		warning(paste("The length of vectors are not the same and do not ",
					"are not named, the results may not be correct.", sep=''))
		#Find the largest
		lsizes <- unlist(lapply(x, length))
		start <- which(lsizes == max(lsizes))[1]
		df <- x[[start]]
		for(i in (1:length(x))[-start]) {
			y <- x[[i]]
			if(length(y) < length(x[[start]])) {
				y <- c(y, rep(NA, length(x[[start]]) - length(y)))
			}
			if(i < start) {
				df <- rbind(y, df)
			} else {
				df <- rbind(df, y)
			}
		}
		df <- as.data.frame(df, row.names=1:length(x))
		names(df) <- paste('Col', 1:ncol(df), sep='')
	}
	if(missing(row.names)) {
		row.names(df) <- names(x)
	} else {
		row.names(df) <- row.names
	}
	return(df)
}

if(FALSE) { #Test the function
	require(devtools)
	source_gist(4676064)
	
	test1 <- list( c(a='a',b='b',c='c'), c(a='d',b='e',c='f'))
	as.data.frame(test1)
	
	test2 <- list( c('a','b','c'), c(a='d',b='e',c='f'))
	as.data.frame(test2)

	test3 <- list('Row1'=c(a='a',b='b',c='c'), 'Row2'=c(a='d',var2='e',var3='f'))
	as.data.frame(test3)

	test4 <- list('Row1'=letters[1:5], 'Row2'=letters[1:7], 'Row3'=letters[8:14])
	as.data.frame(test4)
	
	test5 <- list(letters[1:10], letters[11:20])
	as.data.frame(test5)
	
	test6 <- list(list(letters), letters)
	as.data.frame(test6)
}


#PLACE API Handler
#GPAPI Takes a string and an API key and constructs a google place API Query.  
#NEXT STEP:  Let gpapi execute it and store the resulting object.  
#############DEVELOPMENT:#####################
gpapi<- function(query,key) {
  library(stringr)
  library(jsonlite)
  #Split the query object by whitespace
  sq<- str_split(query,pattern=" ")
  csq<- as.character(sq[[1]])
  lcsq<- length(csq)
  xcsq<- csq[-lcsq]
  newe<- paste(xcsq,"+",sep="")
  newg<- c(newe,csq[lcsq])
  fulle<- paste(newg,collapse="")
  stich<- paste("https://maps.googleapis.com/maps/api/place/textsearch/json?query=",
  fulle,"&key=",key,sep="")
  FULLAPI<- fromJSON(stich)
  FULLAPI
}
