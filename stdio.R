

#take the whole data frame and convert a new GEOID based on state,county,tract,and blockgroup
MFtract <- function(df) {
  a <- "0"
  b <- "00"
  c <- "000"
  d <- "0000"
  e <- "00000"
  tracts <- df$tract
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
  ln <- lapply(rval, as.character)
  df$tractnew <- unlist(ln)
  df$GEO.ID2  <- paste0(df$state,"0",df$county,df$tractnew,df$blockgroup)
  return(df)
}


#WRITE! WRITE!   Quick write function, takes objc and fn, and if fn is not specified 
#It executes a quickwrite 
ww <- function(obj, fn=NULL) {
  fudg <- sample(1:500000, 1)
  ffn <- paste("C:/databin/Quickwrite_",fudg,".csv",sep="")
  fn <-  if (is.character(fn) == FALSE) {fn <- ffn} else {fn <- fn}
  write.table(obj, fn, sep=",", row.names=F)
}



##################000000000000000000000000 MISSING DATA
#
##################000000000000000000000000 MISSING DATA

#STANDARD NA REPLACER 
NAS <- function(df, replace) {
  df[is.na(df)] <- replace
  
  return(df)
}
#Qna takes a df and returns the percent missing for each variable.  Handy!
qna <- function(df) {
  sumna <- sapply(df, function(x) sum(as.numeric(is.na(x))))
  pctnas <- (sumna/nrow(df))*100
  shlist <- list(VariableName=names(pctnas), Percents=pctnas)
  class(shlist) <- "qna"
  return(shlist)
}

print.qna <- function(qna) {
  vn <- qna$VariableName
  vpcts <- qna$Percents
g <-   for (i in 1:length(vn)) {
    print("-----------------------------")
    print(paste0("VARIABLE:         ", vn[i]))
    cat("\n")
    print(paste0("PERCENT MISSING:  ", round(vpcts[i],2), "%"))
    print("-----------------------------")
    cat("\n")
  }
 return(g)
} 
#000000000000000000000000 MISSING DATA
print("MISSING DATA FUNCTION LOADED:  qna")
print("Qna takes a df and returns the percent missing for each variable.  Handy!")
print("MISSING DATA FUNCTION LOADED:  NAS")
print("NAS takes a df and a replacement value")
##################000000000000000000000000 MISSING DATA

#Full file list.
#ENHANCE:  it'd be nice if theprint was prettier, and it listed the "FILE TYPE" 
#Spelled out, and maybe file size or date

FF <- function(dir) {
  list.files(dir, full.names=T,
             recursive=T, 
             include.dirs=T)
}

#IN PRODUCTION %%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ifo <- function(dir="C:/") {

 prif <- list.files(dir, full.names=T, include.dirs=T)
 print(list.files(dir, full.names=T, include.dirs=T))
  print("Enter the row number for the desired file or directory")
  ggk <- readline()
  doit <- plasma(prif[ggk])
  return(doit)
}
  #iNTERACTIVE FILE OPENER
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Quickwrite
ww <- function(df,fn) {
  complete_filename <- paste("C:/databin/QuickWrite/",fn,".csv",sep="")
  write.table(df, complete_filename, sep=",", row.names=F)
}

#Chop vnum colums from df
Judo<- function(df,vnum) {
  gbomb<- names(df)
  dv<- gbomb[vnum]
  rv<- df[ , !(names(df) %in% dv)]
  rv
} 
 
#File type detector/opener
plasma<- function(fp) {
  psav<- grep("\\.sav",fp)
  pcsv<- grep("\\.csv",fp)
  pdbf<- grep("\\.dbf",fp)
  pxls <- grep("\\.xls",fp)
  ptxt <- grep("\\.txt",fp)
  dval<- if (length(psav)>0) {library(memisc) 
    library(data.table)
    as.data.table(as.data.set(spss.system.file(fp),stringsAsFactors=F))}
  if (length(psav)>0) return(dval)
  cval<-   if (length(pcsv)>0) (read.csv(fp,sep=",",header=T,as.is=T))
  if (length(pcsv)>0) return(cval)
  dbfl<- if (length(pdbf>0)) {library(foreign)
    read.dbf(fp,as.is=T)}
  if (length(pdbf)>0) return(dbfl)
  dxls <- if (length(pxls>0)) {library(readxl)
    read_excel(fp)}
  if (length(pxls>0)) return(dxls)
  dtxt <- if (length(ptxt>0)) {readLines(fp)}
  if (length(ptxt>0)) return(dtxt)
}

#Spam histograms
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


