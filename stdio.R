ww <- function(df,fn) {
  complete_filename <- paste("C:/databin/QuickWrite/",fn,".csv",sep="")
  write.table(df, complete_filename, sep=",", row.names=F)
}

Judo<- function(df,vnum) {
  gbomb<- names(df)
  dv<- gbomb[vnum]
  rv<- df[ , !(names(df) %in% dv)]
  rv
} 
 
plasma<- function(fp) {
  psav<- grep("\\.sav",fp)
  pcsv<- grep("\\.csv",fp)
  pdbf<- grep("\\.dbf",fp)
  pxls <- grep("\\.xls",fp)
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
}


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


