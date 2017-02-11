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
#ENHANCE:  Add XLS and XLSX processing to plASMA
#Testing packages
library(data.table)
install.packages("tidyverse")
library(tidyverse)
library(readxl)
laff <- list.files("C:/git/dbhdd",full.names=T)
gfg <- laff[grep(".xls",laff)]
test1 <- read_excel(gfg)
#Performs admirably in this test case!  Seeing if I can find others.
gg <- list.files("C:/exceltest",full.names=T)


#Testing several random excel files to see performance.
gg1 <- read_excel(gg[1])
gg2 <- read_excel(gg[2])
gg3 <- read_excel(gg[3])
gg4 <- read_excel(gg[4])
gg5 <- read_excel(gg[5])
gg6 <- read_excel(gg[6])
gg7 <- read_excel(gg[7])
gg8 <- read_excel(gg[8])
gg9 <- read_excel(gg[9])
#All these except an accidental csv I left in there, came out fine
#Awesome and consistent performance.  I'll write it in.  

#integrated.  Testing in the enhanced plasma
gg1 <- plasma(gg[1])
gg2 <- plasma(gg[2])
gg3 <- plasma(gg[3])
gg4 <- plasma(gg[4])
gg5 <- plasma(gg[5])
gg6 <- plasma(gg[6])
gg7 <- plasma(gg[7])
gg8 <- plasma(gg[8])
gg9 <- plasma(gg[9])
#Success!  Integrating to stdio 
