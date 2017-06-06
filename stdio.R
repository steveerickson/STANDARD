#MULTIPLE SUBSET:
#               errp$alcaco[which(errp$ACOBI==1 | errp$ACABI==1)] <- 1

#TO-DO LIST
#DROPCHAR:  Drops character-type variables from df
dropchar <- function(df) {
  rf <- df
  sapper <- sapply(rf, FUN="class")
  gsappr <- grep("character", sapper)
  df <- rf[,-gsappr]
  return(df)
}
#FUNCTION COMPLETE!   VERY HANDY!   



#IN PROGRESS
Export_Mplus <- function(df, targetdir, filename) { 
  varlist <- names(df)
  shortvar <- unlist(lapply(varlist, function(x) substr(x, start=0,stop=8)))
  
}


#SWEET little root function is an alternative to structure command
  #TO ADD- automatically group variables by class
  #TO ADD - if there are less than 7 unique values of a variable , it prints those
root <- function(df) {
  varnames <- names(df)
  sumna <- sapply(df, function(x) sum(as.numeric(is.na(x))))
  pctnas <- as.numeric((sumna/nrow(df))*100)
  varclass <- as.character(sapply(df, class))
  varrange <- sapply(df, range)
  
  for (i in 1:length(varnames)) {
    msg <- paste0("Variable:  ",varnames[i])
    print(msg, quote=F)
    print(paste0("(", toupper(varclass[i]), ")"), quote=F)
    print(paste0("Range:  ", varrange[1,i], " - ", varrange[2,i]), quote=F)
    print(paste0("MISS%:  ", pctnas[i]), quote=F)
    print("-----------------------", quote=F)
  }
  
}

th <- function(x) {
  hist(x, col="royalblue4",border="black", ylab=NULL, xlab=NULL, main=NULL)
}

#Postgres connect function 
#Function takes a list of data frames and a merge variable and returns an object merging all datasets on a default all=t
mmerge <- function(ldf, mvar) {
  basket <- Reduce(function(x,y) merge(x,y,by=mvar, all=T), ldf)
  return(basket)
}


Connect <- function() {
  library(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  password <- readline(prompt="Enter password:")
  con <- dbConnect(drv, dbname = "postgres",
                   host = "localhost", port = 5432,
                   user = "postgres", password = password)
  return(con)
}
#wt - quick write table - assumes csv
wcsv <- function(obj , fp) {
  write.table(obj, fp, sep=",", row.names=F)
}


ttf <- function(table) {
  rf <- data.frame(Name=names(table),
                   Value=as.numeric(table))
  return(rf)
} #Table to frame - takes a table and turns it to a named df.
#Auto-write CSV with option handler
blast <- function(df,fp) {
  write.table(df, fp, row.names=F, sep=",")
}

#REORDER COLS IN DF
#bitxc <- b1[,c(7,9,8,10,14,15,16,6,13,5,12,4,11,17:20)]
#This sets 7 as the first column, etc. 

#Mplus  Extractor functions
#Function takes an output file path and provides variable names
#And means for an mplus imputation file
pullMIM <- function(outfilepath) {
  outfile <- readLines(outfilepath)
  ess <- grep(toupper("Estimated sample statistics"), outfile)
  cov <- grep("Covariances", outfile)
  iso <- outfile[ess:cov[1]]
  gm <- grep("Means", iso)
  vloc <- gm+1
  mloc <- vloc+2
  variableNames <- iso[vloc]
  Means <- iso[mloc]
  #Pick up here - split the elements based on whitespace/ 
  #Bind together into a df
  smean <- strsplit(Means, " ")
  lasmn <- lapply(smean, function(x) x <- x[nchar(x)>1]) #Perfect
  svarn <- strsplit(variableNames, " ")
  lavsm <- lapply(svarn, function(x) x <- x[nchar(x)>1])
  #Unlisting should give us elements for a df
  outputframe <- data.frame(Variables=unlist(lavsm),
                            Means = unlist(lasmn))
  return(outputframe)
}



#Under construction
#Function takes a data frame
#And returns a list with data subset by every possible cat variable @ mean

subsetter <- function(df) {
  varclasses <- lapply(as.list(df), class) #This returns a list where each element
  #Is the name of variable with the first element the class of the var
  varnames <- names(df)
  #Pull the unique values of each categorical variable
  #Subset the data , export
}

#Takes an atomic vector factor and returns a numeric
unFactor <- function(atomicfactor) {
  af <- as.character(atomicfactor)
  an <- as.numeric(af)
  return(an)
}



############FUNCTION IDEAS
#AlertnessXTask Generator:  Takes a vector of tasks, and asks the user to assign a predicted reqd lvl
#of alertness to each. It then plots them in a color-coded manner so that work is more easily organized.

#MatchApply:  Takes a weight matrix and a list of dfs, and spams the matching algorithm at it.  

#APMERGE:  ALL-possible merge
  #Function takes two or more datasets which share some key
  #and whether the user wants to preserve, reduce, or amplify the given information
#Decides on the correct criteria to judge a successful merge
#Peforms many different attempts at merging until one satisfies the criteria
#If it cant satisfy it gives its 10 best guesses. 


 
#Writes a list of data frames to a list of filenames, same length obv
writer <- function(list, filenames) {
  for (i in 1(length(list))) {
    write.table(list[[i]], filenames[i], sep=",", row.names=F)
  }
}

#Loop a subset, write table command perhaps?
#HELLFIRE - burn a HUGE list of dfs out to a folder
hellfire <- function(longdf, path, name) {
  library(data.table)
  longdf <- rbindlist(longdf, idcol="idnumber")
  un <- unique(longdf$idnumber)
  lun <- length(un)
  for (i in 1:lun) {
    write.table(subset(longdf, idnumber==i), paste(path,name,i,".csv",sep=""), sep=",",row.names=F)
  }
} 

Z <- function(mean, popmean, popsd) {
  z <- (mean-popmean)/popsd
  return(z)
}



#Being fucked by Data.table?  RIP IT back to a df.
ripDT <- function(datatable) {
  reio <- unclass(datatable)
  really <- as.data.frame(reio)
  return(really)
}
############HOW DOES THIS WORK?
#peelr was a list of data frames, I was able to merge all of them on GEOID
#In one line of code:  
#$funky <- Reduce(function(x, y) merge(x, y, by="GEO.ID2",all=TRUE), peelr)
#FIGURE OUT WHATS UP WITH THIS AWESOME REDUCE FUNCTION .  

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
#@@@@@@@@@@@@@@@@@@@@@VRI SDFHUJIK DFASIUJK FDSKJIBDFSIKDSBUOJADS
#@BROKEN:  DO NOT USE;
#@IF/THEN PROBABLY BROKEN


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
##################000000000000000000000000 MISSING DATA

#Need a function to remove 100% NA COlumns
killNA <- function(df) {
  kill <- function(dfcol) {
    return(sum(is.na(dfcol)))
  }
  ol <- list()
  g <- for (i in 1:ncol(df)) {
    ol[[i]] <- kill(df[,i])
  }
  soul <- grep(as.character(nrow(df)), ol)
  rdf <- df[,-soul]
  return(rdf)
}

recodeNA <- function(df, recodeval) {
  dfdevil <- lapply(df, function(x) {x[is.na(x)] <- recodeval
return(x)})
 return(as.data.frame(dfdevil)) 
}

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


#FIX FUNCTION - borders for the bars, and squish the y-axis up into the bars
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
      ggsave(plots,filename=paste("C:/databin/",nm[i],".png",sep=""))
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




##############MACHINE LEARNING
#WRAPPER FOR XGBOOST - TAILORED FOR THE CRIME DATA
#ENHANCE:  MODIFY FOR A MORE GENERIC INTERFACE.  
ICER <- function(nreps, data, yvars, xvcs, nxvs, 
                 hsrng, trnrng, nrounds, depth, etarange,
                 testcol) {
  y <- data[,yvars]
  y[is.na(y)] <- 0
  data[,yvars] <- y
  
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
  
  boostLED <- function(data, xcols, ycol, trainpct,
                       depth, eta, nthread, nrounds, hsc) {
    library(xgboost) 
    nex <- names(data)
    ny <- nex[ycol]
    y <- as.numeric(data[,ycol])
    y[y<hsc] <- 0
    y[y>=hsc] <- 1
    data[,ycol] <- y
    
    samplen <- round(nrow(data)*trainpct, 0) 
    sselect <- sample(1:nrow(data), samplen, replace=F)#Sample selector
    testdata <- data[-sselect,]
    traindata <- data[sselect,]
    #Select x-variables for analysis from each data set, split out y variable, build list with sparse matrix
    xtest <- testdata[,xcols]
    xtrain <- traindata[,xcols]
    testlabel <- testdata[,ycol]
    trainlabel <- traindata[,ycol]
    
    xstest <- Spiderman(xtest, c(1:ncol(xtest)))
    xstrain <- Spiderman(xtrain, c(1:ncol(xtrain))) 
    stel <- list(data=xstest, label=testlabel) #Sparse test list                      
    strl <- list(data=xstrain, label=trainlabel)
    
    #Train the model
    training <- xgboost(data=strl$data, label=strl$label,
                        max.depth=depth, eta=eta, nthread=nthread, nrounds=nrounds, objective="binary:logistic")
    
    #Use trained model for prediction of test data.
    testing <- predict(training, stel$data)
    UserOutput <- list(Model=training, Prediction=testing, Observed_Response=stel$label)
    UserOutput$Prediction[UserOutput$Prediction<=.5] <- 0
    UserOutput$Prediction[UserOutput$Prediction>.5] <- 1
    rankdata <- data.frame(obs=as.numeric(UserOutput$Observed_Response)
                           ,pre=UserOutput$Prediction)
    ranked <- rankdata[order(-rankdata$obs),]
    print(ranked[1:20,])
    testcalc <- testing
    testcalc[testcalc<=.5] <- 0
    testcalc[testcalc>.5] <- 1  
    UserOutput$xcols <- xcols
    UserOutput$ycol <- ycol
    UserOutput$xcolchars <- names(data[,xcols])
    UserOutput$rank <- ranked
    UserOutput$Model_Depth <- depth
    UserOutput$HSC <- hsc
    UserOutput$Train_Pct <- trainpct
    UserOutput$ETA <- eta
    return(UserOutput)
  } 
  
  fma <- function(lmodels, testdata) {
    
    l <- lmodels
    ycol <- l[[1]]$ycol
    l$df <- testdata
    
    PredictLoop <- function(l) {
      df <- l$df
      l$df <- NULL
      openlist <- list()
      
      for (i in 1:length(l)) {
        openlist[[i]] <- Predictor(df=df, yvar=l[[i]]$ycol, xvars=l[[i]]$xcols, model=l[[i]]$Model, hsc=l[[i]]$HSC)
        
      }
      return(openlist)
    } 
    
    
    PAI <- function(data, ycol) {
      a <- sum(data$hotspot) #N cells forecasted
      N <- sum(data[,ycol]) #N crimes in the obs data
      # n = The sum of crimes committed within the area forecasted 
      nlocator <- grep("1", data$hotspot)
      y <- as.numeric(data[,ycol])
      yl <- y[nlocator]
      n <- sum(yl) # n = The sum of crimes committed within the area forecasted 
      A <- nrow(data)  #Total area == number of "blocks" 
      PAI <- (n/N)/(a/A)
      rob <- sort(y, decreasing=T)
      nb  <- sum(rob[1:a])
      d <- (nb/N)/(a/A)
      PEI <- PAI/d
      
      xl <- list(PAI=PAI, PEI=PEI)
      return(xl)
    }
    
    ldfs <- PredictLoop(l=l)
    df2 <- lapply(ldfs, function(x) PAI(x, ycol=testcol))
    return(df2)
  } 
  
  
  #nxvs - Number of xvariables to randomly include in each analysis
  sroot <- rep(sample(xvcs, nxvs, replace=F), nreps) 
  trtr <- seq(trnrng[1], trnrng[2], by=.1)
  hsct <- seq(hsrng[1],hsrng[2], by=1)
  etar <- seq(etarange[1], etarange[2], by=.1)
  op <- list()
  for (i in 1:nreps) {
    op[[i]] <- boostLED(data=data, xcols=sample(xvcs,nxvs,replace=F), 
                        ycol=yvars, trainpct=sample(trtr, 1), 
                        depth=depth, eta=sample(etar, 1), nthread=4, nrounds=nrounds,
                        hsc=sample(hsct,1))
  }
  names(op) <- paste0("MODEL_", seq_along(op))
  
  #op is the lmodels
  #Data is the  data frame
  
  formalAssessment <- fma(lmodels=op, testdata=data)
  op$PAI_PEI <- formalAssessment
  print(op$PAI_PEI)
  
  return(op)
}
