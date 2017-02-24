#CRIME STRUCTURE FUNCTION - IN PROGRESS


cstr <- function(datafile, crimetype) {
  all <- if (crimetype == "all") {data <- datafile}
  if (crimetype == "all") return(all)
  burg <- if (crimetype == "burglary") {ds1 <- subset(datafile, CASE_DESC == "PROWLER")
                                              ds2 <- subset(datafile, CASE_DESC == "BURGLARY - PRIORITY *H")
                                              data <- rbind(ds1, ds2)}
  if (crimetype == "burglary") return(burg)
  street <- if (crimetype == "street") {
    ds1 <- subset(datafile, CASE_DESC == "ASSAULT - PRIORITY")
    ds8 <- subset(datafile, CASE_DESC == "ASSAULT - WITH WEAPON *H")
    ds2 <- subset(datafile, CASE_DESC == "SHOTS FIRED")
    ds4 <- subset(datafile, CASE_DESC == "THREAT - PRIORITY")
    ds5 <- subset(datafile, CASE_DESC == "THREAT - WITH WEAPON *H")
    ds6 <- subset(datafile, CASE_DESC == "GANG RELATED")
    ds7 <- subset(datafile, CASE_DESC == "DISTURBANCE - WITH WEAPON *H")
    ds3 <- subset(datafile, CASE_DESC == "DISTURBANCE - PRIORITY")
    ds9 <- subset(datafile, CASE_DESC == "STABBING - WITH WEAPON *H")
    ds10 <- subset(datafile, CASE_DESC == "SHOOTING - WITH WEAPON *H")
    ds11 <- subset(datafile, CASE_DESC == "VICE-DRUGS, LIQUOR, PROSTITUTION, GAMBLING")
    ds12 <- subset(datafile, CASE_DESC == "ROBBERY - PRIORITY *H")
    ds13 <- subset(datafile, CASE_DESC == "ROBBERY - WITH WEAPON *H")
    data <- rbind(ds1,
                  ds8,
                  ds2,
                  ds4,
                  ds5,
                  ds6,
                  ds7,
                  ds3,
                  ds9,
                  ds10,
                  ds11,
                  ds12,
                  ds13)}
  if (crimetype == "street") return(street)
  auto <- if (crimetype == "auto") {ds1 <- subset(datafile, CASE_DESC == "ROLLING STOLEN *H")
  ds2 <- subset(datafile, CASE_DESC == "VEHICLE RECOVERED")
  ds3 <- subset(datafile, CASE_DESC == "VEHICLE STOLEN - PRIORITY")
  data <- rbind(ds1,ds2,ds3)}
  if (crimetype == "auto") return(auto)
} 

boostL <- function(data, xcols, ycol, trainpct, hspct=.01,
                   depth, eta, nthread, nrounds) {
  
  #ID Hot spots
  yvar <- data[,ycol]
  sy <- sort(yvar, decreasing=T)
  npct <- round(length(sy)*hspct, 0)
  cutv <- sy[1:npct]
  #Cutv yields the range of values that are considered hot spots.
  library(car)
  data[,ycol] <- recode(data[,ycol], "0:min(cutv)-1=0;min(cutv):max(cutv)=1")
  #Dichotomize the continuous y variable
  
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
  rankdata <- data.frame(obs=as.numeric(UserOutput$Observed_Response)
                         ,pre=recode(UserOutput$Prediction, "0:.5=0;.50000000000000000001:1=1"))
  ranked <- rankdata[order(-rankdata$obs),]
  #NOTE: FOR SOME REASON this is broken and does not actually rank the response.
  #Going to try wrapping the arguments as numeric because maybe they're being put in as a list.
  print(ranked[1:20,])
  
  testcalc <- recode(testing, "0:.5=0;.50000000000000000001:1=1")
  a <- sum(testcalc) # Forecasted area( total number of cells predicted to be hot spots.)
  N <- sum(testdata[,ycol])  #Total number of crimes reported (test data)
  
  # n = The sum of crimes committed within the area forecasted 
  nlocator <- grep("1", testcalc)  
  n <- sum(testlabel[nlocator])
  
  A <- nrow(testdata)  #Total area == number of "blocks" 
  
  #USING THESE WE should be able to calculate PAI
  
  PAI <- (n/N)/(a/A)
  print(paste("PAI =",round(PAI,2),sep=""))  
  UserOutput$PAI <- PAI
  
  #FOR PEI We need Nbang, which is the sum of crimes contained in the actual hot spots
  #Plus the sum of crimes contained in the predicted hot spots

  osca <- grep("1", stel$label)    #Scan the obs response for ones
  hsfreq <- sum(testlabel[osca])
  nbang <- n + hsfreq
  PEI <- PAI/((nbang)/N)/(a/A)
  UserOutput$PEI <- PEI
  print(paste("PEI =",round(PEI,2),sep=""))  
  return(UserOutput)
} 

#In progress

#NEW TIME FUNCTION TESTED.  - WORKS! 
#Dates data and breaks, either a vector of dates, or "week", month, etc.
#Spits out data with the frequencies.
tf <- function(data, breaks) {
  data$date_format <- as.Date(data$occ_date, order="ymd")
  data$df <- cut(data$date_format, breaks=breaks)
  library(dplyr)
  cright <- dcast(data, id ~ df)
  return(cright)
}

#Testing to see if output is the same
pit <- tf(cc, breaks="month")
#TESTS PASSED.


############CENSUS MERGER
#takes a data subframe with ID and wide-format time series crime data
#AND a "total" frame with the long format data linking Grid_id to their tracts
cmerger <- function(data, total) {
  tractdic <- plasma("C:/lab/crimeds/master_namedcensus.csv")
  td <- Judo(tractdic, c(3:length(names(tractdic))))
  data <- as.data.frame(data)
  total <- as.data.frame(total) #To make sure theyre not tables.
  
  slimt <- data.frame(census_tra=total$census_tra, id=total$id)
  
  gfm <- merge(data, slimt, by.x="id", by.y="id",
               all.x=T, all.y=F)
  gg <- gfm[!duplicated(gfm$id),]
  #Now merging with the tract dictionary
  cstd <- merge(gg, td, by.x="census_tra", by.y="tract.x",
                all.x=T, all.y=F)
  csclean <- cstd[!duplicated(cstd$id),]
  return(csclean)
}

#TESTING:  aut is a wide-format df 
ctx <- cmerger(aut, total)
#CHECKS OUT!  SWEET ! 





#CENSUS HIGH POWER - 
#Takes a single data frame
#Returns a list containing the cleaned data with variable names,
#percentages (not sure if still need these)
#and CLEAN percentages, which results in a file without MOU 
censusHP <- function(data1) {
  
  cleanerf <- function(data) {
    bh <- as.list(data)
    bh <- lapply(bh, function(x) gsub("Margin of Error", "MOE", x))
    bh <- lapply(bh, function(x) gsub("Estimate", "EST", x))
    bh <- lapply(bh, function(x) gsub(";", "", x))
    bh <- lapply(bh, function(x) gsub(" ", "_", x))
    bh <- lapply(bh, function(x) gsub(":", "", x))
    bh <- lapply(bh, function(x) gsub("-", "", x))
    bh$GEO.display.label <- NULL
    booster <- lapply(bh, function(x) x[1])
    nboos <- as.character(unlist(booster)) #YES!
    nba <- gsub(" ", "", nboos)
    bhclone <- bh
    names(bhclone) <- nba
    bfg <- lapply(bhclone, function(x) x[-1])
    bfn <- lapply(bfg, as.numeric)
    bfn$Id <- NULL
    bfn <- as.data.frame(bfn)
    return(bfn)
  }
  #TESTED AND WORKS! 
  
  d1 <- cleanerf(data1)
  dp <- (d1/d1[,2])*100
  pctclean <- dp
  dp$Id2 <- d1$Id2
  pctclean$Id2 <- d1$Id2
  nhp <- names(pctclean) 
  ggh <- gsub("EST", "PCT", nhp)
  names(pctclean) <- ggh
  gnoe <- grep("MOE", nhp)
  pctclean <- pctclean[,-gnoe]
  rf <- list(data=d1, pct=dp, pct_clean=pctclean)
  return(rf)
  
  
}







#BASIC MERGE FUNCTION FOR CENSUS DATA
censusH <- function(data1, data2) {
  
  cleanerf <- function(data) {
    bh <- as.list(data)
    bh <- lapply(bh, function(x) gsub("Margin of Error", "MOE", x))
    bh <- lapply(bh, function(x) gsub("Estimate", "EST", x))
    bh <- lapply(bh, function(x) gsub(";", "", x))
    bh <- lapply(bh, function(x) gsub(" ", "_", x))
    bh <- lapply(bh, function(x) gsub(":", "", x))
    bh <- lapply(bh, function(x) gsub("-", "", x))
    bh$GEO.display.label <- NULL
    booster <- lapply(bh, function(x) x[1])
    nboos <- as.character(unlist(booster)) #YES!
    nba <- gsub(" ", "", nboos)
    bhclone <- bh
    names(bhclone) <- nba
    bfg <- lapply(bhclone, function(x) x[-1])
    bfn <- lapply(bfg, as.numeric)
    bfn$Id <- NULL
    bfn <- as.data.frame(bfn)
    return(bfn)
  }
  #TESTED AND WORKS! 
  
  d1 <- cleanerf(data1)
  d2 <- cleanerf(data2)
  d3 <- merge(d1,d2, by.x="Id2", by.y="Id2",
              all.x=T, all.y=T)
  return(d3)
  
}



