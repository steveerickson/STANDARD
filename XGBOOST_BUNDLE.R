#FULL CONFIGURABLE ION FLUX FUNCTION
#Seeing if i can make this more stable by eliminating the recode statement.  
ion_RUSH <- function(nreps, data, yvars, xvcs, nxvs, 
                     hsrng, trnrng, nrounds, depth, eta) {
  
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
    #NOTE: FOR SOME REASON this is broken and does not actually rank the response.
    #Going to try wrapping the arguments as numeric because maybe they're being put in as a list.
    print(ranked[1:20,])
    testcalc <- testing
    testcalc[testcalc<=.5] <- 0
    testcalc[testcalc>.5] <- 1 
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
    UserOutput$xcols <- names(data[,xcols])
    UserOutput$ycol <- ny
    UserOutput$rank <- ranked
    UserOutput$Model_Depth <- depth
    UserOutput$HSC <- hsc
    UserOutput$Train_Pct <- trainpct
    
    
    return(UserOutput)
  } 
  
  #nxvs - Number of xvariables to randomly include in each analysis
  sroot <- rep(sample(xvcs, nxvs, replace=F), nreps) 
  trtr <- seq(trnrng[1], trnrng[2], by=.1)
  hsct <- seq(hsrng[1],hsrng[2], by=1)
  op <- list()
  for (i in 1:nreps) {
    op[[i]] <- boostLED(data=data, xcols=sample(xvcs,nxvs,replace=F), 
                        ycol=sample(yvars,1), trainpct=sample(trtr, 1), 
                        depth=depth, eta=eta, nthread=4, nrounds=nrounds,
                        hsc=sample(hsct,1))
  }
  names(op) <- paste0("MODEL_", seq_along(op))
  return(op)
}

#Testing IONf
#Last model was restrx110
#Compare with this one: same parameters, but I've restricted HSrng to 6+

#RUN ALREADY
#fff <- IONf(nreps=110, data=ncc, yvars=c(211:215), xvcs=c(4:209,266:297), nxvs=120,hsrng=c(6,10), trnrng=c(.6, .8))
#FUCkin hell!   Sort function threw it. Nixing sort
#NOTE:  Not all of these models are getting to bottom out their train error
#So let's maybe vary that parameter as well

##################FUNCTION##################FUNCTION##################FUNCTION##################FUNCTION##################FUNCTION
##################FUNCTION
meval <- function(model_list) {
  op <- model_list
  lapq <- lapply(op, function(x) x$PAI)
  lapq[is.na(lapq)] <- NULL
  apai <- mean(unlist(lapq))
  eq <- lapply(op, function(x) x$PEI)
  eq[is.na(eq)] <- NULL
  apei <- mean(unlist(eq))
  lapx <- lapply(op, function(x)  xgb.importance(feature_names=x$xcols, model=x$Model))
  
  ol <- list()          
  ol$FEATURE_IMP <- lapx
  ol$AVERAGE_PEI <- apei
  ol$AVERAGE_PAI <- apai
  return(ol)
  
}


pimp <- function(mli) {
  x<- xgb.importance(feature_names=mli$xcols, model=mli$Model)
  print(xgb.plot.importance(x))
  return(x)
} 



#Function takes a data frame and an XGBOOST Model, and makes new prediction based on that model.  
#Spiderman the matrix of x-variables
#Makes the prediction based on data. 
#Outputs a column bound to the input dataframe with the 0/1 prediction.  
Predictor <- function(df, yvar, xvars, model, hsc) {
  
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
  
  
  recodeNA <- function(df, recodevalue) {
    df[is.na(df)] <- recodevalue
    return(df)
  }
  
  
  newx <- df[,xvars]
  newy <- df[,yvar]
  newy <- recodeNA(newy, 0)
  newy[newy>=hsc] <- 1
  newy[newy<hsc] <- 0
  
  newf <- list(data=Spiderman(newx, c(1:ncol(newx))),
               label=newy)
  
  hat <- predict(model, newdata=newf$data)
  hat[hat<=.5] <- 0
  hat[hat>.5] <- 1
  df$hotspot <- hat
  return(df)
}