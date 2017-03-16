
#REMINDER:  Write a wrapper for ggplot's obscenely complex code so you never have to deal with this bullshit again

#Attempting a standard set of data analysis tools for use with R
#Phaser:  AUTOMATIC DATA SELECTOR - FIRE AND FORGET
#STANDARD SCRIPT

Phaser<- function(fp) {
  psav<- grep("\\.sav",fp)
  pcsv<- grep("\\.csv",fp)
  pdbf<- grep("\\.dbf",fp)
  dval<- if (length(psav)>0) {library(memisc)
    data.frame(as.data.set(spss.system.file(fp)))}
    if (length(psav)>0) return(dval)
  cval<-   if (length(pcsv)>0) (read.csv(fp,sep=",",header=T,as.is=T))
    if (length(pcsv)>0) return(cval)
  dbfl<- if (length(pdbf>0)) {library(foreign)
    read.dbf(fp,as.is=T)}
    if (length(pdbf)>0) return(dbfl)
}
#TO-DO LIST COMPLETED.  
#Currently accepts csv sav and dbf files
#DBF Automatically inputs with header
#CSV Assumes header and you want strings and not factors

#Takes a vector vnum defining the df columns to be dropped
#and creates a frame  dropping it
Judo<- function(df,vnum) {
gbomb<- names(df)
dv<- gbomb[vnum]
    rv<- df[ , !(names(df) %in% dv)]
rv
} 

#########DATA PRESENTATION:  XTABLE
mstab<- data.frame(Variable=ttnam,NoHE_MSD=szmpair,HE_MSD=sospair,Statistic=ttval,p=ttps)
#xtable(mstab,caption="Means, SDs, and independent samples T-tests #by whether the patient saw a health educator or not.")
ncn<- c("M(SD): No HE","M(SD): Saw HE")
#Trying to set sanitized new column names
colnames(mstab)[2:3]<- ncn
#Success!  This method allows for setting nonstandard column names in an Xtable.
xtable(mstab,caption="Means, SDs, and independent samples T-tests by whether the patient saw a health educator or not.")




####DATA MANIPULATION TOOLS@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
###############################@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
rm(list=ls())
###Datamanagement 
qt<- table(pr$Q..)
#Basic frequencies for the variable
ltobj$studname<- append(ltobj$studname,ln)
#Append command adds second argument to the end of the first
#Must be a vector
assign(fivecode[1],28000)

ntmane<- which(nums == "TRUE")

#Need a missing data handler
nomiss<- na.omit(fud)

#This works on data frames - data frame is apparently a type of list
mfudi<- sapply(fudi,mean)


#TO-DO:  
  #1(  LARGE DATASETS - Add instructions for ffdf


  #2(  #STANDARD PROGRAM
    #Point it at a data file, and it determines whether the file is dat, csv, spss, or some other format
    #It defaults to the preferred method, memisc for spss, read,table for a dat, and what im asssuming is read.csv
    #WITH STRINGSTOFACTORS OFF

#VECTOR
fv<- dv[-8]
  #Fv dv minus vector 8

#Identify duplicates in crs
dcrs<- duplicated(crs)
#This will return a boolean you turn it to numeric
ncrs<- as.numeric(dcrs)
nrnc<- which(ncrs==1)
#Then use Which to identify the vector numbers which are duplicated


###SCRIPTS

#Write a dat for mplus
write.table(c1k,"C:/shanteria/imputation/main.dat",sep="\t",
  row.names=FALSE,
  col.names=FALSE)

nfile<- names(c1k)
nfile
write.table(nfile,"C:/shanteria/imputation/varlist.csv",sep=",",
  row.names=FALSE,
  col.names=FALSE)
c1k$Perp_gender_1
c1k$Perp_gender_2

#BENCHMARKING
system.time(expr, gcFirst = TRUE) #For benching an expr
Sys.time()#For direct access to the system time

###################STRINGS
#Pad with 0s using sprintf
ddd$pad_county <- sprintf("%03d", ddd$county)

nbc<- gsub("\\.","",ncb)
#String substitution:  Substitutes period with nothing in vector ncb

#####PROVISIONAL SCRIPT
#Using the textminer package for extracting PDF Info
#IN PROGGRESS
#ARTICLERIPPER:
#1)   Add information about an article easily.  
#2)  Pull info from article?
#Background information:
library(tm)
dirname<- file.path("C:","rhandler")
doccorp<- Corpus(DirSource(dirname))
summary(doccorp)
#Creating document term matrix see https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
dtmcorp<- DocumentTermMatrix(doccorp)
tdmcorp<- TermDocumentMatrix(doccorp)
library(wordcloud)
#Word frequencies?
freq<- colSums(as.matrix(dtmcorp))
str(dtmcorp)
dtmcorp$dimnames
dtmcorp$dimnames$Docs
inspect(dtmcorp$dimnames$Docs[8])
pdf1<- readPDF("C:/rhandler/pfex.pdf")
#What a fucking PITA package....



#END SCRIPTS
#Merging################## ->>>>>>>>>>>>>>><___________________





#####################factors

library(plyr)
#Renaming factor levels http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/
revalue(lab, c("Health Problems and Self-Medicating" = "Health and Self-Medicating"))
#WHOA THAT WAS EASY!

######################LISTSL
#Working with lists.
#RULE #1:  know to unlist!   It's awesome!
unlist(listname) #This breaks stuff down into a vector.  
#List creation. 
Basiclist<- list(Name=c("Carol","Linda","Stacy"), Value=c(1000,2812,1233))
#Index list of names
Basiclist$Name[]
#Index Name #2
Basiclist$Name[[2]]
#FUNCTION WRAP:  Add a value to basiclist
Addvalue<- function(Listname,wakevalue) {
  ln<- length(Listname$Value)
  Listname$Value[[ln+1]]<- wakevalue
  Listname
}
#Attempt1
Adding10<- Addvalue(Basiclist,10)
Adding10$Value[[4]]
#COMPLETD.  
#Cycling 
Adding192<- Addvalue(Adding10,192)
Adding192
Addingsane<- Addvalue(Adding192,Adding192$Name[[2]])
Addingsane
#Addingsane added Linda to the end of value list AND transformed
#The VALUE VARIABLE to a chr vector.

Adding20<- Adding10$Value[[3]]^Adding10$Value[[1]]
Adding20
#Returns inf, sweet!
#########################END LISTS

#COMMANDS
ls() #List objects in the environment
obj<- ls()
obj[3]

Basiclist
Basiclist$sortval<- sort(Basiclist$Value)
Basiclist$valindi<- order(Basiclist$Value)
#Sort the frame on value


#-----------------------------stats functions-----------------------------
#Clear workspace.  
rm(list=ls())
rnormal<- rnorm(100) #Random values on the standard normal 
hist(rnormal)

#Simple correlation
core<- cor(df1,df2)


####TO-DO - STANDARD REQUIREMENTS
#STANDARD script should be able to rip out each variable in a dataset, and graph
#Against every other variable and spit the resulting plots into a folder 


####SCAN FUNCTIONality
#PLAYING.

#SCAN:  TEXT FILE
scantext<- scan("C:/rhandler/test1.txt", what="")
scantext
#scantext is now a vector of strings , one for each word, or contiguous string 

#We can also scan in one line at a time for each vector
scanlines<- scan("C:/rhandler/test1.txt", what="", sep="\n")
scanlines

#attempting scan of a PDF
scanpdf<- scan("C:/rhandler/pfex.pdf",what="")
#FAILSON
#2-9-2016
#SCANNING A NOTES FILE FOR PROCESSING ADVANTAGE
yild<- scan("C:/rhandler/yild.txt",what="")
yildline<- scan("C:/rhandler/yild.txt",what="",sep="\n")
#COUNT the number of elements across clusters
#EX. Vitality survey data.
v14<- read.table("C:/rhandler/v142.csv", sep=",")
v14
str(v14)
#V1 is a factor with 159 levels.  Obs are clustered within V1.
#Isolating for count.

#Problem - whitespace trails remain
q15t$cty<- trimws(q15t$county, which="both")


#clean - up this section
cnames<- v14$V1
str(cnames)
cnames[114]
test1<- table(cnames)
test1
str(test1)
test1[14]
dftest1<- data.frame(test1)
dftest1
write.table(dftest1, "C:/rhandler/vitalitycount.csv", sep=",")
#Dftest 1 is a nice frequency table of county vitality responses.
test2<- table(v14$V1, v14$V39)
test2
v142<- read.table("C:/rhandler/v14_2.csv")
test3<- table(v142$V1, v142$V2)
v142
tes4<- ave(v14$V1, v14$V39)
tes4
bar2<- barplot(dftest1$Freq,col="#1437ac", ylab="How many members")
d14<- data.frame(v14)
d14
d14[1]
#CLOSE CLEANUP SECTION
#LOOPS

#Using "scanlines" variable above
#Attempting to loop across the scanlines file and isolate all vectors containing a)
fattempt<- function(x) {
  aloc<- grep("a)", x)
  extract<- x[]
}

#Element testing
aloc<- grep("a)", scanlines)
avec<- scanlines[aloc] #This performs the job very cleanly.  

#GREP:  Exact matching
gblocation421<- grep(searchgb[421], nzane,fixed=T)
#STANDARD.

extraction<- list()
extraction$avalues<- rep(NA, length(x)) #empty list item 

element1<- rep(0, length(scanlines))

#End Loops

#-------------------------TO DO:  Recode variables functions-------------#


#-----------------------------END stats functions-----------------------------
#------------------------------PLOTTING-----------------------------------
#histogram - basic r (no packs )++++++++++++
#++++++++++++++++++++++++++++++++++++++++++
hdata<- c(3,5,7,1,3)
hist1<- hist(hdata, col="#8bb8ea",ylab="Number of students")
hdata2<- seq(from=75, to=100, by=2.2)
hdata2
hist2<- hist(hdata2, col="#8bb8ea", ylab="Number of students", xlab="Test Score")
#Histogram, baby blue coloration.


#BAR PLOT - BASIC R (no packages)+++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++
#These are the frequencies for the bar chart.
colheight<- c(12,9,4,3,0)
#Barplot with names, COLORED BARS
bar2<- barplot(colheight, names.arg=c(5,4,3,2,1), col="#1437ac", 
               xlab="Quiz Score", ylab="How many students")

#Line graph of values across time
#KEY IS THE TYPE="l" command
xaxis<- seq(0, 100, length=100)
line1<- plot(xaxis, (xaxis)^2, ylab="X-squared", xlab="Sequence", type="l")
lines(xaxis, cos(xaxis), lty=3)

#Scatter plot
spam<- rnorm(200)
sick<- rnorm(200)

#Scatter plot with linear trend line
ill<- plot(sick, spam, ylab="Spam Intake", xlab="Food-Borne Illnesses")
abline(lm(sick ~ spam))

ill<- plot(sick, spam, ylab="Spam Intake", xlab="Food-Borne Illnesses")
abline(lm(sick ~ spam*spam))

#$####################GGPLOT EXAMPLES------------------
bas<-  ggplot(ac1, aes(x=ac1$year,y=ac1$amount,colour=ac1$fcode)) +
    geom_point(size=3,shape=15) +
    geom_line() + 
    theme(axis.line.x = element_line(size=1,colour="black",linetype="solid")) + 
    theme(axis.line.y = element_line(size=1,colour="black",linetype="solid")) + 
    theme(panel.background=element_rect(fill='white',colour='white')) + 
    labs(title=ac1$county[1],x="Year",y="Leveraged Cash ($)") +
    scale_x_continuous(breaks=c(seq(2005,2015,by=1))) 
#Geom line connects the pts here
#Axis line options
#Change bgcolor to white
#Set breaks for the x axis
#NOTE:  You need these commands so that ggplot does not "ensure" that the data are set far from the axes
scale_x_discrete(expand=c(0,0)) + 
scale_y_continuous(expand=c(0,0)) +

#ggplot histograms

#Data frames###########################################

#Create an empty data frame.
dtf1<- data.frame()
#Add a variable
dtf1$dollar<- c("$400", "$100", "$99,999", "$3.50")
#This doesn't work, you have to rbind
dtf1<- rbind(dtf1, c("$400", "$100", "$99,999", "$3.50"))
str(dtf1) 
#Each variable now called X..400, X..100, etc.

#This badass script creates a cloned data frame that drops variable names listed in the vector dv
c2<- clone[ , !(names(clone) %in% dv)]
  

########DATA MANAGEMENT#######################
######RESHAPE
library(reshape)
#Attempting to melt test1fix by ID variable (FN)
reshape1<- melt(test1fix, id=c("V1"), measure=c("V6", "V7"), 
                variable_name="Originals")
  #RESULT: Appears to have grouped V6 and V7 responses in long format
str(reshape1) #Structure confirmed.  
#LEARNING??????????????????????????????????????????????????????????

#SEALED......STANDARD.#SEALED......STANDARD.#SEALED......STANDARD.#SEALED......STANDARD.
#Now we need to compute the test score by summing V6 and V7 within ID
#And dividing the products to derive the % correct.

#Format for cast is ID ~ sum?
castcheck<- cast(reshape1, Originals ~ value, sum)
#Chick example 10-23-2015
head(ChickWeight)
chick_m<- melt(ChickWeight, id=2:4, na.rm=TRUE)
head(chick_m)
  #The id=2:4 identifies time chick and diet as id variables?
  #The variable "weight" is now a row value in chick_m
  #The value of weight is now given by the variable "value" in the row

cast1<- cast(chick_m, Time ~ variable, mean)
  #Cast 1 is now a simplified table containing all values of time, 
  #And the mean weight within each time


#R Markdown

#INCLUDING FIGURES#############################

<<c1fig,fig=T>>=
yyy<- read.table("C:/Sustainability/GETFISCAL.csv",sep=",",stringsAsFactors=F)
clist<- unique(yyy$county)
c1<- yyy[yyy$county==clist[1],]
ac1<- c1[c1$amount>0,]
plot(ac1$year,ac1$amount)
@
  #############################################
