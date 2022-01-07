
##  GOAL OF THE WORKSPACE:
##
##                 VALIDATION OF THE 2019.01.14 apsis_val3.3a RUN

## Data downloaded and unzipped from Gaiaweb-ops
## 
## dumped astrometric and integrated photometric data and DSC classification products
## into ascii files and stored in
## Users/sueveges/Documents/DSC/data/apsis_val3.1c/ascii/*.ascii

## column true_SourceType in the dataframes:
## -3 : qso candidate from CU7
## -2 : WD-lowmass star binaries from Silvestri et al.
## -1 : WD-MS binaries
##  0 : star
##  1 : WD
##  2 : binary
##  3 : non-physical binary
##  4 : qso
##  5 : galaxy 

# load("results_validation1/gaia0.RObj")
# load("results_validation1/gaia1.RObj")
# load("results_validation1/spectra0.RObj")


setwd("/Users/sueveges/Documents/DSC/R/cycle3/ValidationData/apsis_val3.3a/")

WORKSPACE.NAME <- "validation1.RData"
# save.image(WORKSPACE.NAME)

pathData <- "/Users/sueveges/Development/data/cycle3/apsis_val3.3a/output/"
pathData2 <- "/Users/sueveges/Development/data/cycle3/apsis_val3.3a/output2/"
pathValXMLog <- "/Users/sueveges/Development/data/cycle3/ingestion_20180607/DSC/xmatch_output/"
pathQsoXMLog <- "/Users/sueveges/Development/data/cycle3/ingestion_20180607/DSC/xmatch_output_qso/"
resultPath <- "/Users/sueveges/Documents/DSC/R/cycle3/ValidationData/apsis_val3.3a/results_validation1/"
imagePath <- "/Users/sueveges/Documents/DSC/R/cycle3/ValidationData/apsis_val3.3a/images_validation1/"


cu8class.str <- c("Star", "WD", "Binary", "Quasar", "Galaxy")
cu8class.names <- as.character(c(0,1,2,4,5))
cu8class.nums <- c(0,1,2,4,5)

cu8class.str.all <- c("Catacl.", "CU7 QSO", "WD-Mdw bin.", "WD-MS bin.", "Star", "WD", "Binary", "Quasar", "Galaxy")
cu8class.names.all <- as.character(c(-4,-3,-2,-1,0,1,2,4,5))
cu8class.nums.all <- c(-4,-3,-2,-1,0,1,2,4,5)

library(mapproj)
library(maps)
library(gplots)



###########################################################################################################
## --------------------------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------------------------
## Data
## --------------------------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------------------------
###########################################################################################################


## --------------------------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------------------------
## The catalogs
## --------------------------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------------------------

## --------------------------------------------------------------------------------------------------------
## XM between Gaiaid and DSCid
## --------------------------------------------------------------------------------------------------------

logf.names.lst <- list.files(pathValXMLog, pattern = ".log") 
   
ii <- 1
all.logs1 <- read.table(paste0(pathValXMLog, logf.names.lst[ii]), row.names = NULL, col.names = c(
	   "Identifier","RAin","DECin","RAout","DECout","PM_RAout","PM_DECout","separation","GmeanMag",
	   "SourceId","NbpObs","NrpObs","minus2lnprob","expectedGmag","true_SourceType","origin",
	   "module"), colClasses = c("character",rep("numeric",8),
       "character",rep("numeric",5),rep("character",2)), stringsAsFactors = FALSE)
for(ii in 2:length(logf.names.lst))
   {
   	d.tmp <- read.table(paste0(pathValXMLog, logf.names.lst[ii]), row.names = NULL, col.names = c(
	   "Identifier","RAin","DECin","RAout","DECout","PM_RAout","PM_DECout","separation","GmeanMag",
	   "SourceId","NbpObs","NrpObs","minus2lnprob","expectedGmag","true_SourceType","origin",
	   "module"), colClasses = c("character",rep("numeric",8),
       "character",rep("numeric",5),rep("character",2)), stringsAsFactors = FALSE)
   	all.logs1 <- rbind(all.logs1, d.tmp)
	cat(ii, "done \n")
   }

all.logs2 <- all.logs1[, c("SourceId","Identifier","RAout","DECout","PM_RAout","PM_DECout",
   "separation","GmeanMag","expectedGmag","true_SourceType")]
rm(all.logs1)
gc()
save(all.logs2, file = "results_validation1/all.logs2")

sum(duplicated(all.logs2$SourceId))
## 39275
sum(duplicated(all.logs2$Identifier))
## 490514
m.tmp <- unique(all.logs2$SourceId[duplicated(all.logs2$SourceId)]) 
n.tmp <- unique(all.logs2$Identifier[duplicated(all.logs2$Identifier)]) 
for(ii in 1:10)
   {
   	print(all.logs2[all.logs2$Identifier == n.tmp[ii], c("SourceId","Identifier","true_SourceType","separation")])
   }
## Lots of cross-identifications. Aaarrgh. Did I do again something stupid, e.g. not using the 
## right coordinate ref epoch?
all.logs4 <- all.logs2[order(all.logs2$Identifier, all.logs2$separation),]
cbind(all.logs4[1:100, c(1,2,7:9)], duplicated(all.logs4$Identifier[1:100]))

all.logs5 <- all.logs4[!duplicated(all.logs4$Identifier),]

rm(all.logs2, all.logs3, all.logs4)

sum(duplicated(all.logs5$SourceId))
## 8748
## Best maybe to remove all these.
m.tmp <- unique(all.logs5$SourceId[duplicated(all.logs5$SourceId)])
for(ii in 1:10)
   {
   	print(all.logs5[all.logs5$SourceId == m.tmp[ii], ])
   }
## There is a case when about 50 sources got the same crossmatched gaia object (m.tmp[1]).
## They have similar expected gmags (around 12) when nonNA, and they are all stars.
## Indeed, it seems to be the best idea to remove all these objects.

all.logs6 <- all.logs5[!is.element(all.logs5$SourceId, m.tmp),]


## --------------------------------------------------------------------------------------------------------
## Training and test set lists
## --------------------------------------------------------------------------------------------------------

t.tmp <- read.table("input_tables/TestingData_testlist.csv", sep = ",", skip = 1, col.names = 
   c("sourceId", "G", "BP", "RP"), colClasses = c("character", rep("numeric", 3))) 
sourceids.test <- t.tmp$sourceId
t.tmp <- read.table("input_tables/TrainingData_trainlist.csv", sep = ",", skip = 1, col.names = 
   c("sourceId", "sourceType", "isNullCalPhotSorce", "G", "BP", "RP"), colClasses = c("character", 
   "numeric", "character", rep("numeric", 3))) 
sourceids.train <- t.tmp$sourceId
rm(t.tmp)

sum(duplicated(sourceids.test))
sum(duplicated(sourceids.train))
sourceids.test <- unique(sourceids.test)
sourceids.train <- unique(sourceids.train)

## Add a column indicating training/test sets  to all.logs6:

all.logs6$testset <- TRUE
all.logs6$testset[is.element(all.logs6$SourceId, sourceids.train)] <- FALSE


## --------------------------------------------------------------------------------------------------------
## Input catalogs for the crossmatch (containing all the info, but mostly the sourceids of CU7 quasars)
## --------------------------------------------------------------------------------------------------------

dsccat <- read.table("/Users/sueveges/Documents/DSC/ValidationTable/18_07_04_Extension_CU7QSO_SimbadSingleStar_Simbad_Catacl/DSCValidationTable_2018_07_20.csv", sep = ",", header = TRUE,
   stringsAsFactors = FALSE, colClasses = c("character", rep("numeric",3),rep("character",2),
   "numeric",rep("character",2)))
cu7qcat <- read.table("/Users/sueveges/Documents/DSC/ValidationTable/18_07_04_Extension_CU7QSO_SimbadSingleStar_Simbad_Catacl/DSC_QSOcandCU7_2018_07_20.csv", sep = ",", header = TRUE,
   stringsAsFactors = FALSE, colClasses = c("character", rep("numeric",3),rep("character",2),
   "numeric",rep("character",2)))

range(as.numeric(all.logs6$Identifier))
range(as.numeric(cu7qcat$ID))
range(as.numeric(dsccat$ID))
## There are no CU7 quasars in this run (of course, I got them from a data request from last time,
## and did not include them in the validation list sources.)

d.tmp <- merge(all.logs6, dsccat, by.x = "Identifier", by.y = "ID")

dim(d.tmp)
## [1] 1098711      19
dim(all.logs6)
## [1] 1098711      11
dim(dsccat)
## [1] 1224090       9

identical(d.tmp$true_SourceType.x, d.tmp$true_SourceType.y)
## TRUE, fine.

dscinfo <- d.tmp[, -c(14,17)]
colnames(dscinfo)[c(9,10)] <- c("expectedGmag","true_SourceType")

rm(list = ls(pattern = ".tmp"))

save(all.logs6, file = "results_validation1/all.logs6.RObj")
save(dscinfo, file = "results_validation1/dscinfo.RObj")

rm(dsccat, cu7qcat, all.logs6)


## --------------------------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------------------------
## Merge the info with the run output
## --------------------------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------------------------

## --------------------------------------------------------------------------------------------------------
## All validation objects, without spectra
## --------------------------------------------------------------------------------------------------------

output.names <- list.files(pathData, pattern = ".txt") 
d.tmp <- read.table(paste0(pathData, output.names[1]), col.names = c("sourceid",
   "alpha", "alphaStarErr", "delta", "deltaErr", "muAlphaStar", "muAlphaStarErr", "muDelta", "muDeltaErr", 
    "classlabel", "Gflux", "GfluxErr", "Gmag", "Gmagerr", "Bpflux", "BpfluxErr", 
    "Bpmag", "Bpmagerr", "Rpflux", "RpfluxErr", "Rpmag", "Rpmagerr", "outlierflag", 
    "weakflag", paste0("posprob", 0:5), paste0("astroprob", 0:5), paste0("etcprob", 0:5), 
    paste0("etsprob", 0:5)), colClasses = c("character", rep("numeric", 21), 
    rep("character", 2), rep("numeric", 24)), stringsAsFactors = F, na.strings = 
    c("NA","NaN"), skip = 1)
gaia1 <- merge(d.tmp, dscinfo, by.x = "sourceid", by.y = "SourceId")
for(ii in 5:length(output.names))
#for(ii in 2:4)
   {
   	s.tmp <- system.time({
   	d.tmp <- read.table(paste0(pathData, output.names[ii]), col.names = c("sourceid",
   "alpha", "alphaStarErr", "delta", "deltaErr", "muAlphaStar", "muAlphaStarErr", "muDelta", "muDeltaErr", 
    "classlabel", "Gflux", "GfluxErr", "Gmag", "Gmagerr", "Bpflux", "BpfluxErr", 
    "Bpmag", "Bpmagerr", "Rpflux", "RpfluxErr", "Rpmag", "Rpmagerr", "outlierflag", 
    "weakflag", paste0("posprob", 0:5), paste0("astroprob", 0:5), paste0("etcprob", 0:5), 
    paste0("etsprob", 0:5)), colClasses = c("character", rep("numeric", 21), 
    rep("character", 2), rep("numeric", 24)), stringsAsFactors = F, na.strings = 
	    c("NA","NaN"), skip = 1)
	e.tmp <- merge(d.tmp, dscinfo, by.x = "sourceid", by.y = "SourceId")
	colnames(e.tmp)[1] <- "sourceid"  
	gaia1 <- rbind(gaia1, e.tmp) 
	})[3]
	cat(ii, "is ready: time", s.tmp, "\n")
   }

gaia2 <- gaia1[, c(1,49,2:48,54,56:64)]
gaia3 <- gaia2[, c(1:10,54,55,12:23,51,50,56:59,52,53,26:49)]

identical(gaia1$Gmag, gaia1$GmeanMag)
colnames(gaia2)[c(1:10,54,55,12:23,51,50,56:59,52,53,26:49)]

gaia3$ets.class <- apply(gaia3[,51:56], 1, function(vec) 
   {
	v.tmp <- which(max(vec) == vec)
	if(length(v.tmp) == 1) {v.tmp - 1} else {return(NA)}
   })
gaia3$etc.class <- apply(gaia3[,45:50], 1, function(vec) 
   {
	v.tmp <- which(max(vec) == vec)
	if(length(v.tmp) == 1) {v.tmp - 1} else {return(NA)}
   })
gaia3$astro.class <- apply(gaia3[,39:44], 1, function(vec) 
   {
	v.tmp <- which(max(vec) == vec)
	if(length(v.tmp) == 1) {v.tmp - 1} else {return(NA)}
   })
gaia3$pos.class <- apply(gaia3[,33:38], 1, function(vec) 
   {
	v.tmp <- which(max(vec) == vec)
	if(length(v.tmp) == 1) {v.tmp - 1} else {return(NA)}
   })

table(gaia3$true_SourceType[gaia3$testset], gaia3$ets.class[gaia3$testset], useNA = "a")
table(gaia3$true_SourceType[gaia3$testset], gaia3$etc.class[gaia3$testset], useNA = "a")
table(gaia3$true_SourceType[gaia3$testset], gaia3$astro.class[gaia3$testset], useNA = "a")
table(gaia3$true_SourceType[gaia3$testset], gaia3$pos.class[gaia3$testset], useNA = "a")

t.tmp <- table(gaia3$true_SourceType[gaia3$testset], gaia3$ets.class[gaia3$testset])[4:8,]
diag(t.tmp)/apply(t.tmp, 1, sum)

write.table(gaia3, file = "results_validation1/dscval_apsis_val3.3a.txt", quote = FALSE,
   sep = ",", col.names = FALSE, row.names = FALSE)  
   
gaia0 <- read.table("results_validation1/dscval_apsis_val3.3a.txt", skip = 31,
   sep = ",", colClasses = c("character", rep("numeric",25), rep("character",4),
   "numeric", "character", rep("numeric",28)), col.names = c("sourceid","Identifier",
   "Ra","alphaStarErr","Dec","deltaErr","muAlphaStar","muAlphaStarErr",
   "muDelta","muDeltaErr","RA","DEC","Gflux","GfluxErr","Gmag","Gmagerr",
   "Bpflux","BpfluxErr","Bpmag","Bpmagerr","Rpflux","RpfluxErr","Rpmag",
   "Rpmagerr","expectedGmag","separation","name","lit_class","origin","module",
   "true_SourceType","testset","posprob0","posprob1","posprob2","posprob3",
   "posprob4","posprob5","astroprob0","astroprob1","astroprob2","astroprob3",
   "astroprob4","astroprob5","etcprob0","etcprob1","etcprob2","etcprob3","
   etcprob4","etcprob5","etsprob0","etsprob1","etsprob2","etsprob3","etsprob4",
   "etsprob5","ets.class","etc.class","astro.class","pos.class")) 
save(gaia0, file = "results_validation1/gaia0.RObj")
gaia0 <- gaia1
gaia0$pm <- sqrt(gaia0$muAlphaStar^2 + gaia0$muDelta^2)

gaia1 <- gaia0[gaia0$testset == "TRUE",]
colnames(gaia1)[c(3,5)] <- c("Ra", "Dec")
gaia.bckp <- gaia1

table(gaia0$origin[gaia0$Gmag > 20.5], gaia0$true_SourceType[gaia0$Gmag > 20.5])
sum(gaia0$true_SourceType == 4 & gaia0$Gmag > 20.5 & is.element(gaia0$origin, 
   c("CU7_DGDREQ44_ALLWISE","random_SDSS_DR12","top300000_SDSS_DR14_qso")))
sum(gaia0$true_SourceType == 4 & gaia0$Gmag > 20.5 & is.element(gaia0$origin, 
   c("CU7_DGDREQ44_ALLWISE")))
sum(gaia0$true_SourceType == 4 & gaia0$Gmag > 20.5 & is.element(gaia0$origin, 
   c("random_SDSS_DR12","top300000_SDSS_DR14_qso")))
sum(gaia0$true_SourceType == 5 & gaia0$Gmag > 20.5 & is.element(gaia0$origin, 
   c("top300000_SDSS_DR14_gal")))

table(gaia0$origin[gaia0$Bpmag - gaia0$Rpmag > 5 & gaia0$true_SourceType == 0])
table(gaia0$lit_class[gaia0$Bpmag - gaia0$Rpmag > 5 & gaia0$true_SourceType == 0 & 
   gaia0$origin == "SimbadSingleStars"])

sum(gaia1$Gmag < 7.5 & gaia1$true_SourceType == 0)
sum(gaia1$Gmag < 7.5 & gaia1$true_SourceType == 2)

gaia1$ets.class[gaia1$Gmag < 7.5 & gaia1$true_SourceType == 0]
## Almost all NA.
gaia1$ets.class[gaia1$Gmag < 7.5 & gaia1$true_SourceType == 2]
## Almost all NA.
colnames(gaia1)
gaia1[gaia1$Gmag < 7.5 & gaia1$true_SourceType == 0, paste0("etsprob", c(0,1,2,4,5))]
## Almost all NA. It appears that these were not processed by the ET.
gaia1[gaia1$Gmag < 7.5 & gaia1$true_SourceType == 0, paste0("astroprob", c(0,1,2,4,5))]
gaia1[gaia1$Gmag < 7.5 & gaia1$true_SourceType == 0, paste0("posprob", c(0,1,2,4,5))]
## These produced results.




## --------------------------------------------------------------------------------------------------------
## Small sample, with spectra
## --------------------------------------------------------------------------------------------------------

output.names2 <- list.files(pathData2, pattern = ".txt") 
d.tmp <- read.table(paste0(pathData2, output.names2[1]), col.names = c("sourceid",
   "alpha", "alphaStarErr", "delta", "deltaErr", "muAlphaStar", "muAlphaStarErr", "muDelta", "muDeltaErr", 
    "varPi", "varPiErr", "classlabel", "Gflux", "GfluxErr", "Gmag", "Gmagerr", "Bpflux", "BpfluxErr", 
    "Bpmag", "Bpmagerr", "Rpflux", "RpfluxErr", "Rpmag", "Rpmagerr", paste0("bpFlux", 25:96), 
    paste0("bpFluxErr", 25:96), paste0("rpFlux", 25:96), paste0("rpFluxErr", 25:96), 
    paste0("posprob", 0:5), paste0("astroprob", 0:5), paste0("etcprob", 0:5), 
    paste0("etsprob", 0:5)), colClasses = c("character", rep("numeric", 21), 
    rep("character", 2), rep("numeric", 24)), stringsAsFactors = F, na.strings = 
    c("NA","NaN"), skip = 1)
spectra1 <- merge(d.tmp, dscinfo, by.x = "sourceid", by.y = "SourceId")
for(ii in 2:length(output.names2))
#for(ii in 2:4)
   {
   	s.tmp <- system.time({
   	d.tmp <- read.table(paste0(pathData2, output.names2[ii]), col.names = c("sourceid",
	   "alpha", "alphaStarErr", "delta", "deltaErr", "muAlphaStar", "muAlphaStarErr", "muDelta", "muDeltaErr", 
	    "varPi", "varPiErr", "classlabel", "Gflux", "GfluxErr", "Gmag", "Gmagerr", "Bpflux", "BpfluxErr", 
	    "Bpmag", "Bpmagerr", "Rpflux", "RpfluxErr", "Rpmag", "Rpmagerr", paste0("bpFlux", 25:96), 
	    paste0("bpFluxErr", 25:96), paste0("rpFlux", 25:96), paste0("rpFluxErr", 25:96), 
	    paste0("posprob", 0:5), paste0("astroprob", 0:5), paste0("etcprob", 0:5), 
	    paste0("etsprob", 0:5)), colClasses = c("character", rep("numeric", 21), 
	    rep("character", 2), rep("numeric", 24)), stringsAsFactors = F, na.strings = 
	    c("NA","NaN"), skip = 1)
	e.tmp <- merge(d.tmp, dscinfo, by.x = "sourceid", by.y = "SourceId")
	colnames(e.tmp)[1] <- "sourceid"  
	spectra1 <- rbind(spectra1, e.tmp) 
	})[3]
	cat(ii, "is ready: time", s.tmp, "\n")
   }

spectra1$ets.class <- apply(spectra1[,331:336], 1, function(vec) 
   {
	v.tmp <- which(max(vec) == vec)
	if(length(v.tmp) == 1) {v.tmp - 1} else {return(NA)}
   })
spectra1$etc.class <- apply(spectra1[,325:330], 1, function(vec) 
   {
	v.tmp <- which(max(vec) == vec)
	if(length(v.tmp) == 1) {v.tmp - 1} else {return(NA)}
   })
spectra1$astro.class <- apply(spectra1[,319:324], 1, function(vec) 
   {
	v.tmp <- which(max(vec) == vec)
	if(length(v.tmp) == 1) {v.tmp - 1} else {return(NA)}
   })
spectra1$pos.class <- apply(spectra1[,313:318], 1, function(vec) 
   {
	v.tmp <- which(max(vec) == vec)
	if(length(v.tmp) == 1) {v.tmp - 1} else {return(NA)}
   })

spectra0 <- spectra1
save(spectra0, file = "results_validation1/spectra0.RObj")
spectra1 <- spectra0[spectra0$testset == "TRUE",]
save(spectra1, file = "results_validation1/spectra1.RObj")
rm(spectra0)
rm(gaia0)



## --------------------------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------------------------
## Plotting functions
## --------------------------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------------------------

## --------------------------------------------------------------------------------------------------------
## Confusion matrix
## --------------------------------------------------------------------------------------------------------

## confMat: a confusion matrix, in which the rows correspond the true labels, and the
## columns represent the estimated labels. na.col is an indicator if there exists a 
## column for the number of NA labels; if there are n classes, it is supposed to be the 
## n+1th column in confMat.
matPlot.fun <- function(confMat, need.ylabs = TRUE, need.xlabs = TRUE) {
	confMat1 <- confMat
	colNames = colnames(confMat1)
	rowNames = rownames(confMat1)
	numCol = length(colNames)
	numRow = length(rowNames)
	totalRow <- rep(NA,numRow)
	totalCol <- rep(NA,numCol)
	for (i in 1:numRow) {
		totalRow[i] <- sum(confMat1[i,1:numCol])	
	}
	for (i in 1:numCol) {
		colsum <- sum(confMat1[1:numRow,i])	
		if (colsum>0) {
			totalCol[i] <- colsum # to avoid writing zero in contamination if there is no entry
		}
	}
    ## Computing the contamination:
	## the following loop only applies to square matrices. If CM is not square, contamination 	
	## is not computed (it depends on classes)	
	contamination <- numeric(numCol)
	dashes <- rep("",numCol)
	for (i in 1:numCol) {
	    contamination[i] <- (sum(confMat1[1:numRow,i])-confMat[i,i]) / sum(confMat1[1:numRow,i])
	    if (is.na(contamination[i])) {
		  dashes[i] <- "-"
	    }
	  }
    ## Computing the classwise error rate:
	err.rate <- numeric(numRow)
	dashes <- rep("",numRow)
	for (i in 1:numRow) {
	    err.rate[i] <- (sum(confMat1[i,1:numCol])-confMat1[i,i]) / sum(confMat1[i,1:numCol])
	    if (is.na(err.rate[i])) {
		  dashes[i] <- "-"
	    }
	  }
	#mat <- t(apply(confMat, MAR = 2, FUN = function(v) {return(rev(v))}))
	## The numbers that should be written into the boxes of the 'image':
	mat1 <- matrix(nrow = nrow(confMat1) + 1, ncol = ncol(confMat1) + 1) 
	mat1[1:(nrow(mat1)-1), 1:(nrow(mat1)-1)] <- confMat1
	mat1[nrow(mat1), 1:(nrow(mat1)-1)] <- contamination
	mat1[1:(nrow(mat1)-1), ncol(mat1)] <- err.rate
	## An empty matrix in the place of the confusion matrix, so that it does not
	## get colored by 'image', only the contamination row and the classwise error
	## column:
	mat2 <- matrix(nrow = nrow(mat1), ncol = ncol(mat1)) 
	mat2[nrow(mat2), 1:(nrow(mat2)-1)] <- contamination
	mat2[1:(nrow(mat2)-1), nrow(mat2)] <- err.rate
	mat1 <- t(apply(mat1, MAR = 2, FUN = function(v) {return(rev(v))}))
	mat2 <- t(apply(mat2, MAR = 2, FUN = function(v) {return(rev(v))}))
	
	## I would like to see regular fonts in the confusion matrix, but boldface in
	## the lines corresponding to the contamination and the classwise errors:
	textfonts <- matrix(1, nrow = nrow(confMat1) + 1, ncol = ncol(confMat1) + 1)
	textfonts[nrow(mat2), 1:(nrow(mat2)-1)] <- 2
	textfonts[1:(nrow(mat2)-1), nrow(mat2)] <- 2
	textfonts <- t(apply(textfonts, MAR = 2, FUN = function(v) {return(rev(v))}))

	image(seq(numCol+1),seq(numRow+1), mat2, breaks = seq(0, 1, by = 0.01),
	    col = rgb(red = 1, green = 0.4, blue = 0, alpha = seq(0.05, 0.95, length.out = 100)),
	    xaxt="n", yaxt="n", xlab="", ylab="", axes = F)
	grid(nx=numCol+1,ny=numRow+1,lty="solid")
	abline(v = numCol+0.5, lty = "solid")
	abline(h = 1.5, lty = "solid")
	abline(v = 0.5, lty = "solid")
	abline(h = numRow+1.5, lty = "solid")
	segments(0.5,0.5,numCol+0.5,0.5, lwd = 2)
	segments(numCol+1.5, 1.5, numCol+1.5, numCol+1.5, lwd = 2)
	for(ii in 1:(numCol+1))
	    for(jj in 1:(numRow+1))
			text(ii,jj, round(mat1[ii,jj], 2), font = textfonts[ii,jj])
	if(need.ylabs) 
	   {
		axis(2, at = 2:(numRow+1), labels = rev(rownames(confMat1)), tick = F, las = 1,
		   font = 1, line = -0.5)
		axis(2, at = 1, labels = "Contam.", tick = F, las = 1, font = 2, line = -0.5)
	   }
	if(need.xlabs)
	   {
		axis(3, at = 1:numCol, labels = colnames(confMat1), tick = F, las = 3, font = 1,
		   line = -0.5)
		axis(3, at = numCol+1, labels = "Error", tick = F, las = 3, font = 2, line = -0.5)
	   }
}

## --------------------------------------------------------------------------------------------------------
## Table
## --------------------------------------------------------------------------------------------------------

tablePlot.fun <- function(confMat, need.ylabs = TRUE, need.xlabs = TRUE) {
	colNames = colnames(confMat)
	rowNames = rownames(confMat)
	numCol = length(colNames)
	numRow = length(rowNames)
	## An empty matrix in the place of the confusion matrix, so that it does not
	## get colored by 'image', only the contamination row and the classwise error
	## column:
	mat1 <- t(apply(confMat, MAR = 2, FUN = function(v) {return(rev(v))}))
	mat2 <- matrix(1, nrow = nrow(mat1), ncol = ncol(mat1)) 
	mat2 <- t(apply(mat2, MAR = 2, FUN = function(v) {return(rev(v))}))
	
	image(seq(numCol),seq(numRow), mat2, breaks = c(0:100)/100,
	    col = rgb(red = 1, green = 1, blue = 1, alpha = seq(0.005, 0.995, length.out = 100)),
	    xaxt="n", yaxt="n", xlab="", ylab="")
	grid(nx=numCol,ny=numRow,lty="solid")
	for(ii in 1:(numCol))
	    for(jj in 1:(numRow))
			text(ii,jj, round(mat1[ii,jj], 2))
	if(need.ylabs) 
	   {
		axis(2, at = 1:(numRow), labels = rev(rownames(confMat)), tick = F, las = 1,
		   font = 1, line = -0.5)
	   }
	if(need.xlabs)
	   {
		axis(3, at = 1:numCol, labels = colnames(confMat), tick = F, las = 3, font = 1,
		   line = -0.5)
	   }
}


## --------------------------------------------------------------------------------------------------------
## Precision, recall, and loss rate
## --------------------------------------------------------------------------------------------------------

## thr: threshold on probability to predict a certain class
## whichclass: the predicted class in which contamination/accuracy is sought
## probvec: the probabilities of the predicted class
## truthvec: the true classes
## fromclass: the class contaminating the predicted class

precision.fun <- function(thr, whichclass, probvec, truthvec)
   {
   	np <- sum(probvec > thr, na.rm = T)
   	ntp <- sum((probvec > thr) & (truthvec == whichclass), na.rm = T)
   	return(ntp / np)
   }

recall.fun <- function(thr, whichclass, probvec, truthvec)
   {
   	np <- sum(truthvec == whichclass, na.rm = T)
   	ntp <- sum(probvec > thr & truthvec == whichclass, na.rm = T)
   	return(ntp / np)
   }

## Compute what fraction of a certain class was misclassified into another
## specific class:
loss.fun <- function(thr, fromclass, probvec, truthvec)
   {
   	nfrom <- sum(truthvec == fromclass, na.rm = T) 
   	npred <- sum(probvec > thr & truthvec == fromclass, na.rm = T)
   	return(npred / nfrom)
   }

precision.fun(thr = 0.95, 2, gaia1$etsprob2, gaia1$true_SourceType)
recall.fun(thr = 0.95, 2, gaia1$etsprob2, gaia1$true_SourceType)

p.tmp <- sapply(seq(0.2, 1, by = 0.01), precision.fun, whichclass = ii, gaia1[, paste0("etsprob", ii)],
   gaia1$true_SourceType)
r.tmp <- sapply(seq(0.2, 1, by = 0.01), recall.fun, whichclass = ii, gaia1[, paste0("etsprob", ii)],
   gaia1$true_SourceType)
plot(seq(0.2, 1, by = 0.01), p.tmp, type = "l", col = "blueviolet", xlab = "Probability threshold",
   ylab = "Fraction", lwd = 2, ylim = c(0,1)) 
lines(seq(0.2, 1, by = 0.01), r.tmp, col ="orangered", lwd = 2)


## --------------------------------------------------------------------------------------------------------
## WDs miscl. as gal.
## --------------------------------------------------------------------------------------------------------

wdgal.miscl.ids <- gaia1$sourceid[gaia1$true_SourceType == 1 & gaia1$ets.class == 5 &
   !is.na(gaia1$ets.class)]
wdgal.miscl.spectra1.ids <- intersect(wdgal.miscl.ids, spectra1$sourceid)

for(ii in 1:length(wdgal.miscl.spectra1.ids))
   {
   	n.tmp <- wdgal.miscl.spectra1.ids[ii]
   	v.tmp <- spectra1[spectra1$sourceid == n.tmp, ]
	pdf(paste0("images_validation1/WD-GALmisclass/BPRPspectrum_", ii, ".pdf"),  height = 3.5, width = 7.5)
   	par(mfcol = c(1,1), mar = c(2.5,2.5,2,1), mgp = c(1.6,0.6,0))
	plot(1:72, as.numeric(v.tmp[25:96]), col = "blue", lwd = 2, type = "l", 
	   xlim = c(0,153), ylim = range(as.numeric(v.tmp[c(25:96, 169:240)])), xlab = "Pixel",
	   ylab = "Flux")
	lines(81:152, as.numeric(v.tmp[169:240]), col = "red", lwd = 2)
	dev.off()
   }


spectraplot.fun <- function(sourceids, trueclass, estclass)
   {
   	v.tmp <- spectra1[is.element(spectra1$sourceid, sourceids), ]
   	par(mfcol = c(1,1), mar = c(2.5,2.5,2,1), mgp = c(1.6,0.6,0))
	plot(1:72, as.numeric(v.tmp[1, 25:96]), type = "n", 
	   xlim = c(0,153), ylim = range(as.numeric(as.matrix(v.tmp[, c(25:96, 169:240)]))), xlab = "Pixel",
	   ylab = "Flux", main = paste0("True class: ", trueclass, "     Est. class: ", estclass))
   	for(ii in 1:length(sourceids))
   	   {
		lines(1:72, as.numeric(v.tmp[ii, 25:96]), lwd = 1, col = "blue")
		lines(81:152, as.numeric(v.tmp[ii, 169:240]), lwd = 1, col = "red")
	   }
   }

pdf("images_validation1/WD-GALmisclass/BPRPspectra_wd-gal.pdf",  height = 3.5, width = 7.5)
spectraplot.fun(sourceids = wdgal.miscl.spectra1.ids, trueclass = 1, estclass = 5)
dev.off()

wdwd.ids <- gaia1$sourceid[gaia1$true_SourceType == 1 & gaia1$ets.class == 1 &
   !is.na(gaia1$ets.class)]
w.tmp <- intersect(wdwd.ids, spectra1$sourceid)
wdwd.spectra1.ids <- sample(w.tmp, min(length(w.tmp), 100))
pdf("images_validation1/WD-GALmisclass/BPRPspectra_wd-wd.pdf",  height = 3.5, width = 7.5)
spectraplot.fun(sourceids = wdwd.spectra1.ids, trueclass = 1, estclass = 1)
dev.off()

wdqso.ids <- gaia1$sourceid[gaia1$true_SourceType == 1 & gaia1$ets.class == 4 &
   !is.na(gaia1$ets.class)]
w.tmp <- intersect(wdqso.ids, spectra1$sourceid)
wdqso.spectra1.ids <- sample(w.tmp, min(length(w.tmp), 100))
pdf("images_validation1/WD-GALmisclass/BPRPspectra_wd-qso.pdf",  height = 3.5, width = 7.5)
spectraplot.fun(sourceids = wdqso.spectra1.ids, trueclass = 1, estclass = 4)
dev.off()

wdstar.ids <- gaia1$sourceid[gaia1$true_SourceType == 1 & gaia1$ets.class == 0 &
   !is.na(gaia1$ets.class)]
w.tmp <- intersect(wdstar.ids, spectra1$sourceid)
wdstar.spectra1.ids <- sample(w.tmp, min(length(w.tmp), 100))
pdf("images_validation1/WD-GALmisclass/BPRPspectra_wd-star.pdf",  height = 3.5, width = 7.5)
spectraplot.fun(sourceids = wdstar.spectra1.ids, trueclass = 1, estclass = 0)
dev.off()

wdbin.ids <- gaia1$sourceid[gaia1$true_SourceType == 1 & gaia1$ets.class == 2 &
   !is.na(gaia1$ets.class)]
w.tmp <- intersect(wdbin.ids, spectra1$sourceid)
wdbin.spectra1.ids <- sample(w.tmp, min(length(w.tmp), 100))
pdf("images_validation1/WD-GALmisclass/BPRPspectra_wd-bin.pdf",  height = 3.5, width = 7.5)
spectraplot.fun(sourceids = wdbin.spectra1.ids, trueclass = 1, estclass = 2)
dev.off()

galgal.ids <- gaia1$sourceid[gaia1$true_SourceType == 5 & gaia1$ets.class == 5 &
   !is.na(gaia1$ets.class)]
w.tmp <- intersect(galgal.ids, spectra1$sourceid)
galgal.spectra1.ids <- sample(w.tmp, min(length(w.tmp), 100))
pdf("images_validation1/WD-GALmisclass/BPRPspectra_gal-gal.pdf",  height = 3.5, width = 7.5)
spectraplot.fun(sourceids = galgal.spectra1.ids, trueclass = 5, estclass = 5)
dev.off()


for(ii in c(0,1,2,4,5))
   for(jj in c(0,1,2,4,5))
      {
      	i.tmp1 <- 
      }


quartz()
hist(gaia1$pm[gaia1$true_SourceType == 4], breaks = 1000, col = "lightgrey", xlim = c(0,100))
quartz()
hist(gaia1$pm[gaia1$true_SourceType == 5], breaks = 1000, col = "lightgrey", xlim = c(0,100))
quartz()
hist(gaia0$pm[gaia0$true_SourceType == 5], breaks = 1000, col = "lightgrey", xlim = c(0,100))



tapply(gaia1$pm[gaia1$true_SourceType == 0], gaia1$origin[gaia1$true_SourceType == 0], 
   range, na.rm = T)
table(gaia1$origin[gaia1$true_SourceType == 0 & gaia1$pm < 4])
table(gaia1$origin[gaia1$true_SourceType == 0 & gaia1$pm > 7])
table(gaia1$origin[gaia1$true_SourceType == 2 & gaia1$pm < 4])
table(gaia1$origin[gaia1$true_SourceType == 2 & gaia1$pm > 7])

table(gaia1$origin[gaia1$true_SourceType == 5 & gaia1$pm < 1])
gaia1[gaia1$true_SourceType == 5 & gaia1$pm < 1 & !is.na(gaia1$pm), c("origin", "muAlphaStar","muDelta")]




confmat.tmp <- table(factor(gaia1$true_SourceType, levels = c(-4,-2,-1,0,1,2,4,5)),
   factor(gaia1$ets.class, levels = c(0,1,2,4,5)), useNA = "n")[4:8,]
sum(diag(confmat.tmp)) / sum(confmat.tmp)


colnames(gaia1)[33:44]
apply(gaia1[,33:44], 2, summary)
apply(gaia1[,59:60], 2, summary)

which(apply(gaia1[,39:44], 1, function(vec) all(is.na(vec))) & !is.na(gaia1[,59]))
## None, this is fine
sum(apply(gaia1[,39:44], 1, function(vec) all(!is.na(vec))) & is.na(gaia1[,59]))
sum(apply(gaia1[,39:44], 1, function(vec) all(is.na(vec))) & is.na(gaia1[,59]))

## ProbVector is not NA, but the estimated class is NA from astrometry:
table(gaia1$true_SourceType[apply(gaia1[,39:44], 1, function(vec) all(!is.na(vec))) & is.na(gaia1[,59])])
##    -4     -2     -1      0      1      2      4      5 
##   138      7    104   4360    912   4695 361630   3019 
gaia1[(apply(gaia1[,39:44], 1, function(vec) all(!is.na(vec))) & is.na(gaia1[,59])), 3:10][21:50,]
## Checking the astrometry, the PM values are typically below 1 or similar, but the errors are 
## often comparable (so the estimates could be reasonably zero).
gaia1[(apply(gaia1[,39:44], 1, function(vec) all(!is.na(vec))) & is.na(gaia1[,59])), 39:44][21:50,]

## ProbVector is NA, so the estimated class is NA from astrometry:
table(gaia1$true_SourceType[apply(gaia1[,39:44], 1, function(vec) all(is.na(vec))) & is.na(gaia1[,59])])
##    -4     -2     -1      0      1      2      4      5 
##   865     27    189  20796   2961   7764  36320 161264 
gaia1[(apply(gaia1[,39:44], 1, function(vec) all(is.na(vec))) & is.na(gaia1[,59])), 3:10][21:50,]
## Checking the astrometry, either it is NA or there are PM values above mas and also its errors are
## of the order of mas, very often above 20%.

## ProbVector is not NA, the estimated class is not NA from astrometry:
table(gaia1$true_SourceType[apply(gaia1[,39:44], 1, function(vec) all(!is.na(vec))) & !is.na(gaia1[,59])])
##    -4     -2     -1      0      1      2      4      5 
##  1401    198   2721 221850  17358  61467  10209    808 
gaia1[(apply(gaia1[,39:44], 1, function(vec) all(!is.na(vec))) & !is.na(gaia1[,59])), 3:10][1:20,]
## Checking the astrometry, all seems to be ok, not too high errors in general.









