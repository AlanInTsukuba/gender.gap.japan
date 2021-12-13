#' DescriptiveStatistics.R
#' If not already in the workspace:
#' occgenall <- read.csv("Data/occgenall.csv", sep="\t",header=T)

#' sample size
with(occgenall[,c("CensusYear","value")], 
aggregate(value,
	list(CensusYear=occgenall$CensusYear),FUN="sum"))
 
#' using outer joins
tmp1985 <- occgenall[occgenall$CensusYear == "1985",c("OccMinor","CensusYear")]
tmp1990 <- occgenall[occgenall$CensusYear == "1990",c("OccMinor","CensusYear")]
tmp1995 <- occgenall[occgenall$CensusYear == "1995",c("OccMinor","CensusYear")]
tmp2000 <- occgenall[occgenall$CensusYear == "2000",c("OccMinor","CensusYear")]
tmp2005 <- occgenall[occgenall$CensusYear == "2005",c("OccMinor","CensusYear")]
head(tmp1985)
colSums(is.na(unique(merge(x = tmp1985 ,y = tmp1990 ,by="OccMinor",all=TRUE))))
colSums(is.na(unique(merge(x = tmp1985 ,y = tmp1995 ,by="OccMinor",all=TRUE))))
colSums(is.na(unique(merge(x = tmp1985 ,y = tmp2000 ,by="OccMinor",all=TRUE))))
colSums(is.na(unique(merge(x = tmp1985 ,y = tmp2005 ,by="OccMinor",all=TRUE))))
rm("tmp1985","tmp1990","tmp1995","tmp2000","tmp2005")

#' labor force participation
tmp <- with(occgenall,aggregate(occgenall$value, 
		list(CensusYear=occgenall$CensusYear,
			Gender=occgenall$Gender),
		FUN="sum"))
tmp <- with(tmp,cbind(tmp[Gender=="FEMALE",c("CensusYear","x")],
		tmp[Gender=="MALE","x"]))
colnames(tmp) <- c("CensusYear","Female","Male")
with(tmp,cbind(tmp, Female / (Female + Male)))
rm(tmp)

#' distribution of occupational major groups
#' These are in the OccMajor column of occgenall
#' First get the grant totals
grandtotal <- with(occgenall,aggregate(occgenall$value, 
		list(CensusYear=occgenall$CensusYear),
		FUN="sum"))
colnames(grandtotal) <- c("CensusYear","Total")

#' Next get the totals by major group
majorgrptotals <- with(occgenall,aggregate(occgenall$value, 
		list(OccMain=occgenall$OccMain,
			OccSub=occgenall$OccSub,
			CensusYear=occgenall$CensusYear),
		FUN="sum"))
colnames(majorgrptotals) <- c("OccMain","OccSub","CensusYear","GrpTotal")
head(majorgrptotals)

majorgrpshares <- merge(majorgrptotals, grandtotal)
majorgrpshares$GrpShare <- with(majorgrpshares, 100*GrpTotal/ Total)
head(majorgrpshares)

#' Get harmonized occupation classes for titles
hoc <- read.csv("Data/HarmonizedOccupationClasses1985-2005.csv",header=T)
head(hoc)
merge(majorgrpshares, hoc[hoc$MainTitle != "",c("OccMain","OccSub","MainTitle")])

tmptbl <- cbind(majorgrpshares[majorgrpshares$CensusYear==1985,c("OccMain","OccSub","GrpShare")], 
	majorgrpshares[majorgrpshares$CensusYear==1990,c("GrpShare")], 
	majorgrpshares[majorgrpshares$CensusYear==1995,c("GrpShare")], 
	majorgrpshares[majorgrpshares$CensusYear==2000,c("GrpShare")], 
	majorgrpshares[majorgrpshares$CensusYear==2005,c("GrpShare")])
colnames(tmptbl)  <- c("OccMain","OccSub","X1985","X1990","X1995","X2000","X2005")

tmp <- with(tmptbl,format(tmptbl[with(tmptbl, order(OccMain,OccSub)),], digits=2))
with(tmp, paste0("<tr><td>",OccMain,OccSub,"</td><td style='text-align: center;'>",X1985,"</td><td style='text-align: center;'>",X1990,"</td><td style='text-align: center;'>",X1995,"</td><td style='text-align: center;'>",X2000,"</td><td style='text-align: center;'>",X2005,"</td></tr>"))

##########################################################
#' Female shares by major group
head(majorgrpshares)

tmpoccgenf <- with(occgenall,occgenall[Gender=="FEMALE",])
head(tmpoccgenf )
majorgrpFshares <- with(tmpoccgenf,aggregate(tmpoccgenf$value, 
		list(OccMain=tmpoccgenf$OccMain,
			OccSub=tmpoccgenf$OccSub,
			CensusYear=tmpoccgenf$CensusYear),
		FUN="sum"))
colnames(majorgrpFshares) <- c("OccMain","OccSub","CensusYear","FemaleGrpTotal")
head(majorgrpFshares)

majorgrpshares <- merge(majorgrpshares,majorgrpFshares )
majorgrpshares$FemShare <- with(majorgrpshares, 100*FemaleGrpTotal/ GrpTotal)

tmptbl <- cbind(majorgrpshares[majorgrpshares$CensusYear==1985,c("OccMain","OccSub","FemShare")], 
	majorgrpshares[majorgrpshares$CensusYear==1990,c("FemShare")], 
	majorgrpshares[majorgrpshares$CensusYear==1995,c("FemShare")], 
	majorgrpshares[majorgrpshares$CensusYear==2000,c("FemShare")], 
	majorgrpshares[majorgrpshares$CensusYear==2005,c("FemShare")])
colnames(tmptbl)  <- c("OccMain","OccSub","X1985","X1990","X1995","X2000","X2005")

tmp <- with(tmptbl,format(tmptbl[with(tmptbl, order(OccMain,OccSub)),], digits=2))
with(tmp, paste0("<tr><td>",OccMain,OccSub,"</td><td style='text-align: center;'>",X1985,"</td><td style='text-align: center;'>",X1990,"</td><td style='text-align: center;'>",X1995,"</td><td style='text-align: center;'>",X2000,"</td><td style='text-align: center;'>",X2005,"</td></tr>"))

ls()
#### clean up
rm("grandtotal","grptotals_ht","majorgrpFshares","majorgrpshares","majorgrptotals","tmp","tmpoccgenf","tmptbl")