#' ReproduceUMResultsV02.R
#' Use script in UMDissimilarity.R and functions in 
#' dissimilarity_um.R
#' @references
#' [UM2020] Uchikoshi, Fumiya, Ryota Mugiyama. (2020) Trends in Occupational Sex Segregation in Japan: 
#'         A Decomposition Analysis of Census Data, 1980-2005. 
#'       Japanese Journal of Population Studies. 2020. 56. 9-23 
#' [Elbers] Benjamin Elbers. 2021. A Method for Studying Differences in
#'   Segregation Across Time and Space Sociological Methods & Research.
#'   doi: 10.1177/0049124121986204

require(segregation)
require(data.table)
source("ScriptsV02/dissimilarity_um.R")

# use 1985 occupation statistics with occupational crosswalk
# make data.table copy
d85 <- as.data.table(occgenall[occgenall$year == "1985",c("OccMinor","Gender","n")])
head(d85)

# start with Elbers dissimilarity function
segregation::dissimilarity(d85, "Gender","OccMinor", weight="n")
#    stat       est
# 1:    D 0.5129446

# then with UM2020 dissimilarity
ggj.dissimilarity_all(d85, "Gender","OccMinor", weight="n")
#    stat       est
# 1:    D 0.5129446

# do remaining years
d90  <- as.data.table(occgenall[occgenall$year == "1990",c("OccMinor","Gender","n")])
d95  <- as.data.table(occgenall[occgenall$year == "1995",c("OccMinor","Gender","n")])
d00  <- as.data.table(occgenall[occgenall$year == "2000",c("OccMinor","Gender","n")])
d05  <- as.data.table(occgenall[occgenall$year == "2005",c("OccMinor","Gender","n")])

# put in data table for plotting as in figure 1 of UM2020
di_um <- data.table( year = c(1985,1990,1995,2000,2005),
DI = c(ggj.dissimilarity_all(d85, "Gender","OccMinor", weight="n")[,"est"],
ggj.dissimilarity_all(d90, "Gender","OccMinor", weight="n")[,"est"],
ggj.dissimilarity_all(d95, "Gender","OccMinor", weight="n")[,"est"],
ggj.dissimilarity_all(d00, "Gender","OccMinor", weight="n")[,"est"],
ggj.dissimilarity_all(d05, "Gender","OccMinor", weight="n")[,"est"]) )

di_um$Change <- as.numeric(di_um$DI ) / as.numeric(di_um[1,2][[1]])

windows(11,5)
par(mfrow=c(1,2))
with(di_um, plot(year, DI, ylim=c(0,0.60), type="b",
	main="Dissimilarity Index", ylab=""))
with(di_um, text(year, DI, labels=round(as.numeric(di_um$DI ),4), 
	pos=3,cex=0.7))
with(di_um, plot(year, Change, ylim=c(0.90,1.05), type="b",
	main="Change versus 1985", ylab=""))
with(di_um, text(year, Change, labels=round(as.numeric(di_um$Change ),3), 
	pos=3,cex=0.7))

# decompose census-to-census changes 
#' dissimilarity indexes for time points
d85.du <- ggj.dissimilarity_unit(d85,"Gender","OccMinor", weight="n")
d90.du <- ggj.dissimilarity_unit(d90,"Gender","OccMinor", weight="n")
d95.du <- ggj.dissimilarity_unit(d95,"Gender","OccMinor", weight="n")
d00.du <- ggj.dissimilarity_unit(d00,"Gender","OccMinor", weight="n")
d05.du <- ggj.dissimilarity_unit(d05,"Gender","OccMinor", weight="n")

#' prepare tables for decomposition
d85.dm <- ggj.prepare_data(d85,"Gender","OccMinor", weight="n")
d85.dm[, n_group := sum(freq), by = Gender]
d85.dm[, n_unit := sum(freq), by = OccMinor]
d85.dm[, n_all := sum(freq)]

d90.dm <- ggj.prepare_data(d90,"Gender","OccMinor", weight="n")
d90.dm[, n_group := sum(freq), by = Gender]
d90.dm[, n_unit := sum(freq), by = OccMinor]
d90.dm[, n_all := sum(freq)]

d95.dm <- ggj.prepare_data(d95,"Gender","OccMinor", weight="n")
d95.dm[, n_group := sum(freq), by = Gender]
d95.dm[, n_unit := sum(freq), by = OccMinor]
d95.dm[, n_all := sum(freq)]

d00.dm <- ggj.prepare_data(d00,"Gender","OccMinor", weight="n")
d00.dm[, n_group := sum(freq), by = Gender]
d00.dm[, n_unit := sum(freq), by = OccMinor]
d00.dm[, n_all := sum(freq)]

d05.dm <- ggj.prepare_data(d05,"Gender","OccMinor", weight="n")
d05.dm[, n_group := sum(freq), by = Gender]
d05.dm[, n_unit := sum(freq), by = OccMinor]
d05.dm[, n_all := sum(freq)]

# decomposition sums 1985-1990
d85_90.diff <- merge(d85.dm,d90.dm, by=c("Gender","OccMinor"))
d85_90.diff[, difffrac := (freq.y * (n_unit.x / n_unit.y)) / (n_group.y * (n_all.x / n_all.y)) ]
d85_90.decomp <- d85_90.diff[, 1/2 * ggj.abs_diff( difffrac ), by = "OccMinor"]

#' differences 
d85_90.summary <- d85.du
colnames(d85_90.summary) <- c("OccMinor","D_1")
d85_90.summary <- merge(d85_90.summary, d90.du)
colnames(d85_90.summary) <- c("OccMinor","D_1","D_2")
d85_90.summary[, within := merge(d85_90.decomp, d85.du, by="OccMinor")[,V1.x-V1.y] ]
d85_90.summary[, marginal := merge( d90.du, d85_90.decomp, by="OccMinor")[,V1.x-V1.y] ]
d85_90.summary[, contribution := within + marginal]
d85_90.summary

# sum over units
d85_90.sum <- with(d85_90.summary, data.table(D_1 = sum(D_1), D_2 = sum(D_2),
	within = sum(within ),marginal = sum(marginal),
	contribution = sum(contribution)))


# decomposition sums 1990-1995
d90_95.diff <- merge(d90.dm,d95.dm, by=c("Gender","OccMinor"))
d90_95.diff[, difffrac := (freq.y * (n_unit.x / n_unit.y)) / (n_group.y * (n_all.x / n_all.y)) ]
d90_95.decomp <- d90_95.diff[, 1/2 * ggj.abs_diff( difffrac ), by = "OccMinor"]

#' differences 
d90_95.summary <- d90.du
colnames(d90_95.summary) <- c("OccMinor","D_1")
d90_95.summary <- merge(d90_95.summary, d95.du)
colnames(d90_95.summary) <- c("OccMinor","D_1","D_2")
d90_95.summary[, within := merge(d90_95.decomp, d90.du, by="OccMinor")[,V1.x-V1.y] ]
d90_95.summary[, marginal := merge( d95.du, d90_95.decomp, by="OccMinor")[,V1.x-V1.y] ]
d90_95.summary[, contribution := within + marginal]
d90_95.summary

# sum over units
d90_95.sum <- with(d90_95.summary, data.table(D_1 = sum(D_1), D_2 = sum(D_2),
	within = sum(within ),marginal = sum(marginal),
	contribution = sum(contribution)))
d90_95.sum


# decomposition sums 1995-2000
d95_00.diff <- merge(d95.dm,d00.dm, by=c("Gender","OccMinor"))
d95_00.diff[, difffrac := (freq.y * (n_unit.x / n_unit.y)) / (n_group.y * (n_all.x / n_all.y)) ]
d95_00.decomp <- d95_00.diff[, 1/2 * ggj.abs_diff( difffrac ), by = "OccMinor"]

#' differences 
d95_00.summary <- d95.du
colnames(d95_00.summary) <- c("OccMinor","D_1")
d95_00.summary <- merge(d95_00.summary, d00.du)
colnames(d95_00.summary) <- c("OccMinor","D_1","D_2")
d95_00.summary[, within := merge(d95_00.decomp, d95.du, by="OccMinor")[,V1.x-V1.y] ]
d95_00.summary[, marginal := merge( d00.du, d95_00.decomp, by="OccMinor")[,V1.x-V1.y] ]
d95_00.summary[, contribution := within + marginal]
d95_00.summary

# sum over units
d95_00.sum <- with(d95_00.summary, data.table(D_1 = sum(D_1), D_2 = sum(D_2),
	within = sum(within ),marginal = sum(marginal),
	contribution = sum(contribution)))
d95_00.sum


# decomposition sums 2000-2005
d00_05.diff <- merge(d00.dm,d05.dm, by=c("Gender","OccMinor"))
d00_05.diff[, difffrac := (freq.y * (n_unit.x / n_unit.y)) / (n_group.y * (n_all.x / n_all.y)) ]
d00_05.decomp <- d00_05.diff[, 1/2 * ggj.abs_diff( difffrac ), by = "OccMinor"]

#' differences 
d00_05.summary <- d00.du
colnames(d00_05.summary) <- c("OccMinor","D_1")
d00_05.summary <- merge(d00_05.summary, d05.du)
colnames(d00_05.summary) <- c("OccMinor","D_1","D_2")
d00_05.summary[, within := merge(d00_05.decomp, d00.du, by="OccMinor")[,V1.x-V1.y] ]
d00_05.summary[, marginal := merge( d05.du, d00_05.decomp, by="OccMinor")[,V1.x-V1.y] ]
d00_05.summary[, contribution := within + marginal]
d00_05.summary

# sum over units
d00_05.sum <- with(d00_05.summary, data.table(D_1 = sum(D_1), D_2 = sum(D_2),
	within = sum(within ),marginal = sum(marginal),
	contribution = sum(contribution)))
d00_05.sum

# decomposition sums 1985-2005
d85_05.diff <- merge(d85.dm,d05.dm, by=c("Gender","OccMinor"))
d85_05.diff[, difffrac := (freq.y * (n_unit.x / n_unit.y)) / (n_group.y * (n_all.x / n_all.y)) ]
d85_05.decomp <- d85_05.diff[, 1/2 * ggj.abs_diff( difffrac ), by = "OccMinor"]

#' differences 
d85_05.summary <- d85.du
colnames(d85_05.summary) <- c("OccMinor","D_1")
d85_05.summary <- merge(d85_05.summary, d05.du)
colnames(d85_05.summary) <- c("OccMinor","D_1","D_2")
d85_05.summary[, within := merge(d85_05.decomp, d85.du, by="OccMinor")[,V1.x-V1.y] ]
d85_05.summary[, marginal := merge( d05.du, d85_05.decomp, by="OccMinor")[,V1.x-V1.y] ]
d85_05.summary[, contribution := within + marginal]
d85_05.summary

# sum over units
d85_05.sum <- with(d85_05.summary, data.table(D_1 = sum(D_1), D_2 = sum(D_2),
	within = sum(within ),marginal = sum(marginal),
	contribution = sum(contribution)))
d85_05.sum

# plot it
res <- cbind(data.table(
	Period = c("1985-1990","1990-1995","1995-2000","2000-2005","1985-2005")),
	rbind(d85_90.sum,d90_95.sum,d95_00.sum,d00_05.sum,d85_05.sum))
colnames(res) <- c("Period","D_1","D_2","Structural","Occupational","Total")

with(res,barplot(cbind(Structural,Occupational,Total) ~ Period, beside=TRUE, ylim=c(-0.04,0.04),
main="Decomposition of changes in 
gender-occupation segregation",xlab="",
names.arg=c("1985-1990","1990-1995","1995-2000","2000-2005","1985-2005"),
legend.text = c("Structural","Occupational","Total"), col = gray.colors(3)))

#' scatterplot corresponding to UM2020 Figure 3
par(mfrow=c(1,1))
with(d85_05.summary[abs(contribution) > 0.002 & abs(contribution) < 0.5 ,], 
	plot(marginal,within, xlim=c(-0.008,0.010),ylim=c(-0.015,0.007)
	))
with(d85_05.summary[abs(contribution) > 0.002 & abs(contribution) < 0.5,], text(marginal,within, 
	labels=as.character(OccMinor ), #xlim=c(-0.010,0.010),ylim=c(-0.005,0.005),
	pos=1,cex=0.7))

# clean up
rm(d00,                   
d00.dm,d00.du,d00_05.decomp,d00_05.diff,d00_05.sum,
d00_05.summary,d05,d05.dm,d05.du,d85,d85.dm,d85.du,                
d85_05.decomp,d85_05.diff,d85_05.sum,d85_05.summary,       
d85_90.decomp,d85_90.diff,d85_90.sum,d85_90.summary,        
d90,d90.dm,d90.du,d90_95.decomp,d90_95.diff,d90_95.sum,            
d90_95.summary,d95,d95.dm,d95.du,d95_00.decomp,d95_00.diff,           
d95_00.sum,d95_00.summary,di_um,ref_di,ref_val,res,tmp )



