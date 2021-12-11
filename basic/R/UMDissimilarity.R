#' UMDissimilarity.R
#' @references
#' Otis Dudley Duncan and Beverly Duncan. 1955. "A Methodological Analysis 
#' 	of Segregation Indexes,"
#'      American Sociological Review 20(2): 210-217.
#' [UM2020] Uchikoshi, Fumiya, Ryota Mugiyama. (2020) Trends in 
#' 		Occupational Sex Segregation in Japan: 
#'         A Decomposition Analysis of Census Data, 1980-2005. 
#'       Japanese Journal of Population Studies. 2020. 56. 9-23 
#' [Elbers] Benjamin Elbers. 2021. A Method for Studying Differences in
#'   Segregation Across Time and Space Sociological Methods & Research.
#'   doi: 10.1177/0049124121986204

#' Reproduce Uchikoshi-Mugiyama decomposition

library(segregation) # Use Elbers package when possible
library(data.table)
#' imports: @import data.table

#' Fake examples in UM2020
um1 <- matrix_to_long(matrix(c(10, 30, 40,50,50,20), ncol = 2))
um2 <- matrix_to_long(matrix(c(20, 30, 30,60,50,10), ncol = 2))

#' Elbers has a dissimilarity function. Try it with UM data.
dissimilarity(um1, "group", "unit", weight="n")
##    stat       est
## 1:    D 0.3333333
dissimilarity(um2, "group", "unit", weight="n")
##    stat       est
## 1:    D 0.2916667
#' These are the same values as in UM2020

#' Adopt parameters from Elber 
#' https://github.com/elbersb/segregation/blob/master/R/dissimilarity.R
#' @param data A data frame.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the first dimension
#'   over which segregation is computed. The D index only
#'   allows two distinct groups.
#' @param unit A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the second dimension
#'   over which segregation is computed.
#' @param weight Numeric. (Default \code{NULL})

source("scripts/dissimilarity_um.R")

um1.prep <- prepare_data(um1, "group", "unit", weight="n")
um2.prep <- prepare_data(um2, "group", "unit", weight="n")

um1.du <- dissimilarity_unit(um1.prep, "group", "unit")
dissimilarity_all(um1.prep, "group", "unit")

um1.prep[, sum(freq)]

## try to put together UM's decomposition
# dissimilarity_margin
um1.prep <- prepare_data(um1, "group", "unit", weight="n")
um2.prep <- prepare_data(um2, "group", "unit", weight="n")

#' dissimilarity indexes for two time points
um1.du <- dissimilarity_unit(um1.prep, "group", "unit")
um2.du <- dissimilarity_unit(um2.prep, "group", "unit")

#' add columns needed for decomposition
um1.prep[, n_group := sum(freq), by = group]
um2.prep[, n_group := sum(freq), by = group]

um1.prep[, n_unit := sum(freq), by = unit]
um2.prep[, n_unit := sum(freq), by = unit]

um1.prep[, n_all := sum(freq)]
um2.prep[, n_all := sum(freq)]

#' decomposition sum
um.diff <- merge(um1.prep,um2.prep, by=c("group","unit"))
um.diff[, difffrac := (freq.y * (n_unit.x / n_unit.y)) / (n_group.y * (n_all.x / n_all.y)) ]
um.decomp <- um.diff[, 1/2 * abs_diff( difffrac ), by = unit]

#' summarize
um.summary <- um1.du
colnames(um.summary) <- c("unit","D_1")
um.summary <- merge(um.summary, um2.du)
colnames(um.summary) <- c("unit","D_1","D_2")
um.summary[, within := merge(um.decomp, um1.du, by="unit")[,V1.x-V1.y] ]
um.summary[, marginal := merge( um2.du, um.decomp, by="unit")[,V1.x-V1.y] ]
um.summary[, contribution := within + marginal]
um.summary
print(um.summary, digits=3)
#    unit    D_1    D_2  within marginal contribution
# 1:    1 0.1458 0.1250 -0.0521   0.0312      -0.0208
# 2:    2 0.0208 0.0208  0.0000   0.0000       0.0000
# 3:    3 0.1667 0.1458  0.0521  -0.0729      -0.0208

print(data.table(D_1 = sum(um.summary$D_1), D_2 = sum(um.summary$D_2),
	within = sum(um.summary$within ),marginal = sum(um.summary$marginal),
	contribution = sum(um.summary$contribution)),digits=3)
#      D_1   D_2    within marginal contribution
# 1: 0.333 0.292 -2.78e-17  -0.0417      -0.0417
#' These are the same values as in UM2020

#' clean up
rm(um.decomp,um.diff,um.summary,um1,um1.du,um1.prep,             
um2,um2.du,um2.prep,umall)
