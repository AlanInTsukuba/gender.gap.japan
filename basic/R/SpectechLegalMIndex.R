# drill.down.legal.elber.R

# 2020-4-4 EstimatingCounterfactuals.html contains an occupation 
# 	group that is a compression of the spectech occupations that 
#	keeps detail for only the legal occupations. It then shows 
#	the computation of counterfactuals following the 'manual' 
#	method in Deming and Stephan (1940).
#
# This script runs the compressed dataset through Elber's walkthrough.
# 	https://elbersb.github.io/segregation/articles/segregation.html

#' Reference:
#' Elbers, Benjamin. A Method for Studying Differences in Segregation Across Time and Space. 
#' SocArXiv, 21 Dec. 2018, supplementary materials

require("tidyverse")
require("tidylog", warn.conflicts = FALSE)
require("fst")
require("ggthemes")
require("segregation")
require("cowplot")
require("knitr")
require("kableExtra")

# Compute the M and H indices using mutual_total():
head(legal1985)
##    year OccMinor Gender       w
## 1: 1985     1028 FEMALE     789
## 2: 1985     1028   MALE   15204
## 3: 1985     1029 FEMALE    2795
## 4: 1985     1029   MALE   23926
## 5: 1985     1999 FEMALE 2555759
## 6: 1985     1999   MALE 3789332

mutual_total(legal1985, "Gender","OccMinor", weight="w")
##    stat         est
## 1:    M 0.001764758
## 2:    H 0.002621140

head(legal2005)
##    year OccMinor Gender       w
## 1: 2005     1028 FEMALE    2302
## 2: 2005     1028   MALE   19506
## 3: 2005     1029 FEMALE    6379
## 4: 2005     1029   MALE   29833
## 5: 2005     1999 FEMALE 4018698
## 6: 2005     1999   MALE 4465215

mutual_total(legal2005, "Gender","OccMinor", weight="w")
##    stat        est
## 1:    M 0.00162290
## 2:    H 0.00234686

# get standard errors and confidence intervals
mutual_total(legal1985, "Gender","OccMinor", weight="w",
             se = TRUE, CI = .95, n_bootstrap = 500)
## 500 bootstrap iterations on 6387805 observations
##    stat         est           se                      CI         bias
## 1:    M 0.001763851 1.991311e-05 0.001723899,0.001802892 9.063735e-07
## 2:    H 0.002619765 2.956261e-05 0.002560269,0.002677838 1.374524e-06

mutual_total(legal2005, "Gender","OccMinor", weight="w",
             se = TRUE, CI = .95, n_bootstrap = 500)
## 500 bootstrap iterations on 8541933 observations
##    stat         est           se                      CI          bias
## 1:    M 0.001623193 1.745553e-05 0.001590180,0.001655798 -2.929060e-07
## 2:    H 0.002347286 2.524235e-05 0.002299543,0.002394471 -4.254594e-07

# Local segregation
mutual_local(legal1985, "Gender","OccMinor", weight="w", wide = TRUE)
##    OccMinor           ls           p
## 1:     1028 3.352452e-01 0.002503677
## 2:     1029 2.189781e-01 0.004183127
## 3:     1999 9.461832e-06 0.993313196

# get inference
(localse1985 <- mutual_local(legal1985, "Gender","OccMinor", weight="w",
                        se = TRUE, wide = TRUE, n_bootstrap = 1000))

##    OccMinor           ls        ls_se                     ls_CI      ls_bias
## 1:     1028 3.351330e-01 4.347840e-03       0.3264138,0.3438668 1.122377e-04
## 2:     1029 2.188440e-01 3.178994e-03       0.2127366,0.2250096 1.341491e-04
## 3:     1999 9.457071e-06 1.214380e-07 9.230823e-06,9.691590e-06 4.760670e-09
##              p         p_se                    p_CI        p_bias     lengthCI
## 1: 0.002504817 1.904515e-05 0.002466403,0.002541730 -1.140454e-06 1.745295e-02
## 2: 0.004181336 2.538590e-05 0.004130366,0.004230272  1.791226e-06 1.227296e-02
## 3: 0.993313847 3.294520e-05     0.9932524,0.9933768 -6.507713e-07 4.607671e-07
> 

#> 1000 bootstrap iterations on 877739 observations
localse1985$lengthCI <- sapply(localse1985$ls_CI, base::diff)
with(localse1985, plot(x = p, y = lengthCI, pch = 16, cex = 0.3))

mutual_local(legal2005, "Gender","OccMinor", weight="w", wide = TRUE)
##    OccMinor           ls           p
## 1:     1028 3.126092e-01 0.002553052
## 2:     1029 1.922804e-01 0.004239321
## 3:     1999 9.720246e-06 0.993207626

# get inference
(localse2005 <- mutual_local(legal2005, "Gender","OccMinor", weight="w",
                        se = TRUE, wide = TRUE, n_bootstrap = 1000))

##    OccMinor           ls        ls_se                     ls_CI       ls_bias
## 1:     1028 3.124703e-01 4.233187e-03       0.3036940,0.3205216  1.388753e-04
## 2:     1029 1.923954e-01 2.800515e-03       0.1867725,0.1977768 -1.149433e-04
## 3:     1999 9.722705e-06 1.164051e-07 9.505735e-06,9.942108e-06 -2.458438e-09
##              p         p_se                    p_CI        p_bias
## 1: 0.002553406 1.748819e-05 0.002521180,0.002588527 -3.539012e-07
## 2: 0.004239184 2.198816e-05 0.004197402,0.004283225  1.376738e-07
## 3: 0.993207410 2.753442e-05     0.9931490,0.9932594  2.162274e-07

#> 1000 bootstrap iterations on 877739 observations
localse2005$lengthCI <- sapply(localse2005$ls_CI, base::diff)
with(localse2005, plot(x = p, y = lengthCI, pch = 16, cex = 0.3))

# counterfactuals using ipf function
(cfacts <- ipf(legal1985, legal2005, "Gender", "OccMinor", weight = "w"))
## [IPF 1/1] #
##              
##   Gender OccMinor n_source n_target           n
## 1 FEMALE     1028      789     2302    1056.823
## 2 FEMALE     1029     2795     6379    3654.223
## 3 FEMALE     1999  2555759  4018698 3007032.515
## 4   MALE     1028    15204    19506   15244.417
## 5   MALE     1029    23926    29833   23415.900
## 6   MALE     1999  3789332  4465215 3337401.121

(cfactstotal <- 100*sweep(cfacts[,3:5],2,colSums(cfacts[,3:5]),`/`) )
##      n_source    n_target           n
## 1  0.01235166  0.02694940  0.01654439
## 2  0.04375525  0.07467865  0.05720624
## 3 40.00997213 47.04670477 47.07458219
## 4  0.23801603  0.22835581  0.23864876
## 5  0.37455746  0.34925350  0.36657193
## 6 59.32134747 52.27405787 52.24644649

colSums(cfactstotal[c(1,4),]  )
##  n_source  n_target         n 
## 0.2503677 0.2553052 0.2551932

colSums(cfactstotal[c(2,5),]  )
##  n_source  n_target         n 
## 0.4183127 0.4239321 0.4237782 

colSums(cfactstotal[c(3,6),]  )
## n_source n_target        n 
## 99.33132 99.32076 99.32103 

colSums(cfactstotal[1:3,]  )
## n_source n_target        n 
## 40.06608 47.14833 47.14833

colSums(cfactstotal[4:6,]  )
## n_source n_target        n 
## 59.93392 52.85167 52.85167

colSums(cfactstotal  )
## n_source n_target        n 
##      100      100      100 

ipf(legal1985, legal2005, "Gender", "OccMinor", weight = "w", precision=1e-7)

# Decomposing changes in indices
mutual_difference(legal1985, legal2005, "Gender", "OccMinor", weight = "w")
            
##              stat           est
## 1:             M1  1.764758e-03
## 2:             M2  1.622900e-03
## 3:           diff -1.418572e-04
## 4:      additions  0.000000e+00
## 5:       removals  0.000000e+00
## 6: group_marginal  1.175015e-04
## 7:  unit_marginal  6.226171e-06
## 8:     structural -2.655849e-04

mutual_difference(legal1985, legal2005, "Gender", "OccMinor", weight = "w", se=TRUE)

##             stat           est           se                          CI          bias
## 1:             M1  0.0017647425 2.070748e-05     0.001722991,0.001802110  1.513961e-08
## 2:             M2  0.0016236760 2.014460e-05     0.001582224,0.001654201 -7.755737e-07
## 3:           diff -0.0001410665 3.096467e-05 -1.988926e-04,-7.613107e-05 -7.907133e-07
## 4:      additions  0.0000000000 0.000000e+00                         0,0  0.000000e+00
## 5:       removals  0.0000000000 0.000000e+00                         0,0  0.000000e+00
## 6: group_marginal  0.0001027409 6.368765e-06   0.0000945652,0.0001153312  7.086803e-06
## 7:  unit_marginal  0.0000209698 6.467000e-06   7.527389e-06,2.779999e-05 -7.069809e-06
## 8:     structural -0.0002647772 3.115294e-05 -0.0003221377,-0.0001986830 -8.077073e-07

###################################################################################
## use the counterfactuals calculated using Deming and Stephan's least squares
## method

legal1985p <- tibble(Gender = c("FEMALE","MALE","FEMALE","MALE","FEMALE","MALE"),
	OccMinor=c(1028, 1028, 1029, 1029, 1999, 1999),
	w=c(1302,20506,4541,31671,3876555,4607358))

mutual_total(legal1985p , "Gender","OccMinor", weight="w")
##    stat         est
## 1:    M 0.002075279
## 2:    H 0.003012003

(legalMmarginal <- 0.002075279 - 1.764758e-03)
## [1] 0.000310521
(legalMstruct <- 1.622900e-03 - 0.002075279)
## [1] -0.000452379

legalMmarginal / 1.764758e-03
## [1] 0.1759567
legalMstruct / 1.764758e-03
## [1] -0.2563405



