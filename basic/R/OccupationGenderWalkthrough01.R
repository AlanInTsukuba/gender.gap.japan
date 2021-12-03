#' OccupationGenderWalkthrough01.R
#' R code to go with OccupationGenderWalkthrough01.html
#' if not yet installed
install.packages("segregation")

library(segregation)
#' Load occupation vs gender for all Japan 1995 and 2000
occgen <- read.csv("Data/OccGen1995-2000.csv",header=T,
colClasses=c("integer","character","integer","integer","integer",
"character","character","integer"))

#' look at the top rows
head(occgen)
#>   CensusYear OccMajor OccMid OccMinor Level
#> 1       1995       JP     NA       NA     0
#> 2       1995        A     NA       NA     1
#> 3       1995        A      1       NA     3
#> 4       1995        A      1        1     4
#> 5       1995        A      1        2     4
#> 6       1995        A      2       NA     3
#>                                     Title Gender   Number
#> 1              All Japan, all occupations Female 25635727
#> 2      PROFESSIONAL AND TECHNICAL WORKERS Female  3405801
#> 3                  SCIENTIFIC RESEARCHERS Female    23312
#> 4             Natural science researchers Female    21596
#> 5 Cultural and social science researchers Female     1716
#> 6               ENGINEERS AND TECHNICIANS Female   143166

###########################################################
#' Select bottom level data of interest
with(occgen, head(occgen[Level == 4 & CensusYear==1995,c("OccMajor","OccMinor","Gender","Number")]))

#>    OccMajor OccMinor Gender Number
#> 4         A        1 Female  21596
#> 5         A        2 Female   1716
#> 7         A        3 Female   6065
#> 8         A        4 Female    458
#> 9         A        5 Female   7216
#> 10        A        6 Female   8662

#' Exclude unclassified
occgen95 <- with(occgen,occgen[Level == 4 & CensusYear==1995 & OccMinor != 293,
c("OccMajor","OccMinor","Gender","Number")])
#' Remove NA rows
na.omit(occgen95)

################################################################################
#' Compute M and H indices
with(occgen95, mutual_total(occgen95,"Gender","OccMinor",weight="Number"))

#>    stat       est
#> 1:    M 0.1921583
#> 2:    H 0.2856683

with(occgen95, mutual_total(occgen95,"OccMinor","Gender",weight="Number"))

#>    stat        est
#> 1:    M 0.19215829
#> 2:    H 0.04423054

#' Using bootstrap error computation
with(occgen95, mutual_total(occgen95,
"Gender","OccMinor",weight="Number",
             se = TRUE, CI = .95, n_bootstrap = 500))

with(occgen95, mutual_total(occgen95[,c("OccMinor","Gender","Number")],
"Gender","OccMinor",weight="Number",
             se = TRUE, CI = .95, n_bootstrap = 500))

#> 500 bootstrap iterations on 63797997 observations
#>    stat       est           se                  CI         bias
#> 1:    M 0.1921549 6.299670e-05 0.1920412,0.1922746 3.373920e-06
#> 2:    H 0.2856635 9.228568e-05 0.2854804,0.2858422 4.805942e-06

#' Repeat with top level occupation classes

########################################################################
#'   between-within decompositon
split_occgen95 <- with(occgen95, split(occgen95, occgen95$OccMajor))

head(split_occgen95)
head(split_occgen95$I_1)

with(split_occgen95, mutual_total(split_occgen95$A,"Gender","OccMinor",weight="Number"))

with(split_occgen95, mutual_total(split_occgen95$B,"Gender","OccMinor",weight="Number"))
with(split_occgen95, mutual_total(split_occgen95$C,"Gender","OccMinor",weight="Number"))
with(split_occgen95, mutual_total(split_occgen95$D,"Gender","OccMinor",weight="Number"))
with(split_occgen95, mutual_total(split_occgen95$E,"Gender","OccMinor",weight="Number"))
with(split_occgen95, mutual_total(split_occgen95$F,"Gender","OccMinor",weight="Number"))
with(split_occgen95, mutual_total(split_occgen95$G,"Gender","OccMinor",weight="Number"))
with(split_occgen95, mutual_total(split_occgen95$H,"Gender","OccMinor",weight="Number"))
with(split_occgen95, mutual_total(split_occgen95$I_1,"Gender","OccMinor",weight="Number"))
with(split_occgen95, mutual_total(split_occgen95$I_2,"Gender","OccMinor",weight="Number"))
with(split_occgen95, mutual_total(split_occgen95$I_3,"Gender","OccMinor",weight="Number"))

#' computing decomposition
#' total segregation
(total <- with(occgen95, mutual_total(occgen95,"Gender","OccMinor",weight="Number")))
#>    stat       est
#> 1:    M 0.1921583
#> 2:    H 0.2856683
#' between-major group segregation
#' 	how much does the gender distribution differ across major occupation groups
(between <- with(occgen95, mutual_total(occgen95,"Gender","OccMajor",weight="Number")))
#>    stat        est
#> 1:    M 0.07042062
#> 2:    H 0.10468943
#' within major group segregation
with(occgen95, mutual_total(occgen95,"Gender","OccMinor",within = "OccMajor",weight="Number"))
#>    stat       est
#> 1:    M 0.1217377
#> 2:    H 0.1809789

#' Get within decomposition by major occupation group
(within <- with(occgen95, mutual_within(occgen95,"Gender","OccMinor",
			within = "OccMajor",weight="Number",wide=TRUE)))
#>     OccMajor          M          p          H  ent_ratio
#>  1:        A 0.30409882 0.12736970 0.44719836 1.01092163
#>  2:        B 0.02105742 0.04258087 0.06680864 0.46857096
#>  3:        C 0.02103788 0.19415152 0.03168898 0.98695331
#>  4:        D 0.15565050 0.15008821 0.23622428 0.97955525
#>  5:        E 0.06525243 0.07978794 0.09973051 0.97268373
#>  6:        F 0.01030613 0.01497975 0.05530293 0.27704508
#>  7:        G 0.02761960 0.05976796 0.04039443 1.01647987
#>  8:        H 0.07490675 0.03827970 0.36327622 0.30653986
#>  9:      I_1 0.16297451 0.18147673 0.25177739 0.96228987
#> 10:      I_2 0.00346329 0.01650898 0.05465039 0.09421035
#> 11:      I_3 0.15945956 0.09500864 0.29051833 0.81598090

with(within, sum(M * p))
#> [1] 0.1217377
with(within, sum(M * p * ent_ratio))
#> [1] 0.1149832

#' add the between component and get all contributions
#' merge into a vector
components <- c(between$est[1], within$M * within$p)
names(components) <- c("Between", "A", "B", "C","D","E","F","G","H","I_1","I_2","I_3")
signif(100 * components / total$est[1], 3)
#> Between       A       B       C       D       E       F       G       H 
#> 36.6000 20.2000  0.4670  2.1300 12.2000  2.7100  0.0803  0.8590  1.4900 
#>     I_1     I_2     I_3 
#> 15.4000  0.0298  7.8800 

################################################################################
#' Local segregation
(localoccgen95 <-with(occgen95, mutual_local(occgen95, "Gender","OccMinor",weight="Number", 
		wide=TRUE)))
#>      OccMinor         ls            p
#>   1:        1 0.17792791 0.0026274179
#>   2:        2 0.08521106 0.0001306781
#>   3:        3 0.23904515 0.0010283081
#>   4:        4 0.41632608 0.0003430202
#>   5:        5 0.40382257 0.0045865390
#>  ---                                 
#> 288:      288 0.08925095 0.0036835012
#> 289:      289 0.02133888 0.0120616169
#> 290:      290 0.01106637 0.0039019564
#> 291:      291 0.07130543 0.0113775516
#> 292:      292 0.08273497 0.0121131232

sum(localoccgen95$p)
#> [1] 1
with(localoccgen95, sum(ls * p))
#> [1] 0.1921583

#' Compute and plot 95% confidence intervals vs occupation size
localoccgen95se <-with(occgen95, mutual_local(occgen95, "Gender","OccMinor",weight="Number", 
		wide=TRUE, se = TRUE, n_bootstrap = 1000))
#> 1000 bootstrap iterations on 63797997 observations
localoccgen95se$lengthCI <- sapply(localoccgen95se$ls_CI, base::diff)
with(localoccgen95se, plot(x = p, y = lengthCI, pch = 16, cex = 0.3))

merge(localoccgen95se[p > 0.02,c("OccMinor","ls","ls_CI","p","p_CI")],
with(occgen,occgen[CensusYear == 1995 & Gender == "Female",c("OccMinor","Title")]))

#' List occupations with shares greater than 2%
merge(with(occgen,occgen[CensusYear == 1995 & Gender == "Female",c("OccMinor","Title")]),
localoccgen95se[p > 0.02,c("OccMinor","ls","p")])
#>   OccMinor                              Title         ls          p
#> 1       57             Directors of companies 0.16476440 0.02544791
#> 2       61           General clerical workers 0.07125314 0.14031904
#> 3       62                  Accounting clerks 0.27254682 0.04310470
#> 4       73 Shop salespersons and sales clerks 0.12359476 0.05244004
#> 5       76  Travelling commodity salespersons 0.27483187 0.03744058
#> 6       90                              Cooks 0.04209085 0.02849340
#> 7      113         Farmers and sericulturists 0.01099404 0.04826190
#> 8      129                 Automobile drivers 0.40667934 0.03166273

#' To get confidence intervals, use
merge(with(occgen,occgen[CensusYear == 1995 & Gender == "Female",c("OccMinor","Title")]),
localoccgen95se[p > 0.02,c("OccMinor","ls","ls_CI","p","p_CI")])

#' Switch OccMinor and Gender to get segregation by gender
(localoccgen95g <-with(occgen95, mutual_local(occgen95,"OccMinor", "Gender",weight="Number", 
		wide=TRUE)))
#>    Gender        ls        p
#> 1: Female 0.2971974 0.399142
#> 2:   Male 0.1223822 0.600858


##################################################################################
#' Inference
(se <- with(occgen95, mutual_total(occgen95,"Gender","OccMinor",weight="Number",
             se = TRUE, CI = .95, n_bootstrap = 500)))
#> 500 bootstrap iterations on 63797997 observations
#>    stat       est           se                  CI         bias
#> 1:    M 0.1921553 6.011835e-05 0.1920404,0.1922738 3.005235e-06
#> 2:    H 0.2856637 8.741495e-05 0.2855050,0.2858442 4.623086e-06


# M
with(se, c(est[1] - 1.96 * se[1], est[1] + 1.96 * se[1]))
#> [1] 0.1920375 0.1922731
# H
with(se, c(est[2] - 1.96 * se[2], est[2] + 1.96 * se[2]))
#> [1] 0.2854924 0.2858350

#' List occupations with segregation scores less than 0.01
merge(with(occgen,occgen[CensusYear == 1995 & Gender == "Female",c("OccMinor","Title")]),
localoccgen95se[ls < 0.01,c("OccMinor","ls","p")])

#' Look at a single occupation. First,
local <- mutual_local(occgen95, "Gender", "OccMinor", weight = "Number",
                      se = TRUE, CI = .95, n_bootstrap = 500)
#> 500 bootstrap iterations on 63797997 observations
# pick bootstrap distribution of local segregation scores for OccMinor 36
ls_occ <- attr(local, "bootstrap")[OccMinor == 36 & stat == "ls", boot_est]
hist(ls_occ, main = "Bootstrap distribution for secondary school teachers")

with(occgen95, mutual_expected(occgen95, "Gender","OccMinor",weight="Number",
	n_bootstrap = 500))
#>    stat       est           se
#> 1: M under 0 2.280157e-06 1.971651e-07
#> 2: H under 0 3.389751e-06 2.931116e-07

##################################################################################
#' Decomposing differences in indices
#' Compare 1995 and 2000 censuses.
#' First create the dataframe for 2000 
occgen2000 <- with(occgen,occgen[Level == 4 & CensusYear==2000 & OccMinor != 293,
c("OccMajor","OccMinor","Gender","Number")])
#' Remove NA rows
na.omit(occgen2000)

mutual_difference(occgen95, occgen2000, "Gender","OccMinor",weight="Number")
            
#>              stat           est
#> 1:             M1  0.1921582894
#> 2:             M2  0.1890423190
#> 3:           diff -0.0031159704
#> 4:      additions  0.0000000000
#> 5:       removals  0.0000000000
#> 6: group_marginal  0.0023852848
#> 7:  unit_marginal -0.0008030578
#> 8:     structural -0.0046981974


################################################################################
#' Compute M and H indices
with(occgen2000, mutual_total(occgen2000,"Gender","OccMinor",weight="Number"))

#' Get the confidence intervals
mutual_difference(occgen95, occgen2000, "Gender","OccMinor",weight="Number",
		se = TRUE, CI = 0.95, n_bootstrap = 500)
#>    stat       est           se                  CI         bias
#> 1:             M1  1.921598e-01 6.176532e-05       0.1920459,0.1922851 -1.502842e-06
#> 2:             M2  1.890383e-01 6.719240e-05         0.188910,0.189166  4.067631e-06
#> 3:           diff -3.121541e-03 9.412480e-05 -0.003295979,-0.002938425  5.570473e-06
#> 4:      additions  5.551115e-20 1.241267e-18                       0,0 -5.551115e-20
#> 5:       removals  0.000000e+00 0.000000e+00                       0,0  0.000000e+00
#> 6: group_marginal  2.727083e-03 2.613274e-04   0.002271945,0.003253979 -9.003382e-05
#> 7:  unit_marginal -1.147215e-03 2.602473e-04 -0.001657271,-0.000686574  9.239236e-05
#> 8:     structural -4.701409e-03 8.364964e-05 -0.004863429,-0.004536118  3.211939e-06

#' clean up
ls()
rm("between")
rm("components","local" ,"localoccgen95" ,"localoccgen95g","localoccgen95se","ls_occ")
rm("se","split_occgen95","total", "within","occgen2000" ,"occgen95" )