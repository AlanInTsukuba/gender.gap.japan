#' DescriptiveStatistics.R
#' 2021-12-15
#' Revised 2022-5-2

#' Adapted from
#' Elbers, Benjamin. A Method for Studying Differences in Segregation Across Time and Space. 
#' SocArXiv, 21 Dec. 2018, supplementary materials

library("tidyverse")
library("tidylog", warn.conflicts = FALSE)
library("fst")
library("ggthemes")
library("segregation")
library("cowplot")
library("knitr")
library("kableExtra")

## data is in occgenall
head(occgenall)
#' Harmonized occupation classes to get main group titles
# V02 created 2022-4-19
#' hoc is created in BuildMasterDataset.R.

# make copy to use as Elbers
d <- as_tibble(occgenall) %>%
	mutate(w = n,OccSub = as.character(OccSub),
	OccMain = as.character(OccMain), OccSub=as.character(OccSub),
	OccMid=as.character(OccMid),OccMinor=as.character(OccMinor),
	year = as.integer(year)) %>%
	filter(OccMinor != 1267) # remove Workers not classifiable by occupation
head(d)

##########################
###### descriptives ######
##########################

samplesize <- d %>% filter(!is.na(n)) %>%
    group_by(year) %>%
    summarize(n = sum(w)) %>%
    mutate(var = "Sample size (in 1000)",
        n = round(n / 1000)) %>%
    spread(year, n)

occ1985 <- filter(d, year == 1985) %>% pull(OccMinor) %>% unique
occ1990 <- filter(d, year == 1990) %>% pull(OccMinor) %>% unique
occ1995 <- filter(d, year == 1995) %>% pull(OccMinor) %>% unique
occ2000 <- filter(d, year == 2000) %>% pull(OccMinor) %>% unique
occ2005 <- filter(d, year == 2005) %>% pull(OccMinor) %>% unique
common_occ <- intersect(occ1985 , occ1990 ) %>%
    intersect(occ1995 ) %>% intersect(occ2000 ) %>% 
	intersect(occ2005 )

nocc <- tribble(~var, ~`1985`, ~`1990`, ~`1995`, ~`2000`, ~`2005`,
    "Number of occupations", length(occ1985),
        length(occ1990), length(occ1995), length(occ2000),
	length(occ2005),
    "Appearing occupations", NA,
        length(setdiff(occ1990, occ1985)),
        length(setdiff(occ1995, occ1990)),
        length(setdiff(occ2000, occ1995)),
        length(setdiff(occ2005, occ2000)),
    "Disappearing occupations", NA,
        length(setdiff(occ1985, occ1990)),
        length(setdiff(occ1990, occ1995)),
        length(setdiff(occ1995, occ2000)),
        length(setdiff(occ2000, occ2005))
    )

gender <- d %>% filter(!is.na(w)) %>%
    group_by(year, Gender) %>%
    summarize(n = sum(w)) %>%
    group_by(year) %>%
    mutate(p = round(n / sum(n) * 100)) %>%
    filter(Gender == "FEMALE") %>%
    select(-n) %>%
    spread(year, p) %>%
    rename(var = Gender)

occ <- d %>% filter(!is.na(w)) %>%
    group_by(year, OccMain,OccSub) %>%
    summarize(n = sum(w)) %>%
    group_by(year) %>%
    mutate(p = round(n / sum(n) * 100)) %>%
    select(-n) %>%
    spread(year, p) %>%
    merge(hoc[nchar(hoc$MainTitle)>0,c("OccMain","OccSub","MainTitle")]) %>%
    mutate(OccMain = paste(OccMain,MainTitle,sep=" "))  %>%
    select(-MainTitle,-OccSub) %>%
    rename(var = OccMain)

occ_gender <- d %>% filter(!is.na(w)) %>%
    group_by(year, OccMain,OccSub, Gender) %>%
    summarize(n = sum(w)) %>%
    group_by(year, OccMain,OccSub) %>%
    mutate(p = round(n / sum(n) * 100)) %>%
    filter(Gender== "FEMALE") %>%
    select(-n, -Gender) %>%
    spread(year, p) %>%
    merge(hoc[nchar(hoc$MainTitle)>0,c("OccMain","OccSub","MainTitle")]) %>%
    mutate(OccMain = paste(OccMain,MainTitle,sep=" "))  %>%
    select(-MainTitle,-OccSub) %>%
    rename(var = OccMain)

desc <- bind_rows(samplesize, nocc, gender, occ, occ_gender)
desc[5, "var"] <- "Female"
names(desc)[[1]] <- ""

options(knitr.kable.NA = "")
kable(desc, "latex", booktabs = T) %>%
    kable_styling(latex_options = c("striped", "condensed"),
        position = "center") %>%
    row_spec(0, align = "c") %>%
    group_rows("A. Number of occupations", 2, 4) %>%
    group_rows("B. Labor force participation (%)", 5, 5) %>%
    group_rows("C. Distribution of occupational major groups (%)", 6, 16) %>%
    group_rows("D. Female labor force by major groups (%)", 17, 27) %>%
    cat(file = "Output/desc.tex", sep = "\n")

kable(desc, "html", booktabs = T) %>%
    kable_styling(latex_options = c("striped", "condensed"),
        position = "center") %>%
    row_spec(0, align = "c") %>%
    group_rows("A. Number of occupations", 2, 4) %>%
    group_rows("B. Labor force participation (%)", 5, 5) %>%
    group_rows("C. Distribution of occupational major groups (%)", 6, 16) %>%
    group_rows("D. Female labor force by major groups (%)", 17, 27) %>%
    cat(file = "Output/desc.html", sep = "\n")

#####################################
###### analysis: cross-section ######
#####################################

by_year <- d %>%
    group_by(year) %>%
    do(mutual_total(., "Gender", "OccMinor", weight = "w"))
p_total <- ggplot(by_year, aes(x = year, y = est, linetype = stat)) +
    geom_point() + geom_line() + ylim(0, max(by_year$est)) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

d %>%
    filter(OccMinor %in% common_occ) %>%
    group_by(year) %>%
    do(mutual_total(., "Gender", "OccMinor", weight = "w")) %>%
    rename(est_common = est) %>%
    left_join(by_year) %>%
    mutate(est / est_common)

between <- d %>%
    group_by(year) %>%
    do(mutual_total(., "Gender", "OccMain", weight = "w"))
p_between <- ggplot(between, aes(x = year, y = est, linetype = stat)) +
    geom_point() + geom_line() +
    ylim(0, max(by_year$est)) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

by_year_major <- d %>%
    group_by(year, OccMain) %>%
    do(mutual_total(., "Gender", "OccMinor", weight = "w")) %>%
    merge(hoc[nchar(hoc$MainTitle)>0,c("OccMain","MainTitle")]) %>%
    mutate(OccMain = paste(OccMain,MainTitle,sep=" "))  %>%
    select(-MainTitle)

p_major <- ggplot(by_year_major, aes(x = year, y = est, linetype = stat)) +
    facet_wrap("OccMain") + geom_point() + geom_line() +
    ylim(0, max(by_year_major$est)) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        strip.text.x = element_text(margin = margin(2, 0, 2, 0)))

p_complete <- ggdraw() +
  draw_plot(p_total, .1, .75, .375, .23) +
  draw_plot(p_between, .5, .75, .375, .23) +
  draw_plot(p_major, 0, 0, 1, .73) +
  draw_plot_label(c("A: Total", "B: Between", "C: Within"),
    c(.11, .51, 0.01), c(1.0, 1.0, .75), size = 12, hjust = 0)
ggsave("Output/ElbersTotalSegregation01.pdf", p_complete, width = 6, height = 8)
ggsave("Output/ElbersTotalSegregation01.png", p_complete, width = 8, height = 12)


##############################
###### analysis: change ######
##############################

d1985 <- filter(d, year == 1985)
d2005 <- filter(d, year == 2005)

diffs <- bind_rows(
    mutual_difference(d1985 , d2005, "Gender", "OccMinor", weight = "w",
            precision = .000001) %>%
        mutate(OccMain= "Total") %>%
        spread(stat, est),
    mutual_difference(d1985 , d2005, "Gender", "OccMain", weight = "w",
            precision = .000001) %>%
        mutate(OccMain= "Between major groups") %>%
        spread(stat, est),
    d %>%
        group_by(OccMain) %>%
            do(mutual_difference(filter(., year == 1985), filter(., year == 2005),
                        "Gender", "OccMinor", weight = "w", precision = .000001)) %>%
            spread(stat, est) %>%
		merge(hoc[nchar(hoc$MainTitle)>0,c("OccMain","MainTitle")]) %>% # add main group titles
    		mutate(OccMain = paste(OccMain,MainTitle,sep=" "))  %>%
    		select(-MainTitle)  %>%
            arrange(diff)
    )

# NOTE mutual_difference doesn't output additions or removals if there are none.
diffs_total <- diffs %>%
    select(OccMain, M1, M2, diff, additions, removals, unit_marginal, group_marginal, structural) %>%
    mutate(order = 1:11)

diffs_percentage <- diffs_total %>%
    mutate(additions = round(additions / diff * 100),
	removals = round(removals / diff * 100),
        unit_marginal = round(unit_marginal / diff * 100),
        group_marginal = round(group_marginal / diff * 100),
        structural = round(structural / diff * 100),
        diff = 100) %>%
    select(-OccMain, -M1, -M2) %>%
    mutate_at(vars(diff:structural),
        function(x) paste0("(", sprintf("%.0f", x), "%)"))

diffs_total <- diffs_total %>%
    mutate_at(vars(M1:structural), function(x) paste0( sprintf("%.3f", x)))

options(knitr.kable.NA = "")
bind_rows(diffs_total, diffs_percentage) %>%
    arrange(order, M1) %>%
    select(-order) %>%
    kable("html", booktabs = TRUE, escape = FALSE,
        align = c("l", "r", "r", "r", "r", "r", "r", "r"),
        col.names = linebreak(c("Component", "1985", "2005", "Diff.","Appearing",
            "Disappearing", "Occupation", "Gender", "Structural"))) %>%
    kable_styling(latex_options = c("striped", "condensed", "scale_down"),
        position = "center") %>%
    #add_header_above(c(" ", "M" = 3, " ", "Marginal" = 2)) %>%
    group_rows("Within major groups (sorted by Diff.)", 5, 22) %>%
    row_spec(c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22), font_size = 8) %>%
    cat(file = "Output/diffs.html", sep = "\n")


diffs_plot <- diffs %>%
    filter(OccMain!= "Between major groups") %>%
    mutate(order = c(1, rep(2, 9))) %>%
    arrange(order, diff) %>%
    mutate(order = 1:10) %>%
    mutate(OccMain= ifelse(OccMain == "Total", "Total labor force", paste("Only:", OccMain))) %>%
    select(OccMain, order, group_marginal, unit_marginal, structural, additions, removals) %>%
    gather(component, value, -OccMain, -order) %>%
    mutate(component = recode_factor(component,
        `group_marginal` = "Gender margins",
        `unit_marginal` = "Occupation margins",
        `additions` = "Appearing occupations",
        `removals` = "Disappearing occupations",
        `structural` = "Structural", .ordered = TRUE))

labels <- rev(diffs_plot$OccMain[1:10])
labels[10] <- expression(bold("Total labor force"))

p <- ggplot(diffs_plot, aes(y = value, x = reorder(OccMain, -order), fill = component)) +
    geom_col() +
    geom_hline(yintercept = 0) +
    theme_bw() +
    theme(axis.title.y = element_blank(), legend.position = "bottom",
        legend.title = element_blank()) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_fill_manual(values = ggthemes::colorblind_pal()(5)) +
    scale_x_discrete(labels = labels) +
    ylab("Difference in M") +
    coord_flip()

ggsave("Output/ElbersTotalSegregation02.png", p, width = 8, height = 5)

# examples
spectech <- d %>%
    filter(OccMain == "A", year == 1985 | year == 2005) %>%
    select(-n) %>%
    spread(year, w, sep = "_") %>%
    na.omit() %>%
    ungroup() %>%
    mutate(n_1985 = sum(year_1985), n_2005 = sum(year_2005)) %>%
    group_by(Gender) %>%
    mutate(p_sex_1985 = sum(year_1985) / n_1985, p_sex_2005 = sum(year_2005) / n_2005) %>%
    ungroup() %>%
    mutate(n_counterf = year_1985 * p_sex_2005 / p_sex_1985) %>%
    select(-n_1985, -n_2005, -p_sex_1985, -p_sex_2005)

# 1001 Natural science researchers
spectech %>%
    group_by(OccMinor) %>%
    mutate(n_OccMinor = sum(year_1985)) %>%
    group_by(OccMinor, Gender) %>%
    mutate(p_OccMinor_sex = sum(year_1985) / n_OccMinor) %>%
    filter(OccMinor == 1001)

mutual_local(spectech, "Gender", "OccMinor", weight = "year_1985", wide = TRUE) %>%
    filter(OccMinor == 1001)
mutual_local(spectech, "Gender", "OccMinor", weight = "n_counterf", wide = TRUE) %>%
    filter(OccMinor == 1001)
mutual_local(spectech, "Gender", "OccMinor", weight = "year_2005", wide = TRUE) %>%
    filter(OccMinor == 1001)

# share of men in research in 1985
xm <- 86322 / (8187 + 86322)
# [1] 0.9133733

# 1018 Nursing
spectech %>%
    group_by(OccMinor) %>%
    mutate(n_OccMinor = sum(year_1985)) %>%
    group_by(OccMinor, Gender) %>%
    mutate(p_OccMinor_sex = sum(year_1985) / n_OccMinor) %>%
    filter(OccMinor == 1018)

mutual_local(spectech, "Gender", "OccMinor", weight = "year_1985", wide = TRUE) %>%
    filter(OccMinor == 1018)
mutual_local(spectech, "Gender", "OccMinor", weight = "n_counterf", wide = TRUE) %>%
    filter(OccMinor == 1018)
mutual_local(spectech, "Gender", "OccMinor", weight = "year_2005", wide = TRUE) %>%
    filter(OccMinor == 1018)

# 1044 Designers
spectech %>%
    group_by(OccMinor) %>%
    mutate(n_OccMinor = sum(year_1985)) %>%
    group_by(OccMinor, Gender) %>%
    mutate(p_OccMinor_sex = sum(year_1985) / n_OccMinor) %>% # share by gende
    filter(OccMinor == 1044) %>% 
    select(c(OccMinor,Gender,year_1985,year_2005,n_counterf,
	n_OccMinor, p_OccMinor_sex))

mutual_local(spectech, "Gender", "OccMinor", weight = "year_1985", 
	wide = TRUE,
       se = TRUE, CI = .95, n_bootstrap = 500) %>%
    filter(OccMinor == 1044)
mutual_local(spectech, "Gender", "OccMinor", weight = "n_counterf", 
	wide = TRUE,
       se = TRUE, CI = .95, n_bootstrap = 500) %>%
    filter(OccMinor == 1044)
mutual_local(spectech, "Gender", "OccMinor", weight = "year_2005", 
	wide = TRUE,
       se = TRUE, CI = .95, n_bootstrap = 500) %>%
    filter(OccMinor == 1044)

# plot 1985 and 2005 bootstrap distributions
local1985 <- mutual_local(spectech, "Gender", "OccMinor", weight = "year_1985", 
	wide = TRUE,
       se = TRUE, CI = .95, n_bootstrap = 500)
# 500 bootstrap iterations on 6387805 observations
local2005 <- mutual_local(spectech, "Gender", "OccMinor", weight = "year_2005", 
	wide = TRUE,
       se = TRUE, CI = .95, n_bootstrap = 500)
#500 bootstrap iterations on 8480459 observations
# pick bootstrap distribution of local segregation scores for occupation 1044 
#	Designers

ls_occ1985 <- attr(local1985, "bootstrap")[OccMinor == "1044" & 
	stat == "ls", boot_est]
ls_occ2005 <- attr(local2005, "bootstrap")[OccMinor == "1044" & 
	stat == "ls", boot_est]

png("Images/LocalSegregationDesigners.png")
par(mfrow=c(2,1))
hist(ls_occ1985, xlab="1985",xlim=c(0,0.006),cex.sub=0.7,
	sub="500 bootstrap iterations on 6387805 observations",
	main = "Local segregation for designers")
#text( 0.001,150,pos=4, "500 bootstrap iterations on 6387805 observations")
hist(ls_occ2005, sub="500 bootstrap iterations on 8480459 observations",
	cex.sub=0.7,
	xlab="2005", main="",xlim=c(0,0.006))
dev.off()


##################################################
###### analysis: detailed structural change ######
##################################################


# structural change
mutual_difference(
    filter(d, OccMain == "A", year == 1985),
    filter(d, OccMain == "A", year == 2005), method = "shapley_detailed",
    "Gender", "OccMinor", weight = "w", precision = .000001) %>%
    as_tibble %>%
    mutate(structural = est[stat == "structural"][[1]]) %>%
    filter(stat == "total") %>%
    mutate(p = est / structural) %>%
    arrange(desc(est)) %>%
    filter(abs(p) > .05)

# Order by OccMinor
diffspectech <- mutual_difference(
    filter(d, OccMain == "A", year == 1985),
    filter(d, OccMain == "A", year == 2005), method = "shapley_detailed",
    "Gender", "OccMinor", weight = "w", precision = .000001) %>%
    as_tibble %>%
    mutate(structural = est[stat == "structural"][[1]]) %>%
    filter(stat == "total") %>%
    mutate(p = est / structural) %>%
    arrange(OccMinor) 

diffspectech %>% print(n = Inf)




