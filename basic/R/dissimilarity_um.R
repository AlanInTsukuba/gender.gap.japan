#' dissimilarity_um.R
#' Functions for UMDissimilarityR
#' @references
#' Otis Dudley Duncan and Beverly Duncan. 1955. "A Methodological Analysis of Segregation Indexes,"
#'      American Sociological Review 20(2): 210-217.
#' [UM2020] Uchikoshi, Fumiya, Ryota Mugiyama. (2020) Trends in Occupational Sex Segregation in Japan: 
#'         A Decomposition Analysis of Census Data, 1980-2005. 
#'       Japanese Journal of Population Studies. 2020. 56. 9-23 
#' [Elbers] Benjamin Elbers. 2021. A Method for Studying Differences in
#'   Segregation Across Time and Space Sociological Methods & Research.
#'   doi: 10.1177/0049124121986204

#' @import segregation
#' @import data.table

#' from https://github.com/elbersb/segregation/blob/master/R/segregation.R
globalVariables(c(
    "V1", "V2", "cond1", "cond2", "entropy_cond", "entropy_cond1", "entropy_cond2", "entropyw",
    "est", "freq", "freq1", "freq2", "freq_orig1", "freq_orig2",
    "ls_diff_mean", "ls_diff1", "ls_diff2", "ls_unit", "bias", "boot_est", "est_debiased",
    "n", "n_group", "n_group_target", "n_source", "n_target", "n_unit", "n_unit_target",
    "n_within_group", "p", "p_exp", "p1", "p2", "p_group", "p_group_g_unit", "p_group_g_unit1",
    "p_group_g_unit2", "p_group_s", "p_group_t", "p_unit", "p_unit1", "p_unit2", "p_unit_s",
    "p_unit_t", "p_within", "sumcond1", "sumcond2", "total", "unit1", "unit2",
    ".", "..base", "..fixed_margins", "..group", "..n_bootstrap", "..unit", "se", "stat"))

#' @import data.table
ggj.prepare_data <- function(data, group, unit, weight, within = NULL) {
    if ("data.frame" %in% class(data)) {
        if (nrow(data) == 0) {
            stop("data.frame is empty")
        }
        test_vars <- c(group, unit, weight, within)
        test_vars <- test_vars[!test_vars %in% names(data)]
        if (length(test_vars) > 0) {
            test_vars <- paste0(test_vars, collapse = ", ")
            stop(paste0("variable(s) ", test_vars, " not in data.frame"))
        }
    } else {
        stop("not a data.frame")
    }
    vars <- c(group, unit)

    # create a copy
    data <- as.data.table(data)

    # check whether there is variation
    n_groups <- nrow(data[, .N, by = group])
    n_units <- nrow(data[, .N, by = unit])
    if (n_groups == 1) stop("Cannot compute segregation: the group variable is constant")
    if (n_units == 1) stop("Cannot compute segregation: the unit variable is constant")

    # use provided weight or weight of 1
    weight_no_conflict <- weight
    if (!is.null(weight_no_conflict)) {
        if (weight_no_conflict == "weight") {
            data[, freq := as.double(weight)]
        } else {
            data[, freq := as.double(get(weight_no_conflict))]
        }
    } else {
        data[, freq := 1]
    }

    if (!is.null(within)) {
        vars <- c(vars, within)
    }

    # collapse on vars, and select only positive weights
    data <- data[freq > 0, list(freq = sum(freq)), by = vars]
    setattr(data, "vars", vars)
    setkey(data, NULL)
    data
}

ggj.abs_diff <- function(x) {
    if (length(x) == 1) {
        abs(x)
    } else {
        abs(diff(x))
    }
}

#' @import data.table
ggj.dissimilarity_unit <- function(data, group, unit, weight = NULL) {
    data <- ggj.prepare_data(data, group, unit, weight)
    data[, n_group := sum(freq), by = group]
    data[, 1/2 * ggj.abs_diff(freq / n_group), by = unit]

}

ggj.dissimilarity_all <- function(data, group, unit, weight = NULL) {
    data <- ggj.prepare_data(data, group, unit, weight)
    data[, n_group := sum(freq), by = group]
    est <- 1/2 * data[, ggj.abs_diff(freq / n_group), by = unit][, sum(V1)]
    data.table(stat = "D", est = est, stringsAsFactors = FALSE)
}

## clean up
# rm(ggj.abs_diff,ggj.dissimilarity_all,ggj.dissimilarity_unit,ggj.prepare_data)

