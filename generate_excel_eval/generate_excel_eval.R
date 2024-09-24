#!/usr/local/bin/Rscript

packages <- c("openxlsx", "dplyr", "tidyr", "tibble", "haven", "stringr", "tools", "confintr", "lsr")
packagecheck <- match(packages, utils::installed.packages()[, 1])
packagestoinstall <- packages[is.na(packagecheck)]
if (length(packagestoinstall) > 0L) {
    utils::install.packages(packagestoinstall, repos = "https://ftp.fau.de/cran")
}

library(openxlsx)
library(dplyr)
library(tidyr)
library(tibble)
library(haven)
library(stringr)
library(tools)
library(confintr)
library(lsr)

#pair-wise Pearson coeff table for each exp group?
groupwise_corr <- FALSE

cfg_file <- ""
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
    print(paste("Processing", args[1], "..."))
    cfg_file <- args[1]
} else {
    suppressPackageStartupMessages(library(radiant.data))
    cfg_file <- choose_files("r", "R")[1]
}
stopifnot(cfg_file != "")

excel_col_names <- NULL
gr_ID_name <- NULL
gr_IDs <- NULL
sourceDF <- NULL
wb_descr <- NULL
curr_row <- 2

merge_exp_columns <- function(varname, col_name_list) {
    stopifnot(length(col_name_list) == length(gr_IDs))
    .data <- sourceDF
    attrib <- NULL
    res <- rep(NA, nrow(.data))
    for (gridx in gr_IDs) {
        colnames <- unlist(col_name_list[match(gridx, gr_IDs)])
        idx_curr_grp <- (!is.na(.data[[gr_ID_name]])) & (.data[[gr_ID_name]] == gridx)
        if (!is.null(colnames)) {
            n_all <- 0
            for (j in c(1:length(colnames))) {
                stopifnot(!is.null(colnames[j]))
                colvals <- .data[[colnames[j]]]
                colvals[is.na(.data[[gr_ID_name]])] <- NA
                encoding_idxs <- unname(attributes(colvals)$labels)
                idx_list <- idx_curr_grp & !is.na(colvals)
                if (length(encoding_idxs) > 1) {
                    stopifnot(all((.data[[gr_ID_name]])[colvals %in% encoding_idxs] == gridx))
                    idx_list <- idx_curr_grp & (colvals %in% encoding_idxs)
                }
                res[idx_list] <- colvals[idx_list]
                n_all <- n_all + length(colvals[idx_list])
            }
            stopifnot(length(na.omit(res[idx_curr_grp])) == n_all)
            if (is.null(attrib)) {
                attrib <- attributes(.data[[colnames[1]]])
            } else {
                stopifnot(attributes(.data[[colnames[1]]])$labels == attrib$labels)
            }
        }
    }
    .data[[varname]] <- res
    attributes(.data[[varname]]) <- attrib
    sourceDF <<- .data
}

add_subgroup <- function(...) {
    sourceDF <<- mutate(sourceDF, ...)
}

columns_corr <- function(df) {
    dummy <- df[, unlist(lapply(df, is.numeric), use.names = FALSE)]
    dummy[, names(which(sapply(dummy, function(x) length(unique(x))) == 1))] <- NULL
    dummy <- dummy %>% as.matrix %>% cor #cor(use = "pairwise.complete.obs")
    dummy[!upper.tri(dummy)] <- NA
    dummy <- dummy %>%
      as.data.frame %>%
      rownames_to_column(var = "var1") %>%
      gather(var2, value, -var1) %>%
      filter(!is.na(value)) %>%
      arrange(desc(value))
    return(dummy)
}

by_valid_labels <- function(col) {
    attr_lab <- attributes(col)$labels
    if (is.null(attr_lab) || length(attr_lab) < 2) {
        return(!is.na(col))
    } else {
        return(col %in% unname(attr_lab))
    }
}

add_to_excel <- function(ctrl_var, varname, col_name_from_src, FUNC) {
    if (is.list(ctrl_var)) {
        for (j in c(1:length(ctrl_var))) {
            add_to_excel(unlist(ctrl_var[j]), varname, col_name_from_src, FUNC)
        }
        return(invisible())
    }
    stopifnot(!(0 %in% gr_IDs))
    iter_range <- c(0, gr_IDs)
    delta_rows <- 0
    curr_col <- 3
    ctrl_var_str <- ""
    num_grp_data <- length(gr_IDs)
    source_df <- filter(sourceDF, !is.na(.data[[gr_ID_name]]))
    if (!is.null(ctrl_var)) {
        source_df[[ctrl_var]] <- replace_na(source_df[[ctrl_var]], FALSE)
        stopifnot(any(source_df[[ctrl_var]]))
        source_df <- filter(source_df, .data[[ctrl_var]] == TRUE)
        ctrl_var_str <- ctrl_var
    }
    #source_df <- filter(source_df, !is.na(.data[[col_name_from_src]]))
    source_df <- filter(source_df, by_valid_labels(.data[[col_name_from_src]]))
    flt_exp_df <- source_df
    for (gridx in iter_range) {
        df_to_add <- data.frame(vars = character(0), ctrl_by = character(0), values = numeric(0))
        if (gridx != 0) {
            curr_col <- match(gridx, gr_IDs) + 3
            flt_exp_df <- filter(source_df, .data[[gr_ID_name]] == gridx)
        }
        if (all(is.na(flt_exp_df[[col_name_from_src]]))) {
            num_grp_data <- num_grp_data - 1
            next()
        }
        if (!identical(FUNC, table)) {
            df_to_add <- data.frame(vars = varname,
                                    ctrl_by = ctrl_var_str,
                                    values = FUNC(flt_exp_df[[col_name_from_src]])
            )
            delta_rows <- 1
        } else {
            encodings <- names(attributes(flt_exp_df[[col_name_from_src]])$labels)
            encoding_idxs <- unname(attributes(flt_exp_df[[col_name_from_src]])$labels)
            stopifnot(length(encoding_idxs) != 0)
            curr_src_col <- factor(flt_exp_df[[col_name_from_src]], levels = encoding_idxs)
            if (length(unique(flt_exp_df[[col_name_from_src]])) < 2) num_grp_data <- 1
            data <- prop.table(table(curr_src_col))
            enc_name <- encodings[match(as.numeric(names(data)), encoding_idxs)]
            #enc_name <- substr(enc_name, str_locate(enc_name, "\\.")[1] + 2, nchar(enc_name))
            df_to_add <- data.frame(vars = paste0(varname, " <", enc_name, ">"),
                                    ctrl_by = rep(ctrl_var_str, length(data)),
                                    values = as.numeric(unname(data))
            )
            delta_rows <- nrow(df_to_add)
        }
        writeData(wb_descr, sheet = "sample overview", x = df_to_add[, c(1, 2)], startRow = curr_row, startCol = 1, colNames = FALSE)
        writeData(wb_descr, sheet = "sample overview", x = df_to_add[, 3], startRow = curr_row, startCol = curr_col, colNames = FALSE)
    }
    if ((num_grp_data > 1) && identical(FUNC, table)) {
        chi2t <- chisq.test(source_df[[gr_ID_name]], source_df[[col_name_from_src]])
        pval <- chi2t[["p.value"]]
        chi_sq <- chi2t[["statistic"]]
        cramv <- cramersv(chi2t)
        df_to_add <- data.frame(text = "chi^2 Test",
                                label1 = "chi^2 = ",
                                statval = chi_sq,
                                label2 = "P(> chi^2) = ",
                                pvar = pval,
                                label3 = "Cramer's V = ",
                                cvval = cramv,
                                sigstars = ifelse(pval <= 0.05, "*", ifelse(pval <= 0.1, "#", "")))
        writeData(wb_descr, sheet = "sample overview", x = df_to_add, startRow = curr_row, startCol = length(gr_IDs) + 4, colNames = FALSE)
    }
    if ((num_grp_data > 1) && identical(FUNC, mean)) {
        anova <- aov(source_df[[col_name_from_src]] ~ factor(source_df[[gr_ID_name]]))
        pval <- summary(anova)[[1]][1, 5] #or (1 - pf(Fval, k-1, n-k))
        Fval <- summary(anova)[[1]][1, 4]
        eta_sq <- etaSquared(anova)[1, 1]
        df_to_add <- data.frame(text = "ANOVA",
                                label1 = "F = ",
                                statval = Fval,
                                label2 = "P(> F) = ",
                                pvar = pval,
                                label3 = "eta^2 = ",
                                etaval = eta_sq,
                                sigstars = ifelse(pval <= 0.05, "*", ifelse(pval <= 0.1, "#", "")))
        writeData(wb_descr, sheet = "sample overview", x = df_to_add, startRow = curr_row, startCol = length(gr_IDs) + 4, colNames = FALSE)
    }
    if (curr_row == 2) {
        stopifnot(length(excel_col_names) == length(gr_IDs))
        excel_col_names_final <- c(list("Variable", "Subgroup", "All"), excel_col_names)
        writeData(wb_descr, sheet = "sample overview", x = as.data.frame(excel_col_names_final), startRow = 1, startCol = 1, colNames = FALSE)
    }
    curr_row <<- curr_row + delta_rows + 1
}

source(cfg_file)

fext <- file_ext(name_data_file)
if (fext == "dta") {
    sourceDF <<- read_dta(name_data_file)
} else if (fext == "sav") {
    sourceDF <<- read_sav(name_data_file)
}

append_vars()

wb_descr <- createWorkbook()
addWorksheet(wb_descr, "sample overview")
add_data_to_main_overview()

if (groupwise_corr) {
    for (gridx in gr_IDs) {
        df_i <- select(filter(sourceDF, .data[[gr_ID_name]] == gridx), -{{gr_ID_name}})
        ws_name <- paste0("exp", gridx, " - columns pearson")
        addWorksheet(wb_descr, ws_name)
        writeData(wb_descr, sheet = ws_name, x = columns_corr(df_i), startRow = 1, startCol = 1, colNames = TRUE)
    }
}

abs_path_dest <- paste0(dirname(name_data_file), "/", name_excel_file)
saveWorkbook(
  wb_descr,
  file = abs_path_dest,
  overwrite = TRUE
)
print(paste("Finished! Excel file written to", abs_path_dest))
