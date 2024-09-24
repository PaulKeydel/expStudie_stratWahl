#!/usr/local/bin/Rscript

library(dplyr)
library(tidyr)
library(tibble)
library(haven)
library(stringr)
library(tools)
library(confintr)
library(lsr)
library(ggplot2)
library(ggsignif)
library(gridExtra)

df <- read_sav("data_mehrheit_faas_keydel.sav")
df <- filter(df, !is.na(expgroup_WK_sum))
exp_grp_names_WK <- c("SPD vor Grüne", "SPD vor CDU", "CDU vor SPD", "Grü min. vor SPD", "AfD vor CDU", "AfD min. vor CDU", "Grü min. vor CDU", "Linke vor CDU")
gr_ID_WKs <- c(c(11:14), c(31:34))
exp_grp_names_states <- c("NDS22: SPD vor CDU, FDP <5%", "NDS23: SPD vor CDU", "SAC19: CDU vor AfD, FDP <5%", "SAC23: AfD vor CDU")
gr_ID_states <- c(1:4)
party_names <- c("SPD", "CDU", "Grüne", "FDP", "AfD", "Linke", "sonst")

merge_exp_columns <- function(gr_IDs, gr_ID_name, varname, col_name_list) {
    stopifnot(length(col_name_list) == length(gr_IDs))
    .data <- df
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
    df <<- .data
}

by_valid_labels <- function(col) {
    attr_lab <- attributes(col)$labels
    if (is.null(attr_lab) || length(attr_lab) < 2) {
        return(!is.na(col))
    } else {
        return(col %in% unname(attr_lab))
    }
}

get_sig_string <- function(pval) {
    sig_symbols <- symnum(pval, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", "\u207A", " "))
    str <- paste0("p = ", formatC(pval, format = "e", digits = 2), sig_symbols)
    return(str)
}

merge_exp_columns(gr_ID_WKs, "expgroup_WK_sum", "erststimme_graben", list(list("v_54", "v_78"), list("v_72", "v_80"), list("v_74", "v_82"), list("v_76", "v_84"),
                                                                          list("v_86", "v_100"), list("v_94", "v_102"), list("v_96", "v_104"), list("v_98", "v_106"))
)
merge_exp_columns(gr_ID_WKs, "expgroup_WK_sum", "erststimme_mregel", list(list("v_55", "v_79"), list("v_73", "v_81"), list("v_75", "v_83"), list("v_77", "v_85"),
                                                                          list("v_87", "v_101"), list("v_95", "v_103"), list("v_97", "v_105"), list("v_99", "v_107"))
)
merge_exp_columns(gr_ID_states, "expgroup_general", "zweitstimme_graben", list("v_49", "v_60", "v_80945", "v_68"))
merge_exp_columns(gr_ID_states, "expgroup_general", "zweitstimme_mregel", list("v_50", "v_61", "v_7b650", "v_69"))

calc_ptv_order <- function() {
    res <- data.frame(ptv_first = numeric(0),
                      ptv_second = numeric(0),
                      place1_parties = I(list()),
                      place2_parties = I(list()),
                      place3_parties = I(list())
    )
    for (j in c(1:nrow(df))) {
        ptvs <- as.numeric(c(df[j, "v_32"], df[j, "v_33"], df[j, "v_34"], df[j, "v_35"], df[j, "v_36"], df[j, "v_37"]))
        ptvs[ptvs == 0] <- NA
        if (all(is.na(ptvs))) {
            res[nrow(res) + 1, ] <- list(ptv_first = NA,
                                         ptv_second = NA,
                                         place1_parties = list(c()),
                                         place2_parties = list(c()),
                                         place3_parties = list(c())
            )
            next()
        }
        valid_ptv_range <- unique(sort(ptvs, na.last = NA, decreasing = TRUE))
        stopifnot(max(valid_ptv_range) <= 11 && min(valid_ptv_range) >= 1)
        stopifnot(!is.na(valid_ptv_range[1]))
        valid_ptv_range <- c(valid_ptv_range, rep(NA, 6 - length(valid_ptv_range)))
        ptv_1_parties <- which(ptvs == valid_ptv_range[1])
        ptv_2_parties <- c()
        if (!is.na(valid_ptv_range[2])) ptv_2_parties <- which(ptvs == valid_ptv_range[2])
        ptv_3_parties <- c()
        if (!is.na(valid_ptv_range[3])) ptv_3_parties <- which(ptvs <= valid_ptv_range[3])
        res[nrow(res) + 1, ] <- list(ptv_first = valid_ptv_range[1],
                                     ptv_second = valid_ptv_range[2],
                                     place1_parties = list(ptv_1_parties),
                                     place2_parties = list(ptv_2_parties),
                                     place3_parties = list(ptv_3_parties)
        )
    }
    df <<- cbind(df, res)
}
calc_ptv_order()
print("#########################")
print("  PTV Statistiken")
print("#########################")
print("Abstände PTV Erst- und Zweitwahl:")
print(table(df$ptv_first - df$ptv_second))
print("PTV Erstwahl vs Parteiident:")
print(addmargins(table(unlist(df$place1_parties))))
print(addmargins(table(df$v3100, useNA = "ifany")))
first_ptv_equals_parteiident <- rep(NA, nrow(df))
for (row_idx in which((df$v3100 >= 1) & (df$v3100 <= 6))) {
    if (!is.na(df$ptv_first[row_idx])) {
        first_ptv_equals_parteiident[row_idx] <- df$v3100[row_idx] == (df$place1_parties[[row_idx]])[1]
    }
}
print(table(first_ptv_equals_parteiident, useNA = "always"))
print("Zusammensetzung PTV Zweitwahl:")
for (p in c(1:6)) {
    curr_party_first <- !is.na(df$ptv_second) & sapply(df$place1_parties, function(x) p %in% x)
    stopifnot(sum(sapply(df$place1_parties, function(x) p %in% x)) == as.numeric(table(unlist(df$place1_parties)))[p])
    print(paste(party_names[p], "auf Platz 1:"))
    print(table(unlist(df$place2_parties[curr_party_first])))
}
#interests and knowledge
df <- mutate(df, idx_knowledge = v_19 + v_180 + v_181)
print("#########################")
print("  Statistiken zu Wissen")
print("#########################")
hist(df$idx_knowledge[df$idx_knowledge != 0], xlab = "Wissens-Index (3...15)")
df$d_elect_fav <- rep(NA, nrow(df))
for (i in c(1:nrow(df))) {
    p_elect <- df$erststimme_graben[i]
    if (is.na(p_elect) || p_elect < 1 || p_elect > 6) next()
    df$d_elect_fav[i] <- df$ptv_first[i] - (df[[paste0("v_", 31 + p_elect)]])[i]
}
print(addmargins(table(df$d_elect_fav, useNA = "always")))
ggplot(df, aes(x = idx_knowledge, y = d_elect_fav, group = idx_knowledge)) +
    geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
    scale_y_continuous(limits = c(0, 10), oob = scales::squish) +
    stat_summary(fun.y = mean, geom = "point", shape = 20, size = 6, color = "red", fill = "red") +
    xlab("Wissens-Index (3...15)") +
    ylab("PTV(fav. Partei) - PTV(gew. Partei)") +
    ggtitle("PTV-Distanz in Abhängigkeit zur Informiertheit")
print("Verständnis politischer Fragen:")
print(table(df$v_180, useNA = "always"))
print("Kenntnisse zum Wahlrecht:")
print(table(df$v_181, useNA = "always"))
print("Verfolgung des Wahlsystemsdebatte:")
print(table(df$v_19, useNA = "always"))
print("Additiver Index (3...15):")
print(table(df$idx_knowledge, useNA = "always"))
print("paarweise Korrelationen:")
print(cor(df$v_180, df$v_181, method = "pearson"))
print(cor(df$v_180, df$v_19, method = "pearson"))
print(cor(df$v_181, df$v_19, method = "pearson"))


#stacked barplot of mregel and graben
barplot_mregel_graben <- function(gr_IDs, gr_ID_col, gr_ID_names, mregel_col, graben_col, ..., title = "") {
    dfplt <- data.frame(values = numeric(0),
                        expgroup = character(0),
                        Wahlsystem = character(0),
                        Partei = character(0))
    .data <- df
    if (!missing(...)) {
        stopifnot(title != "")
        .data <- filter(df, ...)
    }
    annotation_df <- data.frame(expgroup = character(0), label = character(0))
    for (gridx in gr_IDs) {
        mregel <- pull(filter(.data, {{gr_ID_col}} == gridx), {{mregel_col}})
        graben <- pull(filter(.data, {{gr_ID_col}} == gridx), {{graben_col}})
        mregel <- factor(mregel, levels = c(1:7))
        graben <- factor(graben, levels = c(1:7))
        annotation_df <- rbind(annotation_df, data.frame(expgroup = gr_ID_names[match(gridx, gr_IDs)],
                                                         label = get_sig_string(chisq.test(mregel, graben, simulate.p.value = TRUE)[["p.value"]]))
        )
        dfplt <- rbind(dfplt, data.frame(values = 100 * as.numeric(prop.table(table(mregel))),
                                         expgroup = gr_ID_names[match(gridx, gr_IDs)],
                                         Wahlsystem = rep("PVWahl", 7),
                                         Partei = factor(party_names, levels = rev(party_names)))
        )
        dfplt <- rbind(dfplt, data.frame(values = 100 * as.numeric(prop.table(table(graben))),
                                         expgroup = gr_ID_names[match(gridx, gr_IDs)],
                                         Wahlsystem = rep("Graben", 7),
                                         Partei = factor(party_names, levels = rev(party_names)))
        )
    }
    suppressWarnings(ggplot() +
        geom_bar(data = dfplt, aes(y = values, x = Wahlsystem, fill = Partei), stat = "identity", position = "stack") +
        scale_fill_manual(values = c("grey", "orange", "lightblue", "yellow", "green", "darkblue", "red")) +
        geom_signif(data = annotation_df, aes(xmax = "Graben", xmin = "PVWahl", annotations = label, y_position = 105), textsize = 3, manual = TRUE) +
        theme_bw() +
        ylab("Stimmenanteil [%]") +
        ggtitle(paste0(title, " (N = ", nrow(.data), ")")) +
        facet_grid(~ expgroup)
    )
}

#barplot of party identifications for a set of elected parties in each group
barplot_pident_mregel_graben <- function(gr_IDs, gr_ID_col, gr_ID_names, IDs_to_be_printed, mregel_col, graben_col, party_elect, ..., title_prefix = "") {
    ident_names <- c("Partei ist Erstident", paste("Partei ist ZI, EI =", party_names[1:6]), "sonst oder k.A.")
    joint_plot <- (length(IDs_to_be_printed) > 1)
    .data <- df
    if (joint_plot) stopifnot(length(IDs_to_be_printed) == length(party_elect))
    if (!missing(...)) {
        .data <- filter(df, ...)
    }
    dfplt <- data.frame(values = numeric(0),
                        expgroup = character(0),
                        Wahlsystem = character(0),
                        Parteiident = character(0)
    )
    for (gridx in gr_IDs) {
        if (!(gridx %in% IDs_to_be_printed)) {
            next()
        }
        ..data <- filter(.data, {{gr_ID_col}} == gridx)
        mregel <- factor(pull(..data, {{mregel_col}}), levels = c(1:7))
        graben <- factor(pull(..data, {{graben_col}}), levels = c(1:7))
        party_votes_mregel <- length(mregel)
        party_votes_graben <- length(graben)
        party_range <- party_elect
        if (joint_plot) {
            party_range <- party_elect[match(gridx, IDs_to_be_printed)]
        } else {
            dfplt <- dfplt[NULL, ]
        }
        for (p in party_range) {
            ..data <- mutate(..data, party_ident_col = NA)
            for (row_idx in c(1:nrow(..data))) {
                if (p %in% (..data$place1_parties[[row_idx]])) {
                    ..data$party_ident_col[row_idx] <- 0
                } else if (p %in% (..data$place2_parties[[row_idx]])) {
                    erst_ident <- ..data$place1_parties[[row_idx]]
                    ..data$party_ident_col[row_idx] <- erst_ident[1]
                }
            }
            party_ident <- factor(filter(..data, {{mregel_col}} == p)$party_ident_col, levels = c(0:6))
            dfplt <- rbind(dfplt, data.frame(values = 100 * as.numeric(table(party_ident, useNA = "always")) / party_votes_mregel,
                                             expgroup = paste(ifelse(joint_plot, paste(gr_ID_names[match(gridx, gr_IDs)], "\n"), ""), party_names[p], "gewählt"),
                                             Wahlsystem = rep("PVWahl", length(ident_names)),
                                             Parteiident = factor(ident_names, levels = rev(ident_names)))
            )
            party_ident <- factor(filter(..data, {{graben_col}} == p)$party_ident_col, levels = c(0:6))
            dfplt <- rbind(dfplt, data.frame(values = 100 * as.numeric(table(party_ident, useNA = "always")) / party_votes_graben,
                                             expgroup = paste(ifelse(joint_plot, paste(gr_ID_names[match(gridx, gr_IDs)], "\n"), ""), party_names[p], "gewählt"),
                                             Wahlsystem = rep("Graben", length(ident_names)),
                                             Parteiident = factor(ident_names, levels = rev(ident_names)))
            )
        }
        title <- "Parteiliche Erst- und Zweit-Identifikation"
        if (!joint_plot) {
            title <- paste("Prognose", gr_ID_names[match(gridx, gr_IDs)], "(", title, ")")
        }
        if (title_prefix != "") title <- paste0(title_prefix, ": ", title)
        if (!joint_plot || (joint_plot && gridx == IDs_to_be_printed[length(IDs_to_be_printed)])) {
            print(ggplot() +
                geom_bar(data = dfplt, aes(y = values, x = Wahlsystem, fill = Parteiident), stat = "identity", position = "stack") +
                scale_fill_manual(values = c("black", "orange", "lightblue", "yellow", "green", "darkblue", "red", "grey")) +
                theme_bw() +
                ylab("Stimmenanteil für jeweilige Partei [%]") +
                ggtitle(title) +
                facet_grid(~ expgroup)
            )
        }
    }
}

barplot_pident_mregel_graben_sum <- function(gr_IDs, gr_ID_col, gr_ID_names, IDs_to_be_printed, mregel_col, graben_col, party_elect, competitive_party, ..., title_prefix = "") {
    ident_names <- party_names[1:6]
    .data <- df
    stopifnot(length(IDs_to_be_printed) == length(party_elect))
    if (!missing(...)) {
        .data <- filter(df, ...)
    }
    dfplt <- data.frame(values = numeric(0),
                        expgroup = character(0),
                        Wahlsystem = character(0),
                        Erstident = character(0),
                        pi_place = character(0)
    )
    for (k in seq_along(IDs_to_be_printed)) {
        gridx <- IDs_to_be_printed[k]
        ..data <- filter(.data, {{gr_ID_col}} == gridx & !is.na(.data$ptv_first))
        mregel <- factor(pull(..data, {{mregel_col}}), levels = c(1:7))
        graben <- factor(pull(..data, {{graben_col}}), levels = c(1:7))
        party_votes_mregel <- length(mregel)
        party_votes_graben <- length(graben)
        p <- party_elect[k]
        ..data <- mutate(..data, p_erst_ident = sapply(..data$place1_parties, function(x) ifelse(p %in% x, p, x[1])))
        for (partys_pi_place in c(1:3)) {
            curr_cases <- sapply(..data[[paste0("place", partys_pi_place, "_parties")]], function(x) p %in% x)
            if (partys_pi_place == 3) {
                curr_cases <- curr_cases & (..data[[paste0("v_", 31 + p)]] > ..data[[paste0("v_", 31 + competitive_party[k])]])
            }
            stopifnot(all(!is.na(curr_cases)))
            party_ident <- factor(filter(..data[curr_cases, ], {{mregel_col}} == p)$p_erst_ident, levels = c(1:6))
            pi_place_name <- case_when(partys_pi_place == 1 ~ "Partei ist Erstident", partys_pi_place == 2 ~ "Partei ist Zweitident", partys_pi_place == 3 ~ "Partei max. Drittident \nPTV(gewählt) > PTV(Konkur)")
            dfplt <- rbind(dfplt, data.frame(values = 100 * as.numeric(table(party_ident, useNA = "ifany")) / party_votes_mregel,
                                                expgroup = paste(gr_ID_names[match(gridx, gr_IDs)], "\n", party_names[p], "gewählt"),
                                                Wahlsystem = rep("PVWahl", length(ident_names)),
                                                Erstident = factor(ident_names, levels = rev(ident_names)),
                                                pi_place = rep(pi_place_name, length(ident_names)))
            )
            party_ident <- factor(filter(..data[curr_cases, ], {{graben_col}} == p)$p_erst_ident, levels = c(1:6))
            dfplt <- rbind(dfplt, data.frame(values = 100 * as.numeric(table(party_ident, useNA = "ifany")) / party_votes_graben,
                                                expgroup = paste(gr_ID_names[match(gridx, gr_IDs)], "\n", party_names[p], "gewählt"),
                                                Wahlsystem = rep("Graben", length(ident_names)),
                                                Erstident = factor(ident_names, levels = rev(ident_names)),
                                                pi_place = rep(pi_place_name, length(ident_names)))
            )
        }
        title <- "Politische Zusammensetzung der Stimmenanteile"
        if (title_prefix != "") title <- paste0(title_prefix, ":\n", title)
        if (k == length(IDs_to_be_printed)) {
            return(ggplot() +
                geom_bar(data = dfplt, aes(y = values, x = Wahlsystem, fill = Erstident), stat = "identity", position = "stack") +
                scale_fill_manual(values = c("orange", "lightblue", "yellow", "green", "darkblue", "red")) +
                theme_bw() +
                ylab("Stimmenanteil für jeweilige Partei [%]") +
                ggtitle(title) +
                facet_grid(pi_place ~ expgroup, scales = "free_y", space = "fixed")
            )
        }
    }
}

#barplot of PTV distances for a set of elected parties in each group
barplot_ptv_dist_mregel_graben <- function(gr_IDs, gr_ID_col, gr_ID_names, IDs_to_be_printed, mregel_col, graben_col, party_elect, ..., title_prefix = "") {
    dist_values <- c(c(0:10), "k.A.")
    joint_plot <- (length(IDs_to_be_printed) > 1)
    .data <- df
    if (joint_plot) stopifnot(length(IDs_to_be_printed) == length(party_elect))
    if (!missing(...)) {
        .data <- filter(df, ...)
    }
    dfplt <- data.frame(values = numeric(0),
                        expgroup = character(0),
                        Wahlsystem = character(0),
                        PTV_Abstand = character(0)
    )
    for (gridx in gr_IDs) {
        if (!(gridx %in% IDs_to_be_printed)) {
            next()
        }
        ..data <- filter(.data, {{gr_ID_col}} == gridx)
        mregel <- factor(pull(..data, {{mregel_col}}), levels = c(1:7))
        graben <- factor(pull(..data, {{graben_col}}), levels = c(1:7))
        party_votes_mregel <- length(mregel)
        party_votes_graben <- length(graben)
        party_range <- party_elect
        if (joint_plot) {
            party_range <- party_elect[match(gridx, IDs_to_be_printed)]
        } else {
            dfplt <- dfplt[NULL, ]
        }
        for (p in party_range) {
            max_ptv <- filter(..data, {{mregel_col}} == p)$ptv_first
            elect_ptv <- filter(..data, {{mregel_col}} == p)[[paste0("v_", 31 + p)]]
            stopifnot(all(!is.na(elect_ptv)))
            ptv_distance <- factor(max_ptv - elect_ptv, levels = c(0:10))
            dfplt <- rbind(dfplt, data.frame(values = 100 * as.numeric(table(ptv_distance, useNA = "always")) / party_votes_mregel,
                                             expgroup = paste(ifelse(joint_plot, paste(gr_ID_names[match(gridx, gr_IDs)], "\n"), ""), party_names[p], "gewählt"),
                                             Wahlsystem = rep("PVWahl", 12),
                                             PTV_Abstand = factor(dist_values, levels = rev(dist_values)))
            )
            max_ptv <- filter(..data, {{graben_col}} == p)$ptv_first
            elect_ptv <- filter(..data, {{graben_col}} == p)[[paste0("v_", 31 + p)]]
            ptv_distance <- factor(max_ptv - elect_ptv, levels = c(0:10))
            dfplt <- rbind(dfplt, data.frame(values = 100 * as.numeric(table(ptv_distance, useNA = "always")) / party_votes_graben,
                                             expgroup = paste(ifelse(joint_plot, paste(gr_ID_names[match(gridx, gr_IDs)], "\n"), ""), party_names[p], "gewählt"),
                                             Wahlsystem = rep("Graben", 12),
                                             PTV_Abstand = factor(dist_values, levels = rev(dist_values)))
            )
        }
        title <- "Abstand der Bewertung zw. favorisierter u. gewählter Partei"
        if (!joint_plot) {
            title <- paste("Prognose", gr_ID_names[match(gridx, gr_IDs)], "(", title, ")")
        }
        if (title_prefix != "") title <- paste0(title_prefix, ":\n", title)
        if (!joint_plot || (joint_plot && gridx == IDs_to_be_printed[length(IDs_to_be_printed)])) {
            return(ggplot() +
                geom_bar(data = dfplt, aes(y = values, x = Wahlsystem, fill = PTV_Abstand), stat = "identity", position = "stack") +
                scale_fill_manual(values = c("black", "#fdff6b", "#fde96e", "#fdd471", "#febe74", "#fea877", "#fe937a", "#fe7d7c", "#fe677f", "#ff5182", "#ff3c85", "#ff2688")) +
                theme_bw() +
                ylab("Stimmenanteil für jeweilige Partei [%]") +
                ggtitle(title) +
                facet_grid(~ expgroup)
            )
        }
    }
}

#plots for <Erststimme>
cairo_pdf(filename = "Rplots_erststimme.pdf", width = 10, height = 6, onefile = TRUE)
barplot_mregel_graben(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, erststimme_mregel, erststimme_graben, title = "Wahlverhalten")
barplot_mregel_graben(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, erststimme_mregel, erststimme_graben, df$erststimme_graben != df$erststimme_mregel, title = "Wahlverhalten: in beiden Wahlsystemen unterschiedlich gewählt")
barplot_mregel_graben(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, erststimme_mregel, erststimme_graben, df$v_1 > 3, title = "Wahlverhalten: starkes bis sehr starkes Politikinteresse")
barplot_mregel_graben(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, erststimme_mregel, erststimme_graben, df$v_66 > df$v_67, title = "Wahlverhalten: Intension ist Bestimmung der Regierungskoa")
barplot_mregel_graben(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, erststimme_mregel, erststimme_graben, (df$v_66 < df$v_67), title = "Wahlverhalten: Intension ist Selbst-Repräsentation")
for (gr in seq_along(gr_ID_WKs)) {
    #barplot_pident_mregel_graben(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, gr_ID_WKs[gr], erststimme_mregel, erststimme_graben, c(1, 2, 3, 6))
    print(barplot_ptv_dist_mregel_graben(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, gr_ID_WKs[gr], erststimme_mregel, erststimme_graben, c(1, 2, 3, 6)))
}

#plots for <Zweitstimme>
cairo_pdf(filename = "Rplots_zweitstimme.pdf", width = 10, height = 6, onefile = TRUE)
barplot_mregel_graben(gr_ID_states, expgroup_general, exp_grp_names_states, zweitstimme_mregel, zweitstimme_graben, title = "Wahlverhalten")
barplot_mregel_graben(gr_ID_states, expgroup_general, exp_grp_names_states, zweitstimme_mregel, zweitstimme_graben, df$zweitstimme_graben != df$zweitstimme_mregel, title = "Wahlverhalten: in beiden Wahlsystemen unterschiedlich gewählt")
barplot_mregel_graben(gr_ID_states, expgroup_general, exp_grp_names_states, zweitstimme_mregel, zweitstimme_graben, df$v_1 > 3, title = "Wahlverhalten: starkes bis sehr starkes Politikinteresse")
barplot_mregel_graben(gr_ID_states, expgroup_general, exp_grp_names_states, zweitstimme_mregel, zweitstimme_graben, df$v_66 > df$v_67, title = "Wahlverhalten: Intension ist Bestimmung der Regierungskoa")
barplot_mregel_graben(gr_ID_states, expgroup_general, exp_grp_names_states, zweitstimme_mregel, zweitstimme_graben, (df$v_66 < df$v_67), title = "Wahlverhalten: Intension ist Selbst-Repräsentation")
for (gr in seq_along(gr_ID_states)) {
    #barplot_pident_mregel_graben(gr_ID_states, expgroup_general, exp_grp_names_states, gr_ID_states[gr], zweitstimme_mregel, zweitstimme_graben, c(1, 2, 3, 4, 6))
    print(barplot_ptv_dist_mregel_graben(gr_ID_states, expgroup_general, exp_grp_names_states, gr_ID_states[gr], zweitstimme_mregel, zweitstimme_graben, c(1, 2, 3, 4, 6)))
}
print(barplot_ptv_dist_mregel_graben(gr_ID_states, expgroup_general, exp_grp_names_states, gr_ID_states, zweitstimme_mregel, zweitstimme_graben, c(4, 4, 4, 4), title_prefix = "Entscheidung über Parlamentseinzug"))
barplot_pident_mregel_graben(gr_ID_states, expgroup_general, exp_grp_names_states, gr_ID_states, zweitstimme_mregel, zweitstimme_graben, c(4, 4, 4, 4), title_prefix = "Entscheidung über Parlamentseinzug")
print(barplot_ptv_dist_mregel_graben(gr_ID_states, expgroup_general, exp_grp_names_states, gr_ID_states[c(1, 2)], zweitstimme_mregel, zweitstimme_graben, c(1, 1), title_prefix = "Ringen um sozialdemokratische Mehrheit"))
barplot_pident_mregel_graben(gr_ID_states, expgroup_general, exp_grp_names_states, gr_ID_states[c(1, 2)], zweitstimme_mregel, zweitstimme_graben, c(1, 1), title_prefix = "Ringen um sozialdemokratische Mehrheit")
print(barplot_ptv_dist_mregel_graben(gr_ID_states, expgroup_general, exp_grp_names_states, gr_ID_states[c(3, 4)], zweitstimme_mregel, zweitstimme_graben, c(2, 2), title_prefix = "Entscheidung gegen rechtsextreme Mehrheit"))
print(barplot_pident_mregel_graben_sum(gr_ID_states, expgroup_general, exp_grp_names_states, gr_ID_states[c(3, 4)], zweitstimme_mregel, zweitstimme_graben, c(2, 2), c(5, 5), title_prefix = "Entscheidung gegen rechtsextreme Mehrheit"))

#Zusammenfassung
inform_thr <- median(df$idx_knowledge)
cairo_pdf(filename = "Rplots_zusammenfassung.pdf", width = 10, height = 6, onefile = TRUE)
barplot_mregel_graben(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, erststimme_mregel, erststimme_graben, title = "Wahlverhalten insgesamt")
barplot_mregel_graben(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, erststimme_mregel, erststimme_graben, df$idx_knowledge < inform_thr, title = "Wahlverhalten politisch wenig Informierte")
barplot_mregel_graben(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, erststimme_mregel, erststimme_graben, df$idx_knowledge >= inform_thr, title = "Wahlverhalten politisch stark Informierte")
cairo_pdf(filename = "Rplots_zusammenfassung_gruppen.pdf", width = 12, height = 6.4, onefile = TRUE)
grid.arrange(
  barplot_ptv_dist_mregel_graben(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, gr_ID_WKs[c(1, 2, 7, 8)], erststimme_mregel, erststimme_graben, c(1, 1, 3, 6), idx_knowledge < inform_thr, title_prefix = "Unterstützung des Erstplatzierten bei wenig Kenntnis"),
  barplot_pident_mregel_graben_sum(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, gr_ID_WKs[c(1, 2, 7, 8)], erststimme_mregel, erststimme_graben, c(1, 1, 3, 6), c(3, 2, 2, 2), idx_knowledge < inform_thr, title_prefix = "Unterstützung des Erstplatzierten bei wenig Kenntnis"),
  ncol = 2)
grid.arrange(
  barplot_ptv_dist_mregel_graben(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, gr_ID_WKs[c(1, 2, 7, 8)], erststimme_mregel, erststimme_graben, c(1, 1, 3, 6), idx_knowledge >= inform_thr, title_prefix = "Unterstützung des Erstplatzierten bei starker Kenntnis"),
  barplot_pident_mregel_graben_sum(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, gr_ID_WKs[c(1, 2, 7, 8)], erststimme_mregel, erststimme_graben, c(1, 1, 3, 6), c(3, 2, 2, 2), idx_knowledge >= inform_thr, title_prefix = "Unterstützung des Erstplatzierten bei starker Kenntnis"),
  ncol = 2)
grid.arrange(
  barplot_ptv_dist_mregel_graben(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, gr_ID_WKs[c(3, 5, 6)], erststimme_mregel, erststimme_graben, c(1, 2, 2), idx_knowledge < inform_thr, title_prefix = "Unterstützung des Zweitplatzierten bei wenig Kenntnis"),
  barplot_pident_mregel_graben_sum(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, gr_ID_WKs[c(3, 5, 6)], erststimme_mregel, erststimme_graben, c(1, 2, 2), c(2, 5, 5), idx_knowledge < inform_thr, title_prefix = "Unterstützung des Zweitplatzierten bei wenig Kenntnis"),
  ncol = 2)
grid.arrange(
  barplot_ptv_dist_mregel_graben(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, gr_ID_WKs[c(3, 5, 6)], erststimme_mregel, erststimme_graben, c(1, 2, 2), idx_knowledge >= inform_thr, title_prefix = "Unterstützung des Zweitplatzierten bei starker Kenntnis"),
  barplot_pident_mregel_graben_sum(gr_ID_WKs, expgroup_WK_sum, exp_grp_names_WK, gr_ID_WKs[c(3, 5, 6)], erststimme_mregel, erststimme_graben, c(1, 2, 2), c(2, 5, 5), idx_knowledge >= inform_thr, title_prefix = "Unterstützung des Zweitplatzierten bei starker Kenntnis"),
  ncol = 2)
