#path to data file
name_data_file <- "path/seideletal.dta"

#name of Excel file
name_excel_file <- "descr_analysis.xlsx"

#group ID in source file
gr_ID_name <- "expgroupkeydel"
gr_IDs <- c(1:3)

#name of the groups for Excel table
excel_col_names <- list("Exp1", "Exp2", "Exp3")

#do some preprocessing and add necessary variables
append_vars <- function() {
    #merge group-specific variables into one variable
    merge_exp_columns("wahlsystem_bewertung", list("v_132", "v_1042", "v_1085"))
    merge_exp_columns("party_votes", list("v_981", "v_982", "v_1083"))
    merge_exp_columns("alternative_party_votes", list(NULL, NULL, "v_1086"))
    #append boolean variables for subgroup evaluations
    add_subgroup(ptv_non_frac_parties = pmax(v_1007, v_1006, v_196, v_995, v_1005, na.rm = TRUE),
                 ptv_frac_parties     = pmax(v_195, v_191, v_194, v_193, v_190, na.rm = TRUE)
    )
    add_subgroup(ptvdiff = (ptv_frac_parties <= ptv_non_frac_parties & ptv_frac_parties > (ptv_non_frac_parties - 2)),
                 pnomem  = (v_758 == 2),
                 keine_frak_partei_gewaehlt = (v_981 %in% c(2, 4, 5, 7, 10, 11)) | (v_982 %in% c(2, 4, 5, 7, 10, 11)) | (v_1083 %in% c(2, 4, 5, 7, 10, 11))
    )
}

add_data_to_main_overview <- function() {
    add_to_excel(NULL, "Teilnehmer", "lfdn", length)
    add_to_excel(NULL, "Parteimitglied", "v_758", table)
    add_to_excel(NULL, "PTV SPD", "v_190", mean)
    add_to_excel(NULL, "PTV CDU", "v_191", mean)
    add_to_excel(NULL, "PTV Grüne", "v_193", mean)
    add_to_excel(NULL, "PTV FDP", "v_194", mean)
    add_to_excel(NULL, "PTV AfD", "v_195", mean)
    add_to_excel(NULL, "PTV LINKE", "v_196", mean)

    add_to_excel(list(NULL, "keine_frak_partei_gewaehlt"), "Wahlsystem Bewertung", "wahlsystem_bewertung", mean)
    add_to_excel(list(NULL, "ptvdiff", "pnomem"), "elected party", "party_votes", table)
    add_to_excel(list(NULL, "ptvdiff", "pnomem"), "alternative vote", "alternative_party_votes", table)
}
