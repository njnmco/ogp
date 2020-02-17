
allocations <- NULL;

local({

  n <- Inf
  skip <- 7
  sheet <- 1

  # read file twice, once to get # of lines, second so that extra lines don't mess up data types
  for(i in 1:2) {
    allocations <<- readxl::read_xlsx(
      "Allocations - FY1516 to FY1920.xlsx",
      skip = skip, sheet = sheet, n_max=n,
      col_names =
        c("OGP_Budget_Category", "Organization", "FEIN",
          "Request", "Cutoff",
          "RawScore", "ScorePercent",
          "Proposed_100_XScore",
          "Proposed_50",
          "Rounded_Allocation_Final Award",
          "Budget_Size",
          "Funding_Year",
          "Score",
          "Total_Grant_Award", "Prior_Budget_Size",
          "Variance_Value", "NOTES", "Decrease_award_per", "Decrease_score",
          "Budget_Decrease", "Project_Description", "Office_Location",
          "District_Most_Activity", "District", "decreased_score_q",
          "tail")
    )
    n <- sum(!is.na(allocations$OGP_Budget_Category))
  }

  # Drop unneeded columns
  allocations[c("FEIN", "Request", "Cutoff",
                "RawScore", "Proposed_100_XScore", "Proposed_50",
                "Rounded_Allocation_Final Award", "Funding_Year",
                "Score", "Total_Grant_Award", "Prior_Budget_Size", "Variance_Value",
                "NOTES", "Decrease_award_per", "Decrease_score", "Budget_Decrease",
                "Project_Description", "Office_Location", "District_Most_Activity",
                "District", "decreased_score_q", "tail")] <- NULL


  tbl <- tabulizer::extract_tables("ogp_1920_list_of_grantees_for_web.pdf")
  nms <- setNames(nm=tbl[[1]][1,])
  nms["Grantee"] = "Organization" # To match allocations spreadsheet for merging
  for( i in seq_along(tbl)) {
    tbl[[i]] <- gsub("\r", " ", tbl[[i]][-1,])
  }
  tbl <- as.data.frame(do.call(rbind, tbl), stringsAsFactors = FALSE)
  colnames(tbl) <- nms
  tbl <- subset(tbl, select = c('Organization', 'City', 'Discipline'))

  ### Standardize Disciplines
  tbl$Multidisciplinary <- grepl("^Multidisciplinary", tbl$Discipline)
  tbl$Arts_Education <- grepl("^Arts Education", tbl$Discipline)
  tbl$Discipline <- sub("^Multidisciplinary -", "", tbl$Discipline)
  tbl$Discipline <- sub("^Arts Education -", "", tbl$Discipline)
  tbl$Discipline <- sub("(?<=Music)[^A-Z]+", " - ", tbl$Discipline, perl=TRUE)




  tbl[1:3] <- lapply(tbl[1:3], trimws)

  ## Check for perfect match
  stopifnot(
    identical(sort(allocations$Organization), sort(tbl$Organization)))



  allocations <<- merge(allocations, tbl)

})
