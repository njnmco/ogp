allocations <- NULL;

local({

  message("2019 sheet")


  n <- Inf
  skip <- 7
  sheet <- 1

  # read file twice, once to get # of lines, second so that extra lines don't mess up data types
  for(i in 1:2) {
    allocations <- readxl::read_xlsx(
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
                "Project_Description", "Office_Location","decreased_score_q", "tail")] <- NULL


  allocations$OGP_Budget_Category <- as.factor(allocations$OGP_Budget_Category)
  allocations$District_Most_Activity <- as.factor(allocations$District_Most_Activity)

  message("2019 pdf")


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


  ### Fix typo
  tbl$City <- sub("Norh Hollywood", "North Hollywood", tbl$City)

  tbl[1:3] <- lapply(tbl[1:3], trimws)

  ## Check for perfect match
  stopifnot(
    identical(sort(allocations$Organization), sort(tbl$Organization)))

  message("2019 merge")

  allocations19 <- merge(allocations, tbl)
  allocations19$Year <- 19

  message("2018 sheet")

  ## 2018 data

  n <- Inf
  skip <- 7
  sheet <- 2

  # read file twice, once to get # of lines, second so that extra lines don't mess up data types
  for(i in 1:2) {
    allocations18 <- readxl::read_xlsx(
      "Allocations - FY1516 to FY1920.xlsx",
      skip = skip, sheet = sheet, n_max=n,
      col_names =
        c("Organization", "FEIN",
          "Request", "Cutoff",
          "RawScore", "ScorePercent",
          "Proposed_100_XScore",
          "Proposed_50",
          "Rounded_Allocation_Final Award",
          "Budget_Size",
          "Funding_Year",
          "Score",
          "Total_Grant_Award",
          "skip",
          "Variance_Value",
          "NOTES",
          "Decrease_award_per",
          "Decrease_score",
          "Budget_Decrease",
          "Project_Description",
          "Office_Location",
          "District_Most_Activity",
          "District")
    )
    n <- sum(!is.na(allocations18$RawScore))
  }

  # Drop unneeded columns
  allocations18[c("FEIN", "Request", "Cutoff",
                "RawScore", "Proposed_100_XScore", "Proposed_50",
                "Rounded_Allocation_Final Award", "Funding_Year",
                "Score", "Total_Grant_Award", "Prior_Budget_Size", "Variance_Value",
                "NOTES", "Decrease_award_per", "Decrease_score", "Budget_Decrease",
                "Project_Description", "Office_Location",
                "skip")] <- NULL


  allocations18 <- cbind(OGP_Budget_Category=as.factor(cut(allocations18$Budget_Size, c(0, 100*1000, 1.5*1000000, 40*1000000, Inf), labels = FALSE)), allocations18)
  allocations18$District_Most_Activity <- as.factor(allocations18$District_Most_Activity)

  message("2018 pdf")

  tbl <- tabulizer::extract_tables("ogp_1819_grantees_-_list_for_website.pdf", method="lattice")
  for( i in seq_along(tbl)) {
    tbl[[i]] <- gsub("\r", " ", tbl[[i]][-1,])
  }
  tbl <- as.data.frame(do.call(rbind, tbl), stringsAsFactors = FALSE)
  colnames(tbl) <- c("Organization", 'City', 'Award Amount', 'Discipline', 'Project Description', 'OGP Budget Category', 'Year')
  tbl <- subset(tbl, Organization != '',select = c('Organization', 'City', 'Discipline'))

  ### Standardize Disciplines
  tbl$Multidisciplinary <- grepl("^Multidisciplinary", tbl$Discipline)
  tbl$Arts_Education <- grepl("^Arts Education", tbl$Discipline)
  tbl$Discipline <- sub("^Multidisciplinary -", "", tbl$Discipline)
  tbl$Discipline <- sub("^Arts Education -", "", tbl$Discipline)
  tbl$Discipline <- sub("(?<=Music)[^A-Z]+", " - ", tbl$Discipline, perl=TRUE)


  ##Standardize between years
  tbl$Discipline <- sub("Traditional and Folk", "Traditional and Folk Art", tbl$Discipline)
  tbl$Discipline <- sub("Literary", "Literary Arts", tbl$Discipline)

  ### Fix typo
  tbl$City <- sub("Lo ?s  ?An ?geles", "Los Angeles", tbl$City)
  tbl$City <- sub("So uth Pasadena", "South Pasadena", tbl$City)
  tbl$City <- sub("Palos Verdes Pen", "Palos Verdes Peninsula", tbl$City)


  tbl[1:3] <- lapply(tbl[1:3], trimws)


  allocations18$Organization <- sub("^The ", "", allocations18$Organization)
  tbl$Organization <- sub("^The ", "", tbl$Organization)

  #CHECK WITH ROSALYN
  tbl$Organization <- sub("Fulcrum Arts", "Pasadena Arts Council", tbl$Organization)

  # More orgs are not matching "Inc vs Inc.", replace
  mismatch <- cbind(sort(setdiff(allocations18$Organization, tbl$Organization)), sort(setdiff(tbl$Organization, allocations18$Organization)))
  mismatch <- setNames(mismatch[,1], mismatch[, 2])
  tbl$Organization[tbl$Organization %in% names(mismatch)] <- mismatch[tbl$Organization[tbl$Organization %in% names(mismatch)]]



  ## Check for perfect match
  stopifnot(
    identical(sort(allocations18$Organization), sort(tbl$Organization))
  )

  message("2018 merge")


  allocations18 <- merge(allocations18, tbl)
  allocations18$Year <- 18

  message("stack 2019 and 2018")

  print(colnames(allocations19))

  print(colnames(allocations18))


  allocations <<- rbind(allocations19, allocations18)

})
