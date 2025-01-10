library(tabulapdf)
library(readxl)

allocations <- NULL;

local({
  if(!grepl("/data$", getwd())) {
    setwd("data")
    on.exit(setwd(".."))
  }

  if(file.exists("allocations.RDS") && file.mtime("allocations.RDS") > file.mtime("allocations.R")) {
    message("using RDS...")
    allocations <<- readRDS("allocations.RDS")
    return()
  }


  message("2425 sheet")

  allocations2425 <- readxl::read_xlsx(
    "Allocations - FY1920 to FY2425.xlsx",
    skip = 1, sheet = "2425", n_max=238,
    col_names =
      c(
        "EIN",
        "Application ID",
        "MODEL A",
        "Budget Category",
        "Score",
        "ScorePercent",                                 #"Decimal Score",
        "Score cutoff",
        "Organization",                           #"Organization Legal Name", #RENAMING
        "DBA (if different from legal name)",
        "Main Office District",
        "District of Programming",
        "Districts Served",
        "First-time Applicant",
        "BudgetSize",                            #"Budget Size" ,
        "Requested Amount",
        "Fundable Request",
        "% of Fundable Request",
        "2nd Reduction",
        "Rounded Award Amount",
        "Previous Score",
        "Previous Award",
        "Award Year",
        "Previous Budget Size",
        "Budget Variance%",
        "Award Variance #",
        "Award Variance %",
        "Score Variance %",
        "NOTES",
        "Decreased budget",
        "Discipline",
        "Brief Project Description",
        'Objective'                                      #"Project Category"



      )
  )

  allocations2425 <- allocations2425[, c("Organization", "BudgetSize", "ScorePercent", "Objective")]


  message("2425 pdf")
  if(!file.exists("2425.pdf")) {
    message("downloading")
    download.file("https://www.lacountyarts.org/sites/default/files/2024-07/2425_OGPGranteeList_0.pdf", "2425.pdf")
  }

  pg2425 <- tabulapdf::extract_tables("2425.pdf", method = "lattice", output="matrix") |>
    lapply(function(x,body=x[-1,],hdr=x[1,]) `colnames<-`(body, hdr)) |>
    do.call(what=rbind) |>
    as.data.frame()

  pg2425$`Grantee Name` <- gsub("\\r", " ", pg2425$`Grantee Name`)

  pg2425 <- with(pg2425, data.frame(Organization=`Grantee Name`, Year='2425', City=City, Discipline=Discipline, District=`District of Most\rProgramming:`, OGPCat=`OGP\rBudget\rCategory`))

  ###
  stopifnot(all.equal(
   sort(pg2425$Organization),
   sort(allocations2425$Organization)
  ))


  ###
  message("2324 sheet")


  allocations2324 <- readxl::read_xlsx(
    "Allocations - FY1920 to FY2425.xlsx",
    skip = 1, sheet = "2324", n_max=237,
    col_names = c(
      'EIN',
      'Application ID',
      'MODEL A fiscal sponsorship',
      'Budget Category',
      'Score',
      'ScorePercent',                                                    #'n',
      'Score cutoff',
      'Organization',                                                    #'Organization Legal Name',
      'Popular Name or DBA (if different from legal name)',
      'District where Main (Administrative/Office) is located:',
      'District where most of your programming takes place:',
      'District(s) organization serves (check all that apply):',
      'First-time Applicant',
      'BudgetSize',#                                                              'Budget Size',
      'Requested Amount:',
      'Fundable Request:',
      'Discipline',
      'Brief Project Description',
      'Objective',                                                                 #'Project Category',
      '% of Fundable Request',
      '2nd Reduction',
      'Rounded Award Amount',
      'Previous Score',
      'Previous Award',
      'Award Year',
      'Previous Budget Size',
      'Budget Variance%',
      'Award Variance (Decrease) %',
      'Score Variance (Decrease) %',
      'NOTES',
      'Decreased budget'
    )
  )

  allocations2324 <- allocations2324[, c("Organization", "BudgetSize", "ScorePercent", "Objective")]



  message("2324 pdf")
  if(!file.exists("2324.pdf")) {
    message("downloading")
    download.file("https://www.lacountyarts.org/sites/default/files/2023-07/2324OGP-Grantees.pdf", "2324.pdf")
  }

  pg2324 <- tabulapdf::extract_tables("2324.pdf", method = "lattice", output="matrix") |>
    lapply(function(x,body=x[-1,],hdr=x[1,]) `colnames<-`(body, hdr)) |>
    do.call(what=rbind) |>
    as.data.frame()

  pg2324$`Grantee Name` <- gsub("\\r", " ", pg2324$`Grantee Name`)

  pg2324 <- with(pg2324, data.frame(Organization=`Grantee Name`, Year='2324', City=City, Discipline=Discipline, District=`District of Most\rProgramming:`, OGPCat=paste("OGP", `OGP Budget\rCategory`)))

  ## Was left off website per Ros email 12/23/24
  pg2324 <- rbind(
    pg2324,
    data.frame(Organization='Wallis Annenberg Center for the Performing Arts', Year='2324', City='Beverly Hills', Discipline='Multidisciplinary - Performing Arts', District=3, OGPCat="OGP 3")
  )


  ## Fixes
#  allocations2324 <- allocations2324[-grep('Wallis Annenberg Center for the Performing Arts', allocations2324$Organization), ]

  ###
  stopifnot(all.equal(
    sort(pg2324$Organization),
    sort(allocations2324$Organization)
  ))





  ### Old, funding formula only
  allocations2223 <- readxl::read_xlsx(
    "Allocations - FY1920 to FY2425.xlsx",
    skip = 1, sheet = "2223", n_max=227,
    col_names =
  c('Allocation/FundableRequest',
    'First-time applicant',
    'Budget Category',
    'Organization', #'Organization Legal Name',
    'Popular Name or DBA',
    'BudgetSize',#Budget Size',
    'Request',
    'Score Cut-off',
    'Score',
    "ScorePercent",#'Score as %',
    'Office Location',
    'District of Most Activity',
    'District(s) Served',
    '1st Request Reduction ($4.5M)',
    '2nd Reduction ($4.5M)',
    'Rounded Allocation ($4.5M)',
    'Prior Score',
    'Prior Grant Award',
    'Prior Budget Size',
    'Award Variance Value ($4.5M)',
    'Award Variance %',
    'Score Variance',
    'Score Variance %',
    'Budget Size Variance %',
    'Budget Size Variance',
    'NOTES',
    'FEIN Number',
    "skip",
    "skip2"
  ))


  allocations2122 <- readxl::read_xlsx(
    "Allocations - FY1920 to FY2425.xlsx",
    skip = 6, sheet = "2122", n_max=227,
    col_names =c(
      'First-time applicant',
      'Budget Category',
      'Organization',#'Organization Legal Name',
      'Popular Name or DBA',
      'BudgetSize',#'Budget Size',
      'Request',
      'Score Cut-off',
      'Score',
      "ScorePercent",#'Score as %',
      'Office Location',
      'District of Most Activity',
      'District(s) Served',
      '1st Request Reduction ($4.5M)',
      '2nd Reduction ($4.5M)',
      'Rounded Allocation ($4.5M)',
      'Rounded Allocation ($4.5M)',
      'Discipline',
      'Project Category',
      'Project Description',
      'FEIN',
      'Score',
      'Total Grant Award',
      'Prior Budget Size',
      'Variance Value ($4.5M)',
      'Award Variance %',
      'Score Variance %',
      'Budget Size Variance %',
      "skip",
      "skip2",
      "skip",
      "skip2",
      "skip",
      "skip2"
    ))

  allocations2223 <- allocations2223[, c("Organization", "BudgetSize", "ScorePercent")]
  allocations2223$Year <- "2223"


  allocations2122 <- allocations2122[, c("Organization", "BudgetSize", "ScorePercent")]
  allocations2122$Year <- "2122"













  ####

  allocations <<- rbind(
    merge(allocations2425, pg2425),
    merge(allocations2324, pg2324)
  )

  # Widen missing columns
  allocations2223[setdiff(colnames(allocations), colnames(allocations2223))] <- NA
  allocations2122[setdiff(colnames(allocations), colnames(allocations2122))] <- NA

  # append
  allocations <<- rbind(
    allocations,
    allocations2223,
    allocations2122
  )

  saveRDS(allocations, "allocations.RDS")



})




### Legacy Code for 2020 build #################################################
# (function(x){0})({
#
#   message("2019 sheet")
#
#
#   n <- Inf
#   skip <- 7
#   sheet <- 1
#
#   # read file twice, once to get # of lines, second so that extra lines don't mess up data types
#   for(i in 1:2) {
#     allocations <- readxl::read_xlsx(
#       "Allocations - FY1516 to FY1920.xlsx",
#       skip = skip, sheet = sheet, n_max=n,
#       col_names =
#         c("OGP_Budget_Category", "Organization", "FEIN",
#           "Request", "Cutoff",
#           "RawScore", "ScorePercent",
#           "Proposed_100_XScore",
#           "Proposed_50",
#           "Rounded_Allocation_Final Award",
#           "Budget_Size",
#           "Funding_Year",
#           "Score",
#           "Total_Grant_Award", "Prior_Budget_Size",
#           "Variance_Value", "NOTES", "Decrease_award_per", "Decrease_score",
#           "Budget_Decrease", "Project_Description", "Office_Location",
#           "District_Most_Activity", "District", "decreased_score_q",
#           "tail")
#     )
#     n <- sum(!is.na(allocations$OGP_Budget_Category))
#   }
#
#   # Drop unneeded columns
#   allocations[c("FEIN", "Request", "Cutoff",
#                 "RawScore", "Proposed_100_XScore", "Proposed_50",
#                 "Rounded_Allocation_Final Award", "Funding_Year",
#                 "Score", "Total_Grant_Award", "Prior_Budget_Size", "Variance_Value",
#                 "NOTES", "Decrease_award_per", "Decrease_score", "Budget_Decrease",
#                 "Project_Description", "Office_Location","decreased_score_q", "tail")] <- NULL
#
#
#   allocations$OGP_Budget_Category <- as.factor(allocations$OGP_Budget_Category)
#   allocations$District_Most_Activity <- as.factor(allocations$District_Most_Activity)
#
#   message("2019 pdf")
#
#   tbl <- tabulizer::extract_tables("ogp_1920_list_of_grantees_for_web.pdf")
#   nms <- setNames(nm=tbl[[1]][1,])
#   nms["Grantee"] = "Organization" # To match allocations spreadsheet for merging
#   for( i in seq_along(tbl)) {
#     tbl[[i]] <- gsub("\r", " ", tbl[[i]][-1,])
#   }
#   tbl <- as.data.frame(do.call(rbind, tbl), stringsAsFactors = FALSE)
#   colnames(tbl) <- nms
#   tbl <- subset(tbl, select = c('Organization', 'City', 'Discipline'))
#
#   ### Standardize Disciplines
#   tbl$Multidisciplinary <- grepl("^Multidisciplinary", tbl$Discipline)
#   tbl$Arts_Education <- grepl("^Arts Education", tbl$Discipline)
#   tbl$Discipline <- sub("^Multidisciplinary -", "", tbl$Discipline)
#   tbl$Discipline <- sub("^Arts Education -", "", tbl$Discipline)
#   tbl$Discipline <- sub("(?<=Music)[^A-Z]+", " - ", tbl$Discipline, perl=TRUE)
#
#
#   ### Fix typo
#   tbl$City <- sub("Norh Hollywood", "North Hollywood", tbl$City)
#
#   tbl[1:3] <- lapply(tbl[1:3], trimws)
#
#   ## Check for perfect match
#   stopifnot(
#     identical(sort(allocations$Organization), sort(tbl$Organization)))
#
#   message("2019 merge")
#
#   allocations19 <- merge(allocations, tbl)
#   allocations19$Year <- 19
#
#   message("2018 sheet")
#
#   ## 2018 data
#
#   n <- Inf
#   skip <- 7
#   sheet <- 2
#
#   # read file twice, once to get # of lines, second so that extra lines don't mess up data types
#   for(i in 1:2) {
#     allocations18 <- readxl::read_xlsx(
#       "Allocations - FY1516 to FY1920.xlsx",
#       skip = skip, sheet = sheet, n_max=n,
#       col_names =
#         c("Organization", "FEIN",
#           "Request", "Cutoff",
#           "RawScore", "ScorePercent",
#           "Proposed_100_XScore",
#           "Proposed_50",
#           "Rounded_Allocation_Final Award",
#           "Budget_Size",
#           "Funding_Year",
#           "Score",
#           "Total_Grant_Award",
#           "skip",
#           "Variance_Value",
#           "NOTES",
#           "Decrease_award_per",
#           "Decrease_score",
#           "Budget_Decrease",
#           "Project_Description",
#           "Office_Location",
#           "District_Most_Activity",
#           "District")
#     )
#     n <- sum(!is.na(allocations18$RawScore))
#   }
#
#   # Drop unneeded columns
#   allocations18[c("FEIN", "Request", "Cutoff",
#                 "RawScore", "Proposed_100_XScore", "Proposed_50",
#                 "Rounded_Allocation_Final Award", "Funding_Year",
#                 "Score", "Total_Grant_Award", "Prior_Budget_Size", "Variance_Value",
#                 "NOTES", "Decrease_award_per", "Decrease_score", "Budget_Decrease",
#                 "Project_Description", "Office_Location",
#                 "skip")] <- NULL
#
#
#   allocations18 <- cbind(OGP_Budget_Category=as.factor(cut(allocations18$Budget_Size, c(0, 100*1000, 1.5*1000000, 40*1000000, Inf), labels = FALSE)), allocations18)
#   allocations18$District_Most_Activity <- as.factor(allocations18$District_Most_Activity)
#
#   message("2018 pdf")
#
#   tbl <- tabulizer::extract_tables("ogp_1819_grantees_-_list_for_website.pdf", method="lattice")
#   for( i in seq_along(tbl)) {
#     tbl[[i]] <- gsub("\r", " ", tbl[[i]][-1,])
#   }
#   tbl <- as.data.frame(do.call(rbind, tbl), stringsAsFactors = FALSE)
#   colnames(tbl) <- c("Organization", 'City', 'Award Amount', 'Discipline', 'Project Description', 'OGP Budget Category', 'Year')
#   tbl <- subset(tbl, Organization != '',select = c('Organization', 'City', 'Discipline'))
#
#   ### Standardize Disciplines
#   tbl$Multidisciplinary <- grepl("^Multidisciplinary", tbl$Discipline)
#   tbl$Arts_Education <- grepl("^Arts Education", tbl$Discipline)
#   tbl$Discipline <- sub("^Multidisciplinary -", "", tbl$Discipline)
#   tbl$Discipline <- sub("^Arts Education -", "", tbl$Discipline)
#   tbl$Discipline <- sub("(?<=Music)[^A-Z]+", " - ", tbl$Discipline, perl=TRUE)
#
#
#   ##Standardize between years
#   tbl$Discipline <- sub("Traditional and Folk", "Traditional and Folk Art", tbl$Discipline)
#   tbl$Discipline <- sub("Literary", "Literary Arts", tbl$Discipline)
#
#   ### Fix typo
#   tbl$City <- sub("Lo ?s  ?An ?geles", "Los Angeles", tbl$City)
#   tbl$City <- sub("So uth Pasadena", "South Pasadena", tbl$City)
#   tbl$City <- sub("Palos Verdes Pen", "Palos Verdes Peninsula", tbl$City)
#
#
#   tbl[1:3] <- lapply(tbl[1:3], trimws)
#
#
#   allocations18$Organization <- sub("^The ", "", allocations18$Organization)
#   tbl$Organization <- sub("^The ", "", tbl$Organization)
#
#   #CHECK WITH ROSALYN
#   tbl$Organization <- sub("Fulcrum Arts", "Pasadena Arts Council", tbl$Organization)
#
#   # More orgs are not matching "Inc vs Inc.", replace
#   mismatch <- cbind(sort(setdiff(allocations18$Organization, tbl$Organization)), sort(setdiff(tbl$Organization, allocations18$Organization)))
#   mismatch <- setNames(mismatch[,1], mismatch[, 2])
#   tbl$Organization[tbl$Organization %in% names(mismatch)] <- mismatch[tbl$Organization[tbl$Organization %in% names(mismatch)]]
#
#
#
#   ## Check for perfect match
#   stopifnot(
#     identical(sort(allocations18$Organization), sort(tbl$Organization))
#   )
#
#   message("2018 merge")
#
#
#   allocations18 <- merge(allocations18, tbl)
#   allocations18$Year <- 18
#
#   message("stack 2019 and 2018")
#
#   print(colnames(allocations19))
#
#   print(colnames(allocations18))
#
#
#   allocations <<- rbind(allocations19, allocations18)
# })
