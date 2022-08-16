#!/usr/bin/env Rscript
source("./src/NotesGenerationSetup.R") 
readRenviron("Renviron.txt")
# error_file <-"../output/error_log.txt"
error_file <-"output/error_log.txt"

pkgLoad <- function( packages = "std" ) {

    if( length( packages ) == 1L && packages == "std" ) {
        packages <- c( "data.table", "chron", "plyr", "dplyr", "shiny",
                       "shinyjs", "parallel", "doMC", "utils",
                       "stats", "microbenchmark", "ggplot2", "readxl",
                       "feather", "googlesheets4", "readr", "DT", "knitr",
                       "rmarkdown", "Rcpp", "formattable", "ggnewscale",
                       "htmltools", "lubridate", "stringr", "tidyr", "assertive"
        )
    }

    packagecheck <- match( packages, utils::installed.packages()[,1] )

    packagestoinstall <- packages[ is.na( packagecheck ) ]

    if( length( packagestoinstall ) > 0L ) {
        utils::install.packages( packagestoinstall,
                             repos = "https://cran.case.edu/"
        )
    } else {
        print( "All requested packages already installed" )
    }

    for( package in packages ) {
        suppressPackageStartupMessages(
            library( package, character.only = TRUE, quietly = TRUE )
        )
    }
}

## Ensure all packages are loaded/installed.
pkgLoad()

generate_output <- function(tech_filtered, survey_tms_filtered, survey_results_filtered, patient) {
    print(sprintf("generate_output for %s\n", patient))
    output_file <- sprintf("output/%s.html", patient) # Always run from root
    file.create(output_file)
    cat(sprintf("<!DOCTYPE html>
<html>
  <head>
    <meta charset=\"UTF-8\">
    <title>Entries for Patient: %s</title>
  </head>
  <body>\n", patient), file=output_file, fill=FALSE)
    
    dates <- unique(tech_filtered[c('Date')])
    for (i in 1:nrow(dates)) {
      # For debugging purposes only
      # print(dates[[1]][i])
      output <- generate_day(dates[[1]][i], tech_filtered, survey_tms_filtered, survey_results_filtered)
      # print(class(output))
      # print(length(output))
      # print(output)
      if (output[1] != "") {
        cat(output, file=output_file, fill=FALSE, append=TRUE)
        cat("\n<hr>", file=output_file, fill=FALSE, append=TRUE)
      }
      else
        cat(sprintf("Error in entry for patient, %i\n", patient), file=error_file, fill=FALSE, append=TRUE)
    }
    cat("</body>
</html>", file=output_file, fill=FALSE, append=TRUE)
}


generate_day <- function(cur_date, tms_sessions, survey_tms_filtered, survey_results_filtered) {
  tms_sessions_day <- filter(tms_sessions, Date == cur_date)
  patient_surveys_tms_day <- filter(survey_tms_filtered, Date == cur_date)
  patient_surveys_results_day <- filter(survey_results_filtered, Date == cur_date)
  notes_template <- "\n<hr>\n
\"Date: %s <br />
Stimulation Parameters:<br />
Machine: MagVenture MagPro R30 with Cool-B65 coil.<br />
%s
<br />"
  tms_end_template <- "<br />
Course:  Session # %s of %s planned.<br />"
  patient_survey_start <- "/////-----Patient Mood Surveys-----/////<br />
\"Date: %s -"
  output <- sprintf(notes_template, 
                    format(cur_date, "%m/%d/%y"),
                    if (nrow(tms_sessions_day) > 1) paste0(nrow(tms_sessions_day[c('Treatment #')]), " total sessions each separated by at least 45 minutes<br />") else "",
                    nrow(tms_sessions_day))
  if(nrow(tms_sessions_day) == 1) {
    output <- paste0(output, generate_session(tms_sessions_day[1,]))
    tms_end <- sprintf(tms_end_template, 
                       if (!is.na(tms_sessions_day[1,][c('Treatment #')][[1]])) as.character(tms_sessions_day[1,][c('Treatment #')][[1]]) else "NA",
                       if (!is.na(tms_sessions_day[1,][c('Planned Course')][[1]])) as.character(tail(tms_sessions_day[c('Planned Course')][[1]], 1)) else "NA")
  } else {
    for (i in 1:nrow(tms_sessions_day)) {
      output <- paste0(output, sprintf("Session %s: xx:xx XX", if (is.na(tms_sessions_day[i,][c('Treatment #')][[1]]))
        "x" else as.character(tms_sessions_day[i,][c('Treatment #')][[1]])), "\n<br />")
      output <- paste0(output, generate_session(tms_sessions_day[i,]))
    }
    # print("looping flag 1")
    tms_end <- sprintf(tms_end_template,
                       if (!is.na(tms_sessions_day[1,][c('Treatment #')][[1]])) paste0(tms_sessions_day[1,][c('Treatment #')][[1]], "-", tail(tms_sessions_day[c('Treatment #')][[1]], 1)) else "x-x",
                       if (is.na(tms_sessions_day[1,][c('Planned Course')][[1]])) as.character(tail(tms_sessions_day[c('Treatment #')][[1]], 1)) else as.character(tms_sessions_day[1,][c('Planned Course')][[1]]))
  }
  output <- paste0(output, tms_end)

  if (nrow(survey_tms_filtered) > 0 | nrow(survey_results_filtered) > 0) {
    output <- paste0(output, sprintf(patient_survey_start, format(cur_date, "%m/%d/%y")))
    if (nrow(survey_tms_filtered) > 0) {
        output <- paste0(output, generate_pretms(patient_surveys_tms_day[1,]))
    }
    output <- paste0(output, "\n<br />\"<br />")
    if (nrow(survey_results_filtered) > 0) {
        output <- paste0(output, generate_surveys(cur_date, patient_surveys_results_day))
    }
  } else {
    cat(sprintf("Patient %s has no survey data.\n", patient),
        file=error_file, fill=FALSE, append=TRUE)
  }
  output <- paste0(output, "\"")
}

generate_session <- function(entry) {
    notes_template <- "%s
Protocol A: %s stimulated with %s at %s%% MT (%s%% MO)<br />
%s
Technician Notes: %s<br />
Technician: %s<br />
Supervision: Supervised by %s. %s.<br />
<br />
"
    sprintf(notes_template, 
            if(entry$'Did you re-threshold since the last session?'=="Yes") sprintf("Motor Threshold today was measured to be %s%% MO. It was performed by %s due to %s.<br />",
                                                                                    entry$'What is the new motor threshold?',
                                                                                    entry$'What is the full name of the person that performed the re-threshold?',
                                                                                    entry$'Why did you re-threshold?') else "",
            entry$'What is the first target?',
            entry$'What is the first protocol?',
            as.numeric(entry$'What is the machine output as a percent of the motor threshold for the first target?') * 100,
            entry$'What did you set the machine output for the first target?',
            if(entry$'Is there a second target?' == "Yes") sprintf("<br />Protocol B: %s stimulated with %s at %s%% MT (%s%% MO)<br />",
                                                                   entry$'What is the second target?',
                                                                   entry$'What is the second protocol?',
                                                                   as.numeric(entry$'What is the machine output as a percent of the motor threshold for the second target?') * 100,
                                                                   entry$'What did you set the machine output for the second target?') else "",
            entry$'Please write your notes for this patient (select all that may apply)',
            entry$'Technician\'s Full Name', ## one or the other (above); TODO: Standardize column names
            entry$'Who is the supervising psychiatrist?',
            entry$'What is the supervising level?')
}

generate_pretms <- function(survey_tms_filtered) {
  pretms_template <- "<br />//////////-----Pre-TMS Patient Survey------//////////<br />
  Over the last day, have you felt down/ depressed / sad? [1 of 7] %s <br />
Over the last day, have you felt anxious / on edge / panicked? [2 of 7] %s <br />
Over the last day, have you felt irritable / angry? [3 of 7] %s <br />
Over the last day, have you felt happy /euphoric? [4 of 7] %s <br />
Over the last day, have you felt peaceful / mindful? [5 of 7] %s <br />
Over the last day, have you felt hopeful / optimistic? [6 of 7] %s <br />
Over the last day, have you felt suicidal? [7 of 7] %s <br />
<br />
How many hours did you sleep last night? %s <br />
How was your sleep quality? %s <br />
Have you eaten at least one meal (>400 Calories) since you slept last night? %s <br />
How much caffeine did you consume over the last day? %s <br />
How much alcohol did you consume over the last 24 hours? %s <br />
What drugs (e.g. marijuana) did you use in the last 24 hours? %s <br />
OTC medication: %s <br />
Have you experienced any side effects from treatment in the last day? %s <br />"
  sprintf(pretms_template, survey_tms_filtered$`Over the last day, have you felt down/ depressed / sad? [1 of 7]`,
          survey_tms_filtered$`Over the last day, have you felt anxious / on edge / panicked? [2 of 7]`,
          survey_tms_filtered$`Over the last day, have you felt irritable / angry? [3 of 7]`,
          survey_tms_filtered$`Over the last day, have you felt happy /euphoric? [4 of 7]`,
          survey_tms_filtered$`Over the last day, have you felt peaceful / mindful? [5 of 7]`,
          survey_tms_filtered$`Over the last day, have you felt hopeful / optimistic? [6 of 7]`,
          survey_tms_filtered$`Over the last day, have you felt suicidal? [7 of 7]`,
          survey_tms_filtered$`How many hours did you sleep last night?`,
          survey_tms_filtered$`How was your sleep quality?`,
          survey_tms_filtered$`Have you eaten at least one meal (>400 Calories) since you slept last night?`,
          survey_tms_filtered$`How much caffeine did you consume over the last day? (A cup of coffee has ~100mg, black tea ~50mg, green tea ~30mg)`,
          survey_tms_filtered$`How much alcohol did you consume over the last 24 hours? (1 drink = 1 beer, 1 glass of wine, or 1 shot of liquor)`,
          survey_tms_filtered$`What drugs (e.g. marijuana) did you use in the last 24 hours and how much did you use? Write N/A if none.`,
          survey_tms_filtered$`What over-the-counter medication did you use in the last 24 hours and how much did you use? Leave blank if none.`,
          survey_tms_filtered$`Have you experienced any side effects from treatment in the last day? If so, please describe. Leave blank if none.`)
}

generate_surveys <- function(cur_date, survey_results_filtered) {
  output <- ""
  for (r in 1:nrow(survey_results_filtered)) {
    c <- 3
    while (c <= 9) {
      # print(identical(survey_results_filtered[r,c][[1]], numeric(0)))
      if (length(survey_results_filtered[r,c][[1]]) > 0) {
        if (!is.null(survey_results_filtered[r, c][[1]]) & !is.na(survey_results_filtered[r, c][[1]])) {
          output <- paste0(output, sprintf("Date: %s - %s = %d\n<br />", 
                                           format(cur_date, "%m/%d/%y"), colnames(survey_results_filtered)[c],
                                           survey_results_filtered[r, c][[1]]))
        }
      }
      c <- c + 1
    }
  }
  output
}

cat(sprintf("Error log for %s:\n", Sys.time()), file=error_file, fill=FALSE, append=FALSE)

fetch_patient_id_aliases <- function(demographics, id) {
  lower_id <- tolower(id)
  aliases <- demographics[tolower(demographics$`Patient ID`) %like% lower_id, ]
  return(str_split(aliases$`Patient ID`[1], ", ")[[1]])
}

## gs4_deauth()
## gs4_auth()
## Load data
## Remove, rename, and clean columns
## Remove entries with "xx" in the ID column
## Prevent case sensitivity in patient IDs by changing all IDs to lowercase
fetch_tech_raw <- function() {
  tech_raw_old_1 <- read_sheet(Sys.getenv("TECH_OLD"), sheet="Historic TMS Data Pt1")

  tech_raw_old_2 <- read_sheet(Sys.getenv("TECH_OLD"), sheet="Historic TMS Data Pt2")
  
  tech_raw_current <- read_sheet(Sys.getenv("TECH_NEW"), sheet="Query")
  
  tech_raw <- rbind(tech_raw_old_1, tech_raw_old_2, tech_raw_current) %>%
    rename(Date = 'What is the date of the treatment?') %>%
    unite(pt_id, c('What is the four letter patient ID? (First two letters of FIRST and LAST name)', 'What are the last two digits of the patient\'s cell phone number?'), sep="", remove=TRUE)
  
  tech_raw
}

fetch_survey_raw_tms <- function() {
  survey_raw_tms_old <- read_sheet(Sys.getenv("SURVEY_OLD"), sheet="Form Responses 1") %>%
    filter(!is.na(`Over the last day, have you felt down/ depressed / sad? [1 of 7]`)) %>%
    rename(pt_id = 'ID') %>%
    mutate(Date = as.Date(`Start time`),
           pt_id = tolower(pt_id)) %>%
    select(c('pt_id',
             'Date',
             'Over the last day, have you felt down/ depressed / sad? [1 of 7]',
             'Over the last day, have you felt anxious / on edge / panicked? [2 of 7]',
             'Over the last day, have you felt irritable / angry? [3 of 7]',
             'Over the last day, have you felt happy /euphoric? [4 of 7]',
             'Over the last day, have you felt peaceful / mindful? [5 of 7]',
             'Over the last day, have you felt hopeful / optimistic? [6 of 7]',
             'Over the last day, have you felt suicidal? [7 of 7]',
             'How many hours did you sleep last night?',
             'How was your sleep quality?',
             'Have you eaten at least one meal (>400 Calories) since you slept last night?',
             'How much caffeine did you consume over the last day? (A cup of coffee has ~100mg, black tea ~50mg, green tea ~30mg)',
             'How much alcohol did you consume over the last 24 hours? (1 drink = 1 beer, 1 glass of wine, or 1 shot of liquor)',
             'What drugs (e.g. marijuana) did you use in the last 24 hours and how much did you use? Write N/A if none.',
             'What over-the-counter medication did you use in the last 24 hours and how much did you use? Leave blank if none.',
             'Have you experienced any side effects from treatment in the last day? If so, please describe. Leave blank if none.',
             'Over the last day, did you have any thoughts about hurting yourself?'))
  
  survey_raw_tms_current <- read_sheet(Sys.getenv("SURVEY_CURRENT"), sheet="Query") %>%
    filter(!is.na(`Over the last day, have you felt down/ depressed / sad? [1 of 7]`)) %>%
    rename(pt_id = 'ID') %>%
      mutate(Date = as.Date(Timestamp),
             pt_id = tolower(pt_id)) %>%
    select(c('pt_id',
             'Date',
             'Over the last day, have you felt down/ depressed / sad? [1 of 7]',
             'Over the last day, have you felt anxious / on edge / panicked? [2 of 7]',
             'Over the last day, have you felt irritable / angry? [3 of 7]',
             'Over the last day, have you felt happy /euphoric? [4 of 7]',
             'Over the last day, have you felt peaceful / mindful? [5 of 7]',
             'Over the last day, have you felt hopeful / optimistic? [6 of 7]',
             'Over the last day, have you felt suicidal? [7 of 7]',
             'How many hours did you sleep last night?',
             'How was your sleep quality?',
             'Have you eaten at least one meal (>400 Calories) since you slept last night?',
             'How much caffeine did you consume over the last day? (A cup of coffee has ~100mg, black tea ~50mg, green tea ~30mg)',
             'How much alcohol did you consume over the last 24 hours? (1 drink = 1 beer, 1 glass of wine, or 1 shot of liquor)',
             'What drugs (e.g. marijuana) did you use in the last 24 hours and how much did you use? Write N/A if none.',
             'What over-the-counter medication did you use in the last 24 hours and how much did you use? Leave blank if none.',
             'Have you experienced any side effects from treatment in the last day? If so, please describe. Leave blank if none.',
             'Over the last day, did you have any thoughts about hurting yourself?'))
  
  survey_raw_tms <- rbind(survey_raw_tms_old, survey_raw_tms_current)
  survey_raw_tms
}
fetch_survey_raw_results <- function() {
  survey_raw_results_old <- read_sheet(Sys.getenv("SURVEY_OLD"), sheet="Results") %>%
    select(c('Timestamp',
             'Patient Code',
             'QIDS',
             'BDI',
             'BAI',
             'PHQ9',
             'GAD7',
             'MADRS-SR',
             'Harvard Flourishing Scale')) %>%
    rename(pt_id = 'Patient Code') %>%
    mutate(Date = as.Date(`Timestamp`),
           pt_id = tolower(pt_id))
  
  survey_raw_results_current <- read_sheet(Sys.getenv("SURVEY_CURRENT"), sheet="Main Sheet") %>%
    select(c('Timestamp',
             'Patient Code',
             'QIDS',
             'BDI',
             'BAI',
             'PHQ9',
             'GAD7',
             'MADRS-SR',
             'Harvard Flourishing Scale')) %>%
    rename(pt_id = 'Patient Code') %>%
    mutate(Date = as.Date(`Timestamp`),
           pt_id = tolower(pt_id))
  
  survey_raw_results <- rbind(survey_raw_results_old, survey_raw_results_current) %>%
    rename(MADRS = 'MADRS-SR',
           HFS = 'Harvard Flourishing Scale') %>%
    filter(!(is.na(QIDS) & 
             is.na(BDI) & 
             is.na(BAI) & 
             is.na(PHQ9) & 
             is.na(GAD7) &
             is.na(MADRS) &
             is.na(HFS)))
  
  survey_raw_results
}

tech_raw <- fetch_tech_raw()
survey_raw_tms <- fetch_survey_raw_tms()
survey_raw_results <- fetch_survey_raw_results()
demographics <- read_sheet(Sys.getenv("DEMOGRAPHICS"), sheet="Patient Demographics")

for (patient in list_of_patients) {
  patient_ids <- fetch_patient_id_aliases(demographics, patient)
  tech_filtered <- tech_raw %>% filter(tolower(pt_id) %in% tolower(patient_ids),
                                       Date >= as.Date(date_constraints[1], "%m/%d/%y"),
                                       as.Date(date_constraints[2], "%m/%d/%y") >= Date)
  survey_results_filtered <- survey_raw_results %>% filter(tolower(pt_id) %in% tolower(patient_ids),
                                       Timestamp >= as.Date(date_constraints[1], "%m/%d/%y"),
                                       as.Date(date_constraints[2], "%m/%d/%y") >= Timestamp)
  survey_tms_filtered <- survey_raw_tms %>% filter(tolower(pt_id) %in% tolower(patient_ids),
                                       Date >= as.Date(date_constraints[1], "%m/%d/%y"),
                                       as.Date(date_constraints[2], "%m/%d/%y") >= Date)
    if (nrow(tech_filtered) > 0) {
        print(sprintf("Output sessions for %s", patient))
        generate_output(tech_filtered, survey_tms_filtered, survey_results_filtered, patient)
    } else {
      cat(sprintf("Patient %s has no technician data. Check formatting?\n", patient),
          file=error_file, fill=FALSE, append=TRUE)
    }
}
cat("End of error log.", file=error_file, fill=FALSE, append=TRUE)
