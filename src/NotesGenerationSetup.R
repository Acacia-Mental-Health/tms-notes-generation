## Fill in your Acacia Google email within the quotes:
options(gargle_oauth_email = Sys.getenv("GOOGLE_EMAIL"))

date_constraints <- c("7/1/22", "7/28/22") # Date Range, Inclusive; Formatted as "MM/DD/YY"

list_of_patients <- list(
  # A `list` of patient codes. This list references the `Patient Demographics`
  # sheet on google drive, so you only need to enter one of any of the aliases,
  # and the program will find the other relevant entries.
  "AaBb11",
  "CcDd22"
)
