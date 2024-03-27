# Load auxiliary files
cropCoefficientCurves <- vroom::vroom(
  file = "aux-files/crop-coefficient-curves.csv", 
  delim = ",", 
  col_names = TRUE, 
  show_col_types = FALSE
)

cropGrowingSeasonLengths <- vroom::vroom(
  file = "aux-files/crop-growing-season-lengths.csv", 
  delim = ",", 
  col_names = TRUE, 
  show_col_types = FALSE
)

stationNames <- vroom::vroom(
  file = "aux-files/azmet-stations-api-db.csv", 
  delim = ",", 
  col_names = TRUE, 
  show_col_types = FALSE
)

# Set limit to user date selection
databaseStart <- lubridate::as_date("2021-01-01")
