#' `fxnTableTitle.R` Build title for HTML table based on user input
#' 
#' @param annualCrop - Annual crop selected by user
#' @param azmetStation - AZMet station selected by user
#' @return `tableTitle` - Title for HTML table based on user input


fxnTableTitle <- function(azmetStation, annualCrop) {
  tableTitle <- 
    htmltools::h4(
      htmltools::HTML(
        paste(
          "Water Use Estimates for", stringr::str_to_title(annualCrop), "at the AZMet", azmetStation, "Station",
          sep = " "
        )
      ), 
      
      class = "table-title"
    )
  
  return(tableTitle)
}
