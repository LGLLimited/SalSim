#' Get Load File
#'
#' @param file path to run directory containing SJRSim simulation run output
#' @param type type of output to retrieve (i.e., summary, cohort, or nest)
#'
#' @return
#'
#' @examples
#' 
#' @export
GetLoadFile <- function(dir.path, type="summary") {
  # Check that whether the summary file was directly specified or a path
  # to the output from a SJFSim Run path 
  # browser()
  if (file_test("-f", dir.path) == FALSE) {
    if (dir.exists(dir.path)) {
      run.dir <- dir.path
      run.files <- list.files(run.dir)
      if (tolower(type) == "summary") {
       load.file <- run.files[which(str_detect(run.files, "Summary.csv$"))]
      } else if (type == "cohort") {
        load.file <- run.files[which(str_detect(run.files, "HistoryCohorts.csv$"))]
      } else if (type == "nest") {
        load.file <- run.files[which(str_detect(run.files, "HistoryNests.csv$"))]
      }
      if (length(load.file) == 1) {
        file <- file.path(run.dir, load.file)
      } else{
        stop("Directory contains multiple Summary file matches.")
      }
    }
  } else {
    # the dir path is actually a file
    file <- dir.path
  }
  return(file)
}