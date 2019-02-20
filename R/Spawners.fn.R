#' Retreive SJRSim Spawners Output
#'
#' @param file file path to the NAME_Summary.csv
#' @param incl.historic 
#'
#' @return
#' @details 
#' The \emph{[NAME]_Summary.csv} file contains a number of summary report
#' 
#' Summary Report of Fish Counts
#' Summary Report Of Egg Mortality
#' Summary Report Of Fish Mortality by River of Birth or Hatchery Release
#' Manager's Summary Report
#' 
#' @examples
#' @export
Spawners <- function(run.dir, river = NULL, incl.historic = T) {
  
  if(!require(stringr)) stop("The 'stringr' package is required.")
  

 # Checks ------------------------------------------------------------------
  if (!is.null(river)) {
    if (!all(river %in% c("Stanislaus","Tuolumne","Merced","Friant")))
    stop("Unknown river specified") 
  }
  # browser()
  # Determine Load File ---------------------------------------------------
  file <- GetLoadFile(run.dir, type = "summary")
  # browser()
  
  raw <- read.csv(file, skip=0, header=F, stringsAsFactors = F)
  
  # Cleanup -----------------------------------------------------
  # Drop any empty columns or rows
  r <- which(sapply(raw, function(x)all(is.na(x))))
  if (length(r <- which(sapply(raw, function(x)all(is.na(x))))) > 0) raw <- raw[ ,-r]
  
  
  # Retrieve the header row associated with the Manager Summary Report
  s <- which(str_detect(raw$V1, "^Manager's Summary Report"))
  
  # Determine rows with actual data in the manager's summary report section
  # this is hard coded as each row indicates a differnt output summary
  # an the number of output summaries is static.  Columns indicate the number 
  # of years which can change.
  i <- seq(s+2, s+2 + 31)
  
  # Get the meta data header columns so we can determine what we are looking at.
  # because columns, indicate number of sim years we need to transpose to get it 
  # in a more standard orientation.
  header <- t(raw[i, 1:4]); 
  rownames(header) <- LETTERS[seq_len(nrow(header))]; 
  colnames(header) <- paste0('V', seq_len(ncol(header)))
  
  # Retrieve all data summary data (wide format) which will be filtered for
  # Final return values. Because the data is in wide format, columns indicate 
  # the number of years which can change, so we need to transpose into long
  # format.
  data <- data.frame(apply(t(raw[i, -(1:4)]), 2, as.numeric))
  colnames(data)[1] <- c("BroodYear") 
  colnames(data)[-1] <- paste0("V", seq_len(ncol(data) - 1)) 
 
  # Format one year per row with multiple columns, one for each row in the 
  # original summary file.
  
  # browser()
  if (is.null(river)) {
    # Get coluns associated with total tributary spawners
    j <- intersect(
      which(header['B', ] == "Spawners"),
      which(header['C', ] == "ALL Destinations")
    )
    river <- "All Destinations"
  } else {
    j <- which(
      header['B', ] == "Spawners" &
      header['C', ] == "Natural" &
      header['D', ] == river
    )
  }
  
  
  result <- data[ ,c(1, j)]
  colnames(result)[2] <- "Spawners"
  result$River = river
  
  
  if (incl.historic) {
    i <- c(
      head(which(str_detect(raw$V1, "^Brood Year$")), 1),
      which(str_detect(raw$V1, "^Historical Returns"))
    ) 
    hist <- data.frame(apply(t(raw[i, -(1:4)]), 2, as.numeric))
    colnames(hist) <- c("Year", "Historical Returns")
    
    result <- merge(hist, result, by="Year")
  }
  
  return(result)
}
