#' Read HEC flow/temp file
#'
#' @param file 
#' @param wide.format 
#' @param date.format 
#' @param debug.mode 
#'
#' @return
#' @export
#'
#' @examples
ReadHEC <- function(file, wide.format = FALSE, date.format='%m/%d/%Y', debug.mode=FALSE) {
  
  # require(stringr)
  
  # IMPORT  Header --------------------------------------------------------
  header <- read.csv(file, header=F, stringsAsFactors = F)[1:7, ]
  rownames(header) <- header[,1]
  header <- header[, -1]
  colnames(header) <- paste('V', seq_len(ncol(header)), sep='')
  
  # River Mile Designations -----------------------------------------------
  # Retrieve and clean up river mile designations from the file header.
  # There is some variation in how this is specifed in the various files
  # for example RM 18 has been designated as "MILE_18.000 or "18"
  RM <- as.numeric(str_extract_all(header['B',-1], '[[:digit:]]+[\\.]?[[:digit:]]*', simplify=TRUE))
  
  # Conversion Check. 
  # The first position should be NA as this field is associated with the date.
  if (any(is.na(RM))) {
    message("River mile conversion fail - launching browser mode"); browser()
  }
  
  # IMPORT FLOW/TEMP Data -------------------------------------------------
  # Drop the first column as it is a redundant row numbering
  data <- read.csv(file, skip=6, check.names = F, stringsAsFactors = F)[ ,-1] 
  colnames(data) <- c("Date", paste(str_replace_all(header['C', -1], "[-_.]", "_"), paste('RM',  RM, sep=''), sep='.'))
  
  check <- sapply(data[,-1], is.numeric)
  if (any(check == FALSE)) {
    message("WARNING: Numeric check failed for '", basename(file), "' the following data fields: ", paste(names(check)[which(check==FALSE)], collapse = ', '))
    warning("Numeric check failed for '", basename(file), "' the following data fields: ", paste(names(check)[which(check==FALSE)], collapse = ', '))
    if (debug.mode) browser()
  }
  
  # Date Conversion -------------------------------------------------------
  # Coverte date filed to a date obj
  data <- within(data, Date <- as.Date(Date, date.format))
  if (any(is.na(data$Date))) {
    message("WARNING: Date conversion failed")
    warning("Date conversion failed")
    if (debug.mode) browser()
  }
  
  if (any(is.na(data))){
    message("WARNING: Missing value found.")
    warning("Missing value found.")
    if (debug.mode) browser()
  }
  
  # Return wide format if requested
  if (!is.logical(wide.format)) stop("wide.format arugment needs to be a logical")
  if (wide.format) { 
    return(list(header=header,flowtemp=data)) 
  }
  
  # ELSE we will return the data in long format which is useful for plotting 
  
  # Create long data format from wide dat format
  # RM <- unique(as.numeric(header['B', -1]))
  
 
  type <- str_replace_all(unique(as.character(header['C', -1])), "[-_.]", "_")
  # browser()
  data.long <- NULL
  for (t in type) {
    for (rm in RM) {
      message('importing ', t, ' river mile ', rm)
      x <- subset(data, select=c('Date', paste(t, paste('RM', rm, sep=''), sep='.')))
      x <- data.frame(
        x,
        RiverMile = as.numeric(rm),
        Type = as.character(t),
        stringsAsFactors=FALSE
      )
      colnames(x)[2] <- 'Value'
      x <- subset(x, select=c(Date, RiverMile, Type, Value))
      data.long <- rbind(data.long, x)
      
    }
  }
  # return data in long format
  return(data.long)
}



