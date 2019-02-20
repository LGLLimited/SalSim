#' Write SalSim/SJRSim HEC file
#'
#' @param flowtemp  - flowtemp data should be in wide format.  Column names are those returen from readHEC
#' @param river  - character string indicating the river name, options include: TUOLUMNE', 'STANISLAUS', 'SAN JOAQUIN', 'MERCED', 'FRIANT'
#' @param filename  - File name to be written to.
#'
#' @return No values returned, rather a system file is written.
#'
#' @examples
#' 
#' @export
WriteSalSimHEC <- function(flowtemp, header=NULL, river=NULL, filename) {
  
  if (is.null(header) & is.null(river)) stop("Arguments required for either header or river")
  
  if (is.null(header)) {
    
    if (toupper(river) %in% c('TUOLUMNE', 'STANISLAUS', 'SAN JOAQUIN', 'MERCED', 'FRIANT')) {
      stop("River not supported")
    }
    header <- generateHeader(river)
    RM.head <- as.numeric(header['B',-1])
  }
  # Check Dimensionality ----------------------------------------------------
  # Need to ensure the number of fields is the same.
  if (ncol(header) != ncol(flowtemp)) {
    message('WARNING: Dimensionality issue found, expected number of fields differ.')
  } 
  
  # Ensure the river mile designations match
  
  # If a header is provided we will do our best to extract the 
  # river mile designations to ensure they match the improed flow/temp data.
  if (!is.null(header)) {
    RM.head <- str_extract(header['B',-1], '[[:digit:]]*[\\.]?[[:digit:]]*$')
    RM.head <- as.numeric(RM.head)
    
    # Standardize the measurement type designations (_AVG vs _AVE)
    header['C',-1] <- str_replace_all(header['C',-1], pattern = '[-_]', replacement = '_')
    header['C',-1] <- str_replace_all(header['C',-1], pattern = '_AVG', replacement = '_AVE')
  }
  
  RM.data <- str_extract_all(colnames(flowtemp)[-1], 'RM[[:digit:]]*[\\.]?[[:digit:]]*', simplify=TRUE)
  RM.data <- as.numeric(str_replace_all(RM.data, '^RM',''))
  
  if (any(length(d1 <- setdiff(RM.head, RM.data)) > 0) | any(length(d2 <- setdiff( RM.data, RM.head)) > 0) ) {
    # Check if any expected River Mile designations (based on HEC header) are missing
    if (length(d1) > 0) {
      message("Flow/temp data missing the following River Mile points: ", paste(d1, collapse=", "))
    }
    # Check if data has any extra expected River Mile designations, relative to the HEC header
    if (length(d2) > 0) {
      message("Flow/temp has extra River Mile points: ", paste(RM.miss, collapse=", "))
    }
     # browser()
     # print(rbind('header'=header[c('B', 'C'),i], 'flowtemp'=colnames(flowtemp)[i]))
    message("River mile mismatch, file not written")
    return()
    # stop("Rive mile mismatch")
  }
  #--- Check Measurement Type
  # type.head <- toupper(c('DATE', str_replace_all(header['C',-1],pattern = '[-_]','_')))
  type.head <- toupper(c('DATE', header['C',-1]))
  type.data <- toupper(c(colnames(flowtemp)[1], str_replace_all(str_extract(colnames(flowtemp)[-1], "^[[:alpha:]-_]+"),"[-_]", '_')))
  
  # NOTE: there is some inconsistencies in abbreviation used to indicate an average value (AVE and AVG)
  # we will use the SALSIM spelling of AVE since this data is destined for the SALSIM simulator.
  type.data <- str_replace_all(type.data, "_AVG$", "_AVE")
  
  check <- NULL
  if (any(check <- toupper(type.head) != toupper(type.data))) {
    i <- which(check) 
    message("Mismatch in field measurement type:")
    
    rbind(header.RM = RM.head[i], header.type = type.head[i], data.RM = RM.data[i], data.type = type.data[i]  )
    # print(rbind('header'=header[c('B', 'C'),i], 'flowtemp'=colnames(flowtemp[i])))
    browser()
    message("Error in field type ordering, file not written!")
    return()
    # stop("Error in field type ordering")
  }
  
  # Covert Date to SalSim format
  if (class(flowtemp[[1]]) != 'Date') stop ('First column in flowtemp should be of type "Date"')
  # browser()
  # x <- lapply( flowtemp[ ,-1], formatC,digits = 1, format = "f")
  flowtemp2 <- data.frame(
    Date = paste(
      str_replace(format(flowtemp[[1]], "%m"), "^0", ""),
      str_replace(format(flowtemp[[1]], "%d"), "^0", ""),
      format(flowtemp[[1]], "%Y"),
      sep="/"
    ),
    # flowtemp[ ,-1],
    lapply( flowtemp[ ,-1], formatC,digits = 2, format = "f"),
    stringsAsFactors = F
  )
  
  
  # Else write the file in two steps
  # write header
  write.table(header,  file = filename, append = FALSE, quote = FALSE, sep=",", eol = "\r\n", row.names=TRUE, col.names = FALSE)
  # write data
  write.table(flowtemp2, file = filename, append = TRUE,  quote = FALSE, sep=",", eol = "\r\n", row.names=TRUE, col.names = FALSE)
}
