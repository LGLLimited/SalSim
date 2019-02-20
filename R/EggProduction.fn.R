EggProduction <- function(run.dir, river) {
  
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
  s <- which(str_detect(raw$V1, "^Summary Report of Fish Counts"))
  
  # Determine rows with actual data in the manager's summary report section
  # this is hard coded as each row indicates a differnt output summary
  # an the number of output summaries is static.  Columns indicate the number 
  # of years which can change.
  i <- c(s+6, seq(s+8, s+8 + 191))
  
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
  
  j <- which(
    header['A', ] == "Eggs" &
      header['C', ] ==  river &
      header['D', ] %in% c("Reach 1", "Reach 2", "Reach 3", "Reach 4", "Total Eggs", "Total females", "Eggs/Female")
  )
  
  result <- data[ ,c(1, j)]
  colnames(result)[-1] <- c("Reach1", "Reach2", "Reach3", "Reach4", "TotalEggs", "TotalFemales", "EggsPerFemale")
  return(result)
  
}