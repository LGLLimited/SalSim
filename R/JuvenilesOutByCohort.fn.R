#' Juvenile Cohort Summaires at Exit
#'
#' @param run.dir path to SJRSim run output directory. 
#' @param river optional names of birth rivers to return, if not specified all rivers returned. 
#'
#' @return
#' @export
#'
#' @examples
JuvenilesOutByCohort <- function(run.dir, river=NULL) {
  
  
  if (!require(tidyverse)) stop("The 'tidyverse' package is required.")
  
  # Determine file path to the approiate data type
  file <- GetLoadFile(run.dir, type = "cohort")
  
  # Load raw data
  raw <- read.csv(file, skip = 0, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
 
   # Drop any empty columns
  r <- which(sapply(raw, function(x) all(is.na(x))))
  if (length(r) > 0) raw <- raw[ ,-r]
  
  colnames(raw) <- str_replace_all(colnames(raw), "[:space:]", "")
  colnames(raw) <- str_replace_all(colnames(raw), "\\[mm\\]", "")
  colnames(raw) <- str_replace_all(colnames(raw), "-", ".")
  
  if (!is.null(river)) 
    raw <- raw %>% filter(BirthRiver == river)
  
  dat <-  raw %>% 
    rename(Cohort = CohortName) %>%
    mutate(
      River = str_extract(Location, "^[:alpha:]+"),
      Reach = as.numeric(str_replace_all(str_extract(Location, "/[:digit:]+/"), "/", "")),
      RM = as.numeric(str_extract(Location, "[[:digit:].]+$"))
    ) %>% select(
      Cohort, BirthRiver, BroodYear, Date, Origin, 
      InitialCount, CurrentCount, River:RM
    ) 
  
  if (!is.null(river)) {
    dat <- filter(dat, BirthRiver %in% river)
  }
  
  GetRow <- function(river) {
    i <- which(!duplicated(river))[-1] - 1
    if (length(i) == 1) return(i)
    return(NA)
  }
  
 
  # Compute summaries for each brood year cohort
  cohort.out <- dat %>% 
    filter(Origin == "Natural") %>%
    group_by(BirthRiver, Cohort, BroodYear) %>%
    summarise(
      n.River = length(unique(River)),
      i  = GetRow(River),
      ExitDate = Date[i],
      ExitDateJulian = as.numeric(format(as.Date(Date), "%j")),
      InitCount = unique(InitialCount),
      ExitCount = ifelse(!is.na(CurrentCount[i]), CurrentCount[i], 0),
      SJRCount = ifelse(!is.na(CurrentCount[i +1]), CurrentCount[i+1], 0),
      RM = ifelse(ExitCount > 0, RM[i], NA)
    ) %>% 
    mutate(
      ExitDate = ifelse(ExitCount > 0, ExitDate, NA),
      ExitDateJulian = ifelse(ExitCount > 0, ExitDateJulian, NA)
    )  %>%
    select(-c(i, n.River)) %>%
    mutate(Date = as.Date(Date)) %>% ungroup()
    
  return(cohort.out)
}