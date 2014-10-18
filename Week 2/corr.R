corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  # Get complete cases
  completeCases <- complete(directory = directory)
  
  # Get IDs greater than threshold
  validIDs <- as.vector(
    as.integer(
      completeCases$id[completeCases$nobs > threshold]
      )
    )
  
  if(length(validIDs) == 0) {
    return(vector(mode = "integer"))
  }
  
  # Get data for valid ids
  pollutantData <- readPollutantData(directory = directory,
                                     id = validIDs)
  
  # Get correlation between sulfate and nitrate for each ID
  corData <- by(data = pollutantData,       # Data
                INDICES = pollutantData$ID, # Grouping
                FUN = function(x) cor(x$sulfate, x$nitrate, use = "complete.obs"))
  
  # Cooerce into a vector
  corData <- as.vector(corData)
  
  # Return correlation data
  return(corData)
}