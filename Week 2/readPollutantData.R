readPollutantData <- function(directory, id = 1:332) {
  ## Reads in the pollutant data set
  
  # The directory string must end in a "/"
  if(substr(directory, nchar(directory), nchar(directory)) != "/") {
    directory <- paste(directory,"/", sep = "")
  }
  
  # Generate filenames
  filenames <- lapply(
    X = id,
    FUN = function(x) paste(directory, sprintf(fmt = "%03d", x),
      ".csv", sep = "")
    )
  
  # List of data.frames
  pollutantData <- lapply(
    X = filenames,
    FUN = function(x) read.csv(x)
    )
  
  # List to data.frame
  pollutantData <- do.call("rbind", pollutantData)
  
  # Change ID to a factor
  pollutantData <- transform(pollutantData, ID = factor(ID))
  
  # Change Date to Date class
  pollutantData <- transform(pollutantData, Date = as.Date(Date))
  
  # Return pollutant data
  return(pollutantData)
}