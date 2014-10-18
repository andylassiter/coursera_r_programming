complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  ## Requires: readPollutantData.R
  
  # Read Data
  pollutantData <- readPollutantData(directory = directory,
                                     id = id)
  
  # Get indicies of complete cases
  ok <- complete.cases(pollutantData)
  
  # Generate data frame of the form:
  # ID ok
  # 1  117
  # 2  1041
  # ...
  cc <- aggregate( ok ~ ID , data = pollutantData , FUN = sum )
  
  # Change id to factor and order by user ID input
  cc <- transform(cc, ID = factor(ID, id))
  cc <- with(cc, cc[order(ID),])
  rownames(cc) <- c(1:nrow(cc))
  
  # Change column names to id and nobs
  colnames(cc) <- c("id", "nobs")
  
  # Return complete cases
  return(cc)
  
}