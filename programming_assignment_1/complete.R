complete <- function(directory, id = 1:332) {
  ## Creates and returns a data frame containing the number of complete
  ## observations in the data of monitors with ids in id
  ##
  ## Args:
  ##   directory: character vector of length 1 indicating
  ##              the location of the CSV files.
  ##   id: A vector containing the id's of the monitors
  ##       from which the number of observations is going to be computed.
  ##
  ## Returns:
  ##   A data frame containing the number of complete observations in the
  ##    data of monitors with id's in id
  
  ## Setting working directory
  old.wd <- getwd()
  setwd(directory)
  
  ## Initializing empty vectors in which data will be saved
  n <- length(id) # Number of monitors
  nobs <- integer(n)
  
  ## Looping through the monitors data.
  for(i in 1:n) {
    if(id[i] < 10) {
      archive.name <- paste("0", "0", id[i], ".csv", sep = "")
      id.data      <- read_csv(archive.name, col_types = "Dddi")
      nobs[i]      <- sum(complete.cases(id.data))
    } else if (id[i] >= 10 & id[i] < 100) {
      archive.name <- paste("0", id[i], ".csv", sep = "")
      id.data      <- read_csv(archive.name, col_types = "Dddi")
      nobs[i]      <- sum(complete.cases(id.data))
    } else {
      archive.name <- paste(id[i], ".csv", sep = "")
      id.data      <- read_csv(archive.name, col_types = "Dddi")
      nobs[i]      <- sum(complete.cases(id.data))
    }
  }
  
  setwd(old.wd) # Return to old working directory
  
  ## Create data frame with all the data
  data.frame(id, nobs, stringsAsFactors = FALSE)
}