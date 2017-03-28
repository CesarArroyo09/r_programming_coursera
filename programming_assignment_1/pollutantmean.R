pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## Calculate the mean concentration of a given pollutant
  ## in monitors with codes in id.
  ##
  ## Args:
  ##   directory: Directory where the data is stored
  ##   pollutant: Pollutant from which to calculate mean.
  ##              Can be either "sulfate" or "nitrate"
  ##   id: A vector containing the id's of the monitors
  ##       from which the mean is going to be calculated.
  ##
  ## Returns:
  ##   The mean concentration of pollutant in monitors with codes in id.
  
  ## Setting working directory
  old.wd <- getwd()
  setwd(directory)
  
  ## Initializing values
  sum <- 0  # Carries the sum of the all data
  observations <- 0  ## Carries the number of observations
  
  ## Looping through the monitors data.
  for(id in id) {
    if(id < 10) {
      archive.name <- paste("0", "0", id, ".csv", sep = "")
      id.data      <- read_csv(archive.name, col_types = "Dddi")
      sum          <- sum + sum(id.data[, pollutant], na.rm = TRUE)
      observations <- observations + sum(!is.na(id.data[, pollutant]))
    } else if (id >= 10 & id < 100) {
      archive.name <- paste("0", id, ".csv", sep = "")
      id.data      <- read_csv(archive.name, col_types = "Dddi")
      sum          <- sum + sum(id.data[, pollutant], na.rm = TRUE)
      observations <- observations + sum(!is.na(id.data[, pollutant]))
    } else {
      archive.name <- paste(id, ".csv", sep = "")
      id.data      <- read_csv(archive.name, col_types = "Dddi")
      sum          <- sum + sum(id.data[, pollutant], na.rm = TRUE)
      observations <- observations + sum(!is.na(id.data[, pollutant]))
    }
  }
  
  setwd(old.wd)  # Return to old working directory
  mean <- sum/observations
  round(mean, 3) # Mean rounded to three decimal places
}