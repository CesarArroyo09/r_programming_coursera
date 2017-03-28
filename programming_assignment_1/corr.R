corr <- function(directory, threshold = 0) {
  ## This function computes a vector containing the correlations between
  ## the values of nitrate and sulfate for monitors which contain at least 
  ## a number of complete cases as indicated by threshold.
  ##
  ## Args:
  ##   directory: character vector of length 1 indicating
  ##              the location of the CSV files.
  ##   threshold: minimum of complete cases a monitor should have
  ##              in order to calculate the correlation between
  ##              the sulfate and nitrate values.
  ##
  ## Returns:
  ##   A numeric vector of correlations
  
  ## Vector containing number of complete cases
  ## Index of this vector indicates the monitor
  complete.data <- complete(directory)$nobs
  
  ## Setting working directory
  old.wd <- getwd()
  setwd(directory)
  
  ## Initialization of vector of correlations
  correl <- numeric(0)
  
  ## Looping through the monitors
  for(id in 1:332) {
    if(complete.data[id] > threshold) {
      if(id < 10) {
        archive.name <- paste("0", "0", id, ".csv", sep = "")
      } else if (id >= 10 & id < 100) {
        archive.name <- paste("0", id, ".csv", sep = "")
      } else {
        archive.name <- paste(id, ".csv", sep = "")
      }
      id.data      <- read_csv(archive.name, col_types = "Dddi")
      c            <- cor(id.data$sulfate, id.data$nitrate,
                          use = "complete.obs")
      correl       <- append(correl, c)
    }
  }
  
  setwd(old.wd) # Return to old working directory
  
  correl # Return correlation vectors
}