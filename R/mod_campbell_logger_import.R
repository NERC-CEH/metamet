# I found the function here: https://www.campbellsci.com/blog/tool-to-import-data-to-r
# function to correctly load Campbell data logger data (TOA5 format)

importCSdata <- function(filename, RetOpt="data") {
  if(RetOpt == "info") {
    # bring in entire header of CSI TOA5 data file for metadata
    stn.info <- scan(file=filename, nlines=4, what=character(), sep="\r")
    return(stn.info)
  } else {
    # second line of header contains variable names
    header <- scan(file=filename, skip=1, nlines=1, what=character(), sep=",")
    # bring in data
    stn.data <- read.table(file=filename, skip=4, header=FALSE, na.strings=c("NAN"), sep=",")
    names(stn.data) <- header
    stn.data <- stn.data %>%
      mutate(TIMESTAMP = format(as.POSIXct(TIMESTAMP, format="%Y-%m-%d %H:%M:%S"), "%Y-%m-%d %H:%M:%S"))
    return(stn.data)
  }
}

# checking how it's being imported
#test <- importCSdata("./ignore/CLIMOOR_30Minute_2024_04_09_0330.dat")
