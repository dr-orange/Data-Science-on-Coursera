pollutantmean <- function(directory, pollutant, id = 1:332) {
        inputdata <- readdata(directory, asfilename(id))
        
        mean(inputdata[[pollutant]], na.rm = TRUE)
}

# read data from file
readdata <- function(directory, filenames) {
        # Initialize data stack
        readdata <- NULL
        
        # Read each csv file
        for(filename in filenames) {
                filepath <- file.path(directory, filename)
                # Read csv file
                readdata <- rbind(readdata, read.csv(filepath))
        }
        
        readdata
}

# csv filename from id
asfilename <- function(id) {
        # 1, 2, 3, ... -> 001.csv, 002.csv, 003.csv, ...
        paste(formatC(id, width = 3, flag = "0"), ".csv", sep = "")
}

# test
mytest <- function() {
        print(R.version.string)

        print(pollutantmean("specdata", "sulfate", 1:10))
        print("## [1] 4.064128")
        
        print(pollutantmean("specdata", "nitrate", 70:72))
        print("## [1] 1.706047")
        
        print(pollutantmean("specdata", "nitrate", 23))
        print("## [1] 1.280833")
}
