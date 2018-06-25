complete <- function(directory, id = 1:332) {
        inputdata <- readdata(directory, asfilename(id))
        completecases <- complete.cases(inputdata)
        complete <- inputdata[completecases, ]

        outputdata <- NULL
        for(i in id) {
                outputdata <- rbind(outputdata, c(id = i, nobs = nrow(complete[complete$ID == i, ])))
        }

        data.frame(outputdata)
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

        print(complete("specdata", 1))
        print("##   id nobs")
        print("## 1  1  117")

        print(complete("specdata", c(2, 4, 8, 10, 12)))
        print("##   id nobs")
        print("## 1  2 1041")
        print("## 2  4  474")
        print("## 3  8  192")
        print("## 4 10  148")
        print("## 5 12   96")

        print(complete("specdata", 30:25))
        print("##   id nobs")
        print("## 1 30  932")
        print("## 2 29  711")
        print("## 3 28  475")
        print("## 4 27  338")
        print("## 5 26  586")
        print("## 6 25  463")

        print(complete("specdata", 3))
        print("##   id nobs")
        print("## 1  3  243")
}
