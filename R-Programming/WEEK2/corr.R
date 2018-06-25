corr <- function(directory, threshold = 0) {
        completedata <- complete(directory)
        ids = completedata$id[completedata$nobs > threshold]
        
        corr <- rep(0, 0)
        for(id in ids) {
                inputdata <- readdata(directory, asfilename(id))
                completecases <- complete.cases(inputdata)
                complete <- inputdata[completecases, ]

                corr <- append(corr, cor(complete$sulfate, complete$nitrate))
        }
        
        corr
}

# test
mytest <- function() {
        print(R.version.string)

        source("complete.R")
        cr <- corr("specdata", 150)
        print(head(cr))
        print("## [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814")

        print(summary(cr))
        print("##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. ")
        print("## -0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313")

        cr <- corr("specdata", 400)
        print(head(cr))
        print("## [1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860")

        print(summary(cr))
        print("##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. ")
        print("## -0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313")

        cr <- corr("specdata", 5000)
        print(summary(cr))
        print("##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. ")
        print("## ")

        print(length(cr))
        print("## [1] 0")

        cr <- corr("specdata")
        print(summary(cr))
        print("##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. ")
        print("## -1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000")

        print(length(cr))
        print("## [1] 323")
}
