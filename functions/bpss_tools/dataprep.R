dataprep = function(dfData, 
                    vars, 
                    logseries = log_trans,
                    differences = NULL,
                    startdate = startdate_calibration,
                    enddate = enddate_calibration, 
                    timedummy = NULL,
                    frequency = NULL, 
                    Tsigbrk = NULL){
    
    ## TI - modified 9.14.2022

    ## A simple function to prepare the data
    ## Takes logarithms and trims the data to desired start and end points (with string inputs)
    ## Also converts string inputs of break points into the correct numerical indices

    ## Karthik Sastry
    ## R 3.0.2, 64 Bit
    ## June 2014


    ## END Preamble

########################################



    ##
    ## Extracting relevant variables
    ##

    Y = dfData[,vars]

    ##
    ## Finding start point
    ##

    if (!is.null(startdate)) {
	startdate = which(dfData[,1] == startdate)
	##need this to be 1d object else error
    } else { ##find first Year in which all data are available
	missingData = is.na(Y)
	anyMissing = apply(missingData,1,any)
	allPresent = which(anyMissing == FALSE, arr.ind = TRUE)
	startdate = allPresent[1]
    }

    ##
    ## Finding end point
    ##

    if (!is.null(enddate)) {
	enddate = which(dfData[,1] == enddate)
    } else {
	enddate = dim(Y)[1] ##end at last observation
    }

    ##
    ## Applying start and end points
    ##

    Y = Y[startdate:enddate,]
    dates = dfData[startdate:enddate,1]

    Y = data.matrix(Y) ##data frame class -> matrix class

    ##
    ## Applying logarithms and differences
    ##

    if (sum(logseries) > 0) {
	lcLogSeries = (logseries == 1) ##lc = logical; prefer 0/1 input so conversion necessary
	Y[,lcLogSeries] = log(Y[,lcLogSeries])
    }

    if (sum(differences) > 0) {
	tempY = Y
	for (iColumn in 1:length(differences)) { ##columnwise Kronecker produdct
            tempY[,iColumn] = differences[iColumn] * tempY[,iColumn]
	}
	Y = Y[2:dim(Y)[1],] - tempY[1:(dim(Y)[1] - 1),] 
	##subtracting lag 1 for differenced series, zeros for other series
	dates = dates[2:length(dates)]
	##small issue: will waste a row if: d1 series were available at t-1, but d0 series were not, then t will not be used (because it will initially start at t, then start at t+1 after trying to difference)
    }

    ##
    ## Adding date dummies
    ##
    if (!is.null(timedummy)){
	nDummies = length(timedummy)
	dummyMatrix = matrix(0,length(dates),nDummies)
	for (iDummy in (1:nDummies)) {
            if (grepl(':',timedummy[iDummy])){
                ##dummy for time range
                endpoints = strsplit(timedummy[iDummy],':')
                startPoint = which(dates == endpoints[1], ind = TRUE)
                endPoint = which(dates == endpoints[2], ind = TRUE)
                dummyMatrix[startPoint:endPoint, iDummy] = 1
            } else {
                ##dummy for single period
                dummyMatrix[dates == endpoints, iDummy] = 1
            }
	}
	colnames(dummyMatrix) = timedummy
	X = ts(dummyMatrix)
    } else {
	X = NULL
    }

    ##
    ## Determining date from string
    ##
    if (!is.null(Tsigbrk)) {
	nBreaks = length(Tsigbrk)
	breakInd = rep(NA,nBreaks)
	for (iBreak in (1:nBreaks)){
            breakInd[iBreak] = which(dates == Tsigbrk[iBreak], arr.ind = TRUE)
	}
    } else {
	breakInd = NULL
    }

    ## Will be implemented later

    ##
    ## Converting string break dates to integer indices
    ##


    ##
    ## Creating time series object
    ##

    Y = ts(Y)

    output = list(Y = Y, X = X, Tsigbrk = breakInd)
    return(output)
}
