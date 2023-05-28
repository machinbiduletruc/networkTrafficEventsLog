#Packages required for the project
required_packages <- c('rstudioapi', 'fs', 'stringr')
#Installation of all required packages not already installed
install.packages(setdiff(required_packages, rownames(installed.packages())))
#Loading required packages
library('rstudioapi', 'fs', 'stringr')
#Get this rmd file current absolute path 
#(from: https://stackoverflow.com/questions/47044068/get-the-path-of-current-script)
if (rstudioapi::isAvailable()) {
    wdir <- dirname(getActiveDocumentContext()$path)
}else{
    wdir <- getwd()
}
wdir <- stringr::str_trim(paste(head(strsplit(wdir, "/")[[1]], -1), collapse = "/"))
if (fs::dir_exists(wdir)){
    setwd(wdir)
    codedir <- file.path(wdir, "code", fsep = .Platform$file.sep)
    datadir <- file.path(wdir, "data")
    outputdir <- file.path(wdir, "output")
    features <- file.path(datadir, "features.csv")
    tryCatch(expr = {source(file.path(codedir, "myutils.R"))}
             , error = function(e){print(e)}
             , warning = function(w){print(w)})
    debugprint(c('Working dir is:', wdir))
    if (fs::dir_exists(codedir)==FALSE){ 
        print(paste('[PROBLEM] Unfound directory:', codedir))
    }else if(fs::dir_exists(datadir)==FALSE){
        print(paste('[PROBLEM] Unfound directory:', datadir))
    }else if(fs::dir_exists(outputdir)==FALSE){
        print(paste('[PROBLEM] Unfound directory:', outputdir))
    }else if(fs::file_exists(features)==TRUE){
        features <- read.csv(features)
        nfeatures <- nrow(features)
        features <- features[,2:4]
        colnames(features) <- c('Field', 'Type', 'Description')
        features$Field <- apply(features[, 1, drop = FALSE], 2, function(val){toupper(val)})
        features$Type <- apply(features[, 2, drop = FALSE], 2, function(val){tolower(val)})
    }
} else{
    print(paste('[PROBLEM] Unfound directory:', wdir))
}

eventscsv <- file.path(datadir, "events_sample.csv") # File name needs adapting here to use sample or whole csv
if(fs::file_exists(eventscsv)==TRUE){
    df.events <- read.csv(eventscsv)
    ncols <- ncol(df.events)
    nrows <- nrow(df.events)
    colnames <- toupper(names(df.events))
    cat(paste('The file contains', ncols, 'columns and', nrows, 'rows.\n'), fill=TRUE)
    if(nfeatures == 49){ 
        features = data.frame(features, Insight='') # Adding a column to keep the str() output
        for(i in 1:nfeatures) {
            features$Insight[i] <- capture.output(str(df.events[ , i]))
        }
    }else{
        print(str(df.events))
        print(summary(df.events))
    }
}else{
    print(paste('[PROBLEM] Unfound file:', datadir))
}
kable(features)

for(i in 1:ncols) {
    col <- df.events[ , i]
    cat(paste('\n----------------------------\nDetails of variable', i, '[', trimstr(colnames[i]), ']:\n'), fill = TRUE)
    cat(paste('   Data type:', features[i, 3]), fill = TRUE)
    cat(paste('   Field description:', features[i, 4], '\n'), fill = TRUE)
    datatype <- class(col)
    colsummary <- summary(col) 
    nmissingvals <- sum(!is.finite(col)) # 'is.finite()' function is used to identify non-NA and non-NaN values
    cat(paste(datatype, str(col), '\n'), fill = TRUE)
    if (datatype == "character" || features[i, 3] == 'timestamp' || features[i, 3] == 'binary' 
        || features[i, 3] == 'nominal'){
        values <- levels(factor(col)) # Getting the values
        values.dis <- data.frame(table(col)) # Getting values distribution
        values.dis <- values.dis[order(values.dis$Freq, decreasing=TRUE),] # Reordering df based on Frequency DESC
        colnames(values.dis)[1] <- "Values"
        if(nrow(values.dis)>=30){
            print(values.dis[1:30,], row.names = FALSE)
            cat("...", fill = TRUE)
        }else{
            print(values.dis, row.names = FALSE)
        }
    }else{
        print(colsummary)
        boxplot(col, na.rm = TRUE)
    }
    cat(paste('\nNumber of Missing values:', nmissingvals, '\n'))
}


