library(foreign)

ReadFamilyData <- function(path) {
    # Read the survey data of family income and expenditure from a file.
    # Currently, the function only supports ".sav" (SPSS) and ".dta" (Stata)
    # file formats.
    #
    # Args:
    #   path: The path to the data.
    #
    # Returns:
    #   The income and expenditure for each family.
    if (endsWith(path, ".sav")) {
        data <- read.spss(path)
    } else if(endsWith(path, ".dta")) {
        data <- read.dta(path)
    } else {
        stop("The file format is not supported: \"", path ,"\"")
    }

    return(data)
}
