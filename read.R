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
    #   List of family data that contains the income and expenditure of each
    #   family.
    if (endsWith(path, ".sav")) {
        data <- read.spss(path)
    } else if(endsWith(path, ".dta")) {
        data <- read.dta(path)
    } else {
        stop("The file format is not supported: \"", path ,"\"")
    }

    families <- list()
    for (idx in 1:length(data$id)) {
        data$Get <- function(key) {
            return(data[[key]][idx])
        }
        data$GetItem <- function(item.num) {
            key <- paste("itm", item.num, sep="")
            value <- data$Get(key)
            value <- if (is.na(value)) 0 else value

            return(value)
        }

        id <- data$Get("id")
        population <- list(
            total = data$Get("a8"),
            worker = data$Get("a9"),
            adult = data$Get("a12"),
            elder = data$Get("a19")
        )
        type <- data$Get("a18")
        total.income <- data$GetItem("500")
        apportion.base <- list(
            tax = list(
                individual.income = data$GetItem("610"),
                house.land.value = data$GetItem("590")
            ),
            income = list(
                distributed.factor = data$GetItem("190")
                                     + data$GetItem("240")
                                     + data$GetItem("330")
                                     - data$GetItem("540"),
                investment = data$GetItem("350"),
                business.net = data$GetItem("290"),
                # TODO(JiaKuan Su): Property income may be item 330 or 360.
                property = data$GetItem("360"),
                employee = data$GetItem("190")
            ),
            expenditure = list(
                recurring = data$GetItem("600")
                            + data$GetItem("1000"),
                consumption = data$GetItem("1000"),
                transportaion.communication.tools = data$GetItem("1111")
                                                    + data$GetItem("1131"),
                take.transportaion = data$GetItem("1113"),
                entertainment.culture.services = data$GetItem("1152"),
                tobacco = data$GetItem("1021"),
                alcoholic = data$GetItem("1022")
            ),
            savings = data$GetItem("400")
                      - data$GetItem("600")
                      - data$GetItem("1000")
        )

        families[[idx]] <- list(
            id = id,
            population = population,
            type = type,
            total.income = total.income,
            apportion.base = apportion.base
        )
    }

    return(families)
}
