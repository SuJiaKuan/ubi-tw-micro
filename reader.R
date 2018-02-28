library(foreign)

CalculateWeights <- function(values) {
    # Calculate weights of a list of values, which the sum of weights will be
    # one.
    #
    # Args:
    #   values: List of values.
    #
    # Returns:
    #   The list of weights.
    dvalues <- as.double(values)
    dvalues[is.na(dvalues)] <- 0.0
    dvalues.sum <- sum(dvalues)
    weights <- dvalues / dvalues.sum

    return(weights)
}

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

    families <- list(
        id = data$id,
        population = list(
            total = data$a8,
            worker = data$a9,
            adult = data$a12,
            elder = data$a19
        ),
        type = data$a18,
        total.income = data$itm500,
        apportion.tax = list(
            individual.income = data$itm610,
            house.land.value = data$itm590
        ),
        apportion.weights = list(
            income = list(
                distributed.factor = CalculateWeights(
                    data$itm190
                    + data$itm240
                    + data$itm330
                    - data$itm540
                ),
                investment = CalculateWeights(data$itm350),
                business.net = CalculateWeights(data$itm290),
                # TODO(JiaKuan Su): Property income may be item 330 or 360.
                property = CalculateWeights(data$itm360),
                employee = CalculateWeights(data$itm190)
            ),
            expenditure = list(
                recurring = CalculateWeights(data$itm600 + data$itm1000),
                consumption = CalculateWeights(data$itm1000),
                transportaion.communication.tools = CalculateWeights(
                    data$itm1111
                    + data$itm1131
                ),
                take.transportaion = CalculateWeights(data$itm1113),
                entertainment.culture.services = CalculateWeights(data$itm1152),
                tobacco = CalculateWeights(data$itm1021),
                alcoholic = CalculateWeights(data$itm1022)
            ),
            savings = CalculateWeights(data$itm400 + data$itm600 + data$itm1000)
        )
    )

    return(families)
}

ReadTaxTotals <- function(path, year) {
    # Read the total income amount of each tax for a given year.
    #
    # Args:
    #   path: The path to the data.
    #   year: The given year.
    #
    # Returns:
    #   Total income amount of each tax.
    data <- read.csv(path)

    # Find the row index for the given year.
    year.idx <- match(year, data[[1]])

    # Help function to get total value of a tax item.
    GetTaxTotal <- function(tax.idx) {
        return(data[[tax.idx]][year.idx])
    }

    tax.totals <- list(
        # 綜合所得稅
        individual.income <- GetTaxTotal(2),
        # 營利事業所得稅
        enterprise.income <- GetTaxTotal(3),
        # 土地增值稅
        land.value.increment  <- GetTaxTotal(4),
        # 營業稅
        sales <- GetTaxTotal(5),
        # 印花稅
        stamp <- GetTaxTotal(6),
        # 使用牌照稅
        vehicle.license <- GetTaxTotal(7),
        # 娛樂稅
        amusement <- GetTaxTotal(8),
        # 房屋稅與地價稅
        house.land.value <- GetTaxTotal(9),
        # 遺產及贈與稅
        estate.gift <- GetTaxTotal(10),
        # 契稅
        deed <- GetTaxTotal(11),
        # 證券交易稅
        securities.exchange <- GetTaxTotal(12),
        # 菸酒稅
        alcohol.tobacco <- GetTaxTotal(13),
        # 貨物稅
        commodity <- GetTaxTotal(14),
        # 關稅
        tariff <- GetTaxTotal(15)
    )

    return(tax.totals)
}

ReadHouseholdTotals <- function(path, year) {
    # Read the total number of households for a given year.
    #
    # Args:
    #   path: The path to the data.
    #   year: The given year.
    #
    # Returns:
    #   Total number of households.
    data <- read.csv(path)

    # Find the row index for the given year.
    year.idx <- match(year, data[[1]])

    household.totals <- data[[2]][year.idx]

    return(household.totals)
}
