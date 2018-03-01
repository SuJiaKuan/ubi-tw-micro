library(foreign)

# The number of groups.
kNumGroups = 10

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
    dvalues.sum <- sum(dvalues)
    weights <- dvalues / dvalues.sum

    return(weights)
}

CalculateTopGroupPropertyWeights <- function(total.income, property.income) {
    # Calculate the weights of property income in group who have top total
    # income. The sum of weights will be one.
    #
    # Args:
    #   total.income: List of total incomes.
    #   perperty.income: List of perperty incomes.
    #
    # Returns:
    #   The list of weights of top group.

    # Get the indexse of families excluding the top group.
    skip.num <- as.integer((1 - (1 / kNumGroups)) * length(total.income))
    sorted.total.income.indexes <- sort(total.income, index.return = TRUE)$ix
    skip.indexes <- head(sorted.total.income.indexes, skip.num)

    # Get the property incomes of the top group.
    top.group.property.income <- property.income
    top.group.property.income[skip.indexes] <- 0

    # Calculate the weights of property income in the top group.
    top.group.property.weights <- CalculateWeights(top.group.property.income)

    return(top.group.property.weights)
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

    # Help function to get an item from the data.
    data$GetItem <- function(idx) {
        name <- paste("itm", idx, sep = "")
        item <- data[[name]]
        item[is.na(item)] <- 0

        return(item)
    }

    families <- list(
        samples.size = length(data$id),
        id = data$id,
        population = list(
            total = data$a8,
            worker = data$a9,
            adult = data$a12,
            elder = data$a19
        ),
        type = data$a18,
        total.income = data$GetItem(500),
        tax = list(
            # 綜合所得稅支出
            individual.income = data$GetItem(610),
            # 房屋稅與地價稅支出
            house.land.value = data$GetItem(590)
        ),
        apportion.weights = list(
            income = list(
                # 已分配要素所得
                distributed.factor = CalculateWeights(
                    data$GetItem(190)
                    + data$GetItem(240)
                    + data$GetItem(330)
                    - data$GetItem(540)
                ),
                # 投資收入
                investment = CalculateWeights(data$GetItem(350)),
                # 營業淨收入
                business.net = CalculateWeights(data$GetItem(290)),
                # 投資收入與營業淨收入
                investment.business.net = CalculateWeights(
                    data$GetItem(350)
                    + data$GetItem(290)
                ),
                # 財產收入
                # TODO(JiaKuan Su): Property income may be item 330 or 360.
                property = CalculateWeights(data$GetItem(330)),
                # 最高所得組之財產收入
                # TODO(JiaKuan Su): Property income may be item 330 or 360.
                top.group.property = CalculateTopGroupPropertyWeights(
                    data$GetItem(500),
                    data$GetItem(330)
                ),
                # 受雇人員報酬
                employee = CalculateWeights(data$GetItem(190))
            ),
            expenditure = list(
                # 經常性支出
                recurring = CalculateWeights(
                    data$GetItem(600)
                    + data$GetItem(1000)
                ),
                # 消費支出
                consumption = CalculateWeights(data$GetItem(1000)),
                # 個人交通通訊工具之購置費用
                transportaion.communication.tools = CalculateWeights(
                    data$GetItem(1111)
                    + data$GetItem(1131)
                ),
                # 搭乘交通設備之費用
                take.transportaion = CalculateWeights(data$GetItem(1113)),
                # 娛樂消遣服務支出
                entertainment.culture.services = CalculateWeights(
                    data$GetItem(1152)
                ),
                # 菸草支出
                tobacco = CalculateWeights(data$GetItem(1021)),
                # 酒精支出
                alcohol = CalculateWeights(data$GetItem(1022))
            ),
            savings = CalculateWeights(
                data$GetItem(400)
                - data$GetItem(600)
                - data$GetItem(1000)
            )
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
    #   Total income amount of each tax. The unit of each tax income amount is
    #   million.
    data <- read.csv(path)

    # Find the row index for the given year.
    year.idx <- match(year, data[[1]])

    # Help function to get total value of a tax item.
    GetTaxTotal <- function(tax.idx) {
        return(data[[tax.idx]][year.idx])
    }

    tax.totals <- list(
        # 綜合所得稅
        individual.income = GetTaxTotal(2),
        # 營利事業所得稅
        enterprise.income = GetTaxTotal(3),
        # 土地增值稅
        land.value.increment = GetTaxTotal(4),
        # 營業稅
        sales = GetTaxTotal(5),
        # 印花稅
        stamp = GetTaxTotal(6),
        # 使用牌照稅
        vehicle.license = GetTaxTotal(7),
        # 娛樂稅
        amusement = GetTaxTotal(8),
        # 房屋稅與地價稅
        house.land.value = GetTaxTotal(9),
        # 遺產及贈與稅
        estate.gift =  GetTaxTotal(10),
        # 契稅
        deed = GetTaxTotal(11),
        # 證券交易稅
        securities.exchange = GetTaxTotal(12),
        # 菸酒稅
        alcohol.tobacco = GetTaxTotal(13),
        # 貨物稅
        commodity = GetTaxTotal(14),
        # 關稅
        tariff = GetTaxTotal(15)
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
