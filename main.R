source("reader.R")
source("apportion.R")

# Path to the location of file that contains the total amount of each tax.
kTaxTotalsPath = './static/tax_totals.csv'
# Path to the location of file that contains the statistics of Taiwan household.
kHouseholdsStatisticsPath = './static/households.csv'
# The supported year (of R.O.C.) for microsimulation.
kSupportedYears = c(104, 105)
# The supported apportion hypothesis
kSupportedApportionHypothesis = c("A", "B")

main <- function(year, input.path, apportion.hypothesis = "A") {
    # Check the year is in supported list.
    if (!year %in% kSupportedYears) {
        stop("Year \"", year, "\" is not in supprted list: [",
             paste(kSupportedYears, collapse = ","), "]")
    }
    # Check the apportion hypothesis is in supported list.
    if (!apportion.hypothesis %in% kSupportedApportionHypothesis) {
        stop("Apportion hypothesis \"", apportion.hypothesis,
             "\" is not in supprted list: [",
             paste(kSupportedApportionHypothesis, collapse = ","), "]")
    }

    # Read the income and expenditure of each family.
    families <- ReadFamilyData(input.path)
    # Read the total income amount of each tax.
    tax.totals <- ReadTaxTotals(kTaxTotalsPath, year)
    # Read the total number of households.
    household.totals <- ReadHouseholdTotals(kHouseholdsStatisticsPath, year)

    # Calculate the tax apportion amount of each tax for each family.
    apportion.amounts <- CalculateApportionAmounts(families,
                                                   tax.totals,
                                                   household.totals,
                                                   apportion.hypothesis)
}

if(!interactive()) {
    args = commandArgs(trailingOnly = TRUE)

    if (length(args) < 2) {
        stop("Usage: Rscript main.R year input.path [apportion.hypothesis]")
    }

    year = as.integer(args[1])
    input.path = args[2]
    apportion.hypothesis = NULL

    if (length(args) > 2) {
        apportion.hypothesis = args[3]
        main(year, input.path, apportion.hypothesis)
    } else {
        main(year, input.path)
    }
}
