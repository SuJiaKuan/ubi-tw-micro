source("reader.R")

# Path to the location of file that contains the total amount of each tax.
kTaxTotalsPath = './static/tax_totals.csv'
# Path to the location of file that contains the statistics of Taiwan household.
kHouseholdsStatisticsPath = './static/households.csv'
# The supported year (of R.O.C.) for microsimulation.
kSupportedYears = c(104)

main <- function(year, input.path) {
    if (!year %in% kSupportedYears) {
        stop("Year \"", year, "\" is not in supprted list: [",
             paste(kSupportedYears, collapse = ","), "]")
    }

    families <- ReadFamilyData(input.path)
    tax.totals <- ReadTaxTotals(kTaxTotalsPath, year)
    household.totals <- ReadHouseholdTotals(kHouseholdsStatisticsPath, year)
}

if(!interactive()) {
    args = commandArgs(trailingOnly = TRUE)

    if (length(args) < 2) {
        stop("Usage: Rscript main.R year input.path")
    }

    year = as.integer(args[1])
    input.path = args[2]

    main(year, input.path)
}
