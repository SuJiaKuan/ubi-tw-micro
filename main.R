source("read.R")

kSupportedYears = c(104)

main <- function(year, input.path) {
    if (!year %in% kSupportedYears) {
        stop("Year \"", year, "\" is not in supprted list: [",
             paste(kSupportedYears, collapse = ","), "]")
    }
    families <- ReadFamilyData(input.path)
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
