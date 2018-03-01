AdjustIncomes <- function(incomes, apportion.amounts) {
    # Adjust the income of each family.
    #
    # Args:
    #   incomes: The income of each family.
    #   apportion.amounts: The apportion amount of each tax item for each
    #                      family.
    #
    # Returns:
    #   The adjusted income of each family.
    adjusted.incomes <- incomes -
                        apportion.amounts$enterprise.income -
                        apportion.amounts$sales -
                        apportion.amounts$amusement -
                        apportion.amounts$commodity -
                        apportion.amounts$tariff -
                        apportion.amounts$securities.exchange -
                        apportion.amounts$alcohol.tobacco

    return(adjusted.incomes)
}

CalculateAfterTaxIncomes <- function(incomes, apportion.amounts) {
    # Calculate the after tax income of each family.
    #
    # Args:
    #   incomes: The income of each family.
    #   apportion.amounts: The apportion amount of each tax item for each
    #                      family.
    #
    # Returns:
    #   The after tax income of each family.
    after.tax.incomes <- incomes

    for (name in names(apportion.amounts)) {
        after.tax.incomes = after.tax.incomes - apportion.amounts[[name]]
    }

    return(after.tax.incomes)
}
