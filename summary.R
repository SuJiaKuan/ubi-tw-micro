library(ineq)

Gini <- function(incomes) {
    # Calculate Gini coefficient for a list of incomes.
    #
    # Args:
    #   incomes: The list of incomes.
    #
    # Returns:
    #   The result of Gini coefficient.
    gini.coeff <- ineq(incomes, type = "Gini")

    return(gini.coeff)
}
