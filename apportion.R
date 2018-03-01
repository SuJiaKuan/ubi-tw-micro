# The unit of tax amount
kTaxAmountUnit = 1e6

# 綜合所得稅
IndividualIncomeTax <- function(families) {
    amount <- families$tax$individual.income

    return(amount)
}

# 營利事業所得稅
EnterpriseIncomeTax <- function(families,
                                samples.tax.totals,
                                apportion.hypothesis) {
    if (apportion.hypothesis == "A") {
        amount =
            samples.tax.totals$enterprise.income *
            ((3 / 4) * families$apportion.weights$income$investment.business.net +
             (1 / 4) * families$apportion.weights$income$property)

        return(amount)
    } else {
        # TODO(JiaKuan Su): Handle hypothesis B.
    }
}

# 土地增值稅
LandValueIncrementTax <- function(families,
                                  samples.tax.totals,
                                  apportion.hypothesis) {
    if (apportion.hypothesis == "A") {
        amount =
            samples.tax.totals$land.value.increment *
            families$apportion.weights$income$property

        return(amount)
    } else {
        # TODO(JiaKuan Su): Handle hypothesis B.
    }
}

# 營業稅
SalesTax <- function(families, samples.tax.totals, apportion.hypothesis) {
    if (apportion.hypothesis == "A") {
        amount =
            samples.tax.totals$sales *
            ((1 / 2) * families$apportion.weights$income$distributed.factor +
             (1 / 2) * families$apportion.weights$expenditure$recurring)

        return(amount)
    } else {
        # TODO(JiaKuan Su): Handle hypothesis B.
    }
}

# 印花稅
StampTax <- function(families, samples.tax.totals, apportion.hypothesis) {
    if (apportion.hypothesis == "A") {
        amount =
            samples.tax.totals$stamp *
            ((1 / 2) * families$apportion.weights$income$property +
             (1 / 2) * families$apportion.weights$expenditure$recurring)

        return(amount)
    } else {
        # TODO(JiaKuan Su): Handle hypothesis B.
    }
}

# 使用牌照稅
VehicleLicenseTax <- function(families, samples.tax.totals) {
    amount =
        samples.tax.totals$vehicle.license *
        ((1 / 3) * families$apportion.weights$expenditure$transportaion.communication.tools +
         (1 / 3) * families$apportion.weights$income$property +
         (1 / 3) * families$apportion.weights$expenditure$take.transportaion)

    return(amount)
}

# 娛樂稅
AmusementTax <- function(families, samples.tax.totals) {
    amount =
        samples.tax.totals$amusement *
        families$apportion.weights$expenditure$entertainment.culture.services

    return(amount)
}

# 房屋稅與地價稅
HouseLandValue <- function(families, samples.tax.totals, apportion.hypothesis) {
    if (apportion.hypothesis == "A") {
        amount = families$tax$house.land.value

        return(amount)
    } else {
        # TODO(JiaKuan Su): Handle hypothesis B.
    }

}

# 遺產及贈與稅
EstateGiftTax <- function(families, samples.tax.totals) {
    amount =
        samples.tax.totals$estate.gift *
        families$apportion.weights$income$top.group.property

    return(amount)
}

# 契稅
DeedTax <- function(families, samples.tax.totals, apportion.hypothesis) {
    if (apportion.hypothesis == "A") {
        amount =
            samples.tax.totals$deed *
            families$apportion.weights$income$property

        return(amount)
    } else {
        # TODO(JiaKuan Su): Handle hypothesis B.
    }
}

# 證券交易稅
SecuritiesExchangeTax <- function(families, samples.tax.totals) {
    amount =
        samples.tax.totals$securities.exchange *
        families$apportion.weights$income$investment

    return(amount)
}

# 菸酒稅
AlcoholTobaccoTax <- function(families, samples.tax.totals) {
    amount =
        samples.tax.totals$alcohol.tobacco *
        ((1 / 2) * families$apportion.weights$expenditure$alcohol +
         (1 / 2) * families$apportion.weights$expenditure$tobacco)

    return(amount)
}

# 貨物稅
CommodityTax <- function(families, samples.tax.totals, apportion.hypothesis) {
    if (apportion.hypothesis == "A") {
        amount =
            samples.tax.totals$commodity *
            ((1 / 2) * families$apportion.weights$income$distributed.factor +
             (1 / 2) * families$apportion.weights$expenditure$consumption)

        return(amount)
    } else {
        # TODO(JiaKuan Su): Handle hypothesis B.
    }
}

# 關稅
TariffTax <- function(families, samples.tax.totals, apportion.hypothesis) {
    if (apportion.hypothesis == "A") {
        amount =
            samples.tax.totals$tariff *
            ((1 / 2) * families$apportion.weights$income$distributed.factor +
             (1 / 2) * families$apportion.weights$expenditure$consumption)

        return(amount)
    } else {
        # TODO(JiaKuan Su): Handle hypothesis B.
    }
}

CalculateApportionAmounts <- function(families,
                                      tax.totals,
                                      household.totals,
                                      apportion.hypothesis) {
    # Calculate the apportion amount of each tax item for each family.
    #
    # Args:
    #   families: Families data that contain their income and expenditure.
    #   tax.totals: The total income amount of each tax item.
    #   household.totals: The total number of household.
    #   apportion.hypothesis: The hypothesis of tax apportion. It can be "A" or
    #                         "B".
    #
    # Returns:
    #   The apportion amount of each tax item for each family.

    # Calculate the ratio between the survey sample size to the total number of
    # households.
    samples.ratio <- families$samples.size / household.totals

    # Calculate the total amount of tax income for the sample families.
    samples.tax.totals <- lapply(as.double(tax.totals),
                                 "*",
                                 samples.ratio * kTaxAmountUnit)
    names(samples.tax.totals) <- names(tax.totals)

    apportion.amounts <- list(
        individual.income = IndividualIncomeTax(families),
        enterprise.income = EnterpriseIncomeTax(families,
                                                samples.tax.totals,
                                                apportion.hypothesis),
        land.value.increment = LandValueIncrementTax(families,
                                                     samples.tax.totals,
                                                     apportion.hypothesis),
        sales = SalesTax(families, samples.tax.totals, apportion.hypothesis),
        stamp = StampTax(families, samples.tax.totals, apportion.hypothesis),
        vehicle.license = VehicleLicenseTax(families, samples.tax.totals),
        amusement = AmusementTax(families, samples.tax.totals),
        house.land.value = HouseLandValue(families,
                                          samples.tax.totals,
                                          apportion.hypothesis),
        estate.gift = EstateGiftTax(families, samples.tax.totals),
        deed = DeedTax(families, samples.tax.totals, apportion.hypothesis),
        securities.exchange = SecuritiesExchangeTax(families,
                                                    samples.tax.totals),
        alcohol.tobacco = AlcoholTobaccoTax(families, samples.tax.totals),
        commodity = CommodityTax(families,
                                 samples.tax.totals,
                                 apportion.hypothesis),
        tariff = TariffTax(families, samples.tax.totals, apportion.hypothesis)
    )

    return(apportion.amounts)
}
