
Kelvin.to.Celsius <- function (x) {

    x - 273.15
}

hours.to.years <- function (x, first.year) {

    (x - min(x))/24/365.25 + first.year
}
