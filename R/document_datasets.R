#' Dataset: Legal Forms from the GLEIF
#'
#'
#' @format A data frame with 3,322 rows and 3 variables:
#' \describe{
#'   \item{id}{Legal Form ID}
#'   \item{iso3}{ISO 3166-1 alpha-3 country codes}
#'   \item{lfo}{Original legal form}
#'   \item{lfs}{Standardized legal form}
#' }
"legal_form_gle"

#' Dataset: Legal Forms from the ECB
#'
#'
#' @format A data frame with 2,399 rows and 3 variables:
#' \describe{
#'   \item{id}{Legal Form ID}
#'   \item{iso3}{ISO 3166-1 alpha-3 country codes}
#'   \item{lfo}{Original legal form}
#'   \item{lfs}{Standardized legal form}
#' }
"legal_form_ecb"

#' Dataset: Combined Legal Forms from the GLEIF and ECB
#'
#'
#' @format A data frame with 4,933 rows and 3 variables:
#' \describe{
#'   \item{id}{Legal Form ID}
#'   \item{iso3}{ISO 3166-1 alpha-3 country codes}
#'   \item{lfo}{Original legal form}
#'   \item{lfs}{Standardized legal form}
#' }
"legal_form_all"


#' Example Dataset: Source
#'
#'
#' @format A data frame with 2,426 rows and 5 variables:
#' \describe{
#'   \item{id}{A unique identifier}
#'   \item{name}{Company name}
#'   \item{iso3}{ISO 3166-1 alpha-3 country codes}
#'   \item{city}{City Name}
#'   \item{address}{Address Name}
#'   \item{size}{Firm Size}
#' }
"table_source"


#' Example Dataset: Target
#'
#'
#' @format A data frame with 4,671 rows and 5 variables:
#' \describe{
#'   \item{id}{A unique identifier}
#'   \item{name}{Company name}
#'   \item{iso3}{ISO 3166-1 alpha-3 country codes}
#'   \item{city}{City Name}
#'   \item{address}{Address Name}
#'   \item{size}{Firm Size}
#' }
"table_target"

#' Example Dataset: Matches
#'
#'
#' @format A data frame with 2,426 rows and 11 variables:
#' \describe{
#'   \item{id_s}{Source Dataframe: A unique identifier}
#'   \item{id_t}{Traget Dataframe: A unique identifier}
#'   \item{name_s}{Source Dataframe: Company name}
#'   \item{name_t}{Traget Dataframe: Company name}
#'   \item{iso3_s}{Source Dataframe: ISO 3166-1 alpha-3 country codes}
#'   \item{iso3_t}{Traget Dataframe: ISO 3166-1 alpha-3 country codes}
#'   \item{city_s}{Source Dataframe: City Name}
#'   \item{city_t}{Source Dataframe: City Name}
#'   \item{address_s}{Source Dataframe: Address Name}
#'   \item{address_t}{Traget Dataframe: Address Name}
#'   \item{match}{Indicator for matched names (always 1)}
#' }
"table_matches"
