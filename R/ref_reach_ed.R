#' ed_data: Reference tables for the energy densities of all foods in the REACH
#'
#' Energy Density for each food served during project REACH
#'
#'
#' @docType data
#'
#' @usage data(ed_data)
#'
#' @format A data.frame with columns:
#' \describe{
#'     \item{meal}{meal food was served at}
#'     \item{food}{food item}
#'     \item{ed}{energy denisty}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(ed_data)
#' ed <- attr(ed_data, "ed")
#'
"ed_data"
