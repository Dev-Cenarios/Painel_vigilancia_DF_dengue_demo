#' originize app
#'
#' @description Originize app functions
#' @import origin
#' @noRd
originize_app <- function() {
  pkgs <- readLines("DESCRIPTION")

  pkgs_start <- which(pkgs == "Imports: ")
  pkgs_end <- which(grepl("Encoding: ", pkgs, fixed=T))

  pkgs <- pkgs[(pkgs_start+1):(pkgs_end-1)] |>
    gsub(pattern = ",| ", replacement = "")

  origin::originize_dir(path = "R", pkgs = pkgs)
}
