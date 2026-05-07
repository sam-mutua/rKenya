utils::globalVariables(c("ke_subcounty", "county", "subcounty"))

#' Plot Sub-Counties of a Selected County
#'
#' Creates a \code{ggplot2} map of all sub-counties within a specified county.
#' Optionally fills sub-counties by a numeric variable.
#'
#' @param county A character vector of one or more county names to plot. Matched
#'   case-insensitively against the \code{county} field in \code{ke_subcounty}.
#' @param data An optional data frame to join onto the sub-county boundaries.
#'   Must contain a column matching \code{join_by} with sub-county names, plus
#'   the variable named in \code{fill_var}.
#' @param fill_var A string giving the column in \code{data} to use as the
#'   fill variable. Ignored when \code{data} is \code{NULL}.
#' @param join_by A string giving the sub-county name column in \code{data}
#'   used to join with the \code{subcounty} field. Defaults to
#'   \code{"subcounty"}.
#' @param title Optional plot title string. Defaults to the county name.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \donttest{
#' # Plot sub-counties of a single county
#' plot_subcounties(county = "Makueni")
#'
#' # Plot sub-counties of two counties
#' plot_subcounties(county = c("Makueni", "Kitui"))
#'
#' # Plot two counties filled by a variable
#' df <- data.frame(
#'   subcounty = c(
#'     "Makueni  Sub County", "Kibwezi East  Sub County",
#'     "Kibwezi West  Sub County", "Kaiti  Sub County",
#'     "Mbooni  Sub County", "Kilome Sub County",
#'     "Kitui West  Sub County", "Kitui Central  Sub County",
#'     "Kitui Rural Sub County", "Kitui East Sub County",
#'     "Kitui South Sub County", "Mwingi West Sub County",
#'     "Mwingi Central Sub- County", "Mwingi North Sub County"
#'   ),
#'   value = c(60, 45, 70, 55, 80, 40, 65, 50, 75, 30, 85, 55, 45, 70)
#' )
#' plot_subcounties(county = c("Makueni", "Kitui"), data = df,
#'                 fill_var = "value", title = "Makueni & Kitui Sub-Counties")
#' }
#'
#' @importFrom sf st_as_sf
#' @export
plot_subcounties <- function(county, data = NULL, fill_var = NULL,
                             join_by = "subcounty", title = NULL) {

  matched <- ke_subcounty[tolower(ke_subcounty$county) %in% tolower(county), ]

  if (nrow(matched) == 0) {
    stop("No counties matched. Check spelling against ke_subcounty$county.")
  }

  if (!is.null(data)) {
    matched <- dplyr::left_join(
      matched, data,
      by = stats::setNames(join_by, "subcounty")
    )
  }

  p <- ggplot2::ggplot(data = matched)

  if (!is.null(fill_var)) {
    fill_sym <- rlang::sym(fill_var)
    p <- p +
      ggplot2::geom_sf(ggplot2::aes(fill = !!fill_sym)) +
      ggplot2::labs(fill = fill_var)
  } else {
    p <- p + ggplot2::geom_sf()
  }

  p <- p +
    suppressWarnings(
      ggplot2::geom_sf_text(
        ggplot2::aes(label = trimws(gsub("Sub-? County", "", subcounty, ignore.case = TRUE))),
        size = 2.5
      )
    ) +
    ggplot2::theme_void() +
    ggplot2::labs(title = if (!is.null(title)) title else paste(county, collapse = " & "))

  p
}
