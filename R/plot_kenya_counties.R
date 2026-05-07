utils::globalVariables(c("ke_shape"))

#' Plot Kenya County Boundaries
#'
#' Creates a \code{ggplot2} map of Kenya's 47 counties. Optionally fill
#' counties by a numeric variable to produce a map.
#'
#' @param data An optional data frame to join onto the county boundaries.
#'   Must contain a column matching \code{join_by} with county names, plus
#'   the variable named in \code{fill_var}.
#' @param fill_var A string giving the column in \code{data} to use as the
#'   fill variable. Ignored when \code{data} is \code{NULL}.
#' @param join_by A string giving the county name column in \code{data} used
#'   to join with the \code{COUNTY} field in \code{kenya_map}.
#'   Defaults to \code{"county"}.
#' @param title Optional plot title string.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \donttest{
#' # Map: pass a data frame with a numeric column
#' df <- data.frame(
#'   county = ke_shape$COUNTY,
#'   value  = runif(47, 0, 100)
#' )
#' plot_kenya_counties(data = df, fill_var = "value", title = "Random values")
#' }
#'
#' @importFrom sf st_as_sf
#' @export
plot_kenya_counties <- function(data = NULL, fill_var = NULL, join_by = "county", title = NULL)
{

  counties_sf <- ke_shape

  if(!is.null(data))
  {
    counties_sf <- dplyr::left_join(counties_sf, data, by = stats::setNames(join_by, "COUNTY"))
  }

  p <- ggplot2::ggplot(data = counties_sf)

  if (!is.null(fill_var)) {
    fill_sym <- rlang::sym(fill_var)
    p <- p + ggplot2::geom_sf(ggplot2::aes(fill = !!fill_sym))
  } else {
    p <- p + ggplot2::geom_sf()
  }

  p <- p + ggplot2::theme_void()

  if(!is.null(title))
  {
    p <- p + ggplot2::labs(title = title)
  }
  p
}
