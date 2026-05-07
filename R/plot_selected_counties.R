utils::globalVariables(c("ke_shape", "COUNTY"))

#' Plot Selected Kenya Counties
#'
#' Creates a \code{ggplot2} map highlighting specific counties by name.
#' Only the selected counties are plotted.
#'
#' @param counties A character vector of county names to plot. Names are
#'   matched against the \code{COUNTY} field in the boundary data
#'   (case-insensitive).
#' @param data An optional data frame to join onto the selected counties.
#'   Must contain a column matching \code{join_by} with county names, plus
#'   the variable named in \code{fill_var}.
#' @param fill_var A string giving the column in \code{data} to use as the
#'   fill variable. Ignored when \code{data} is \code{NULL}.
#' @param join_by A string giving the county name column in \code{data} used
#'   to join with the \code{COUNTY} field. Defaults to \code{"county"}.
#' @param title Optional plot title string.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \donttest{
#' # Plot a subset of counties with no fill
#' plot_selected_counties(counties = c("Nairobi", "Mombasa", "Kisumu"))
#'
#' # Plot selected counties filled by a variable
#' df <- data.frame(
#'   county = c("Nairobi", "Mombasa", "Kisumu"),
#'   value  = c(80, 55, 40)
#' )
#' plot_selected_counties(
#'   counties  = c("Nairobi", "Mombasa", "Kisumu"),
#'   data      = df,
#'   fill_var  = "value",
#'   title     = "Selected counties"
#' )
#' }
#'
#' @importFrom sf st_as_sf
#' @export
plot_selected_counties <- function(counties, data = NULL, fill_var = NULL,
                                   join_by = "county", title = NULL) {

  matched <- ke_shape[tolower(ke_shape$COUNTY) %in% tolower(counties), ]

  if (nrow(matched) == 0) {
    stop("No counties matched. Check spelling against ke_shape$COUNTY.")
  }

  if (!is.null(data)) {
    matched <- dplyr::left_join(
      matched, data,
      by = stats::setNames(join_by, "COUNTY")
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

  p <- p + suppressWarnings(ggplot2::geom_sf_text(ggplot2::aes(label = COUNTY), size = 3))

  p <- p + ggplot2::theme_void()

  if (!is.null(title)) {
    p <- p + ggplot2::labs(title = title)
  }

  p
}
