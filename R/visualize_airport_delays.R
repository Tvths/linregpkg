#' Visualize mean arrival delay per destination airport
#'
#' Uses \pkg{dplyr} to compute the mean arrival delay of all flights in
#' \code{nycflights13::flights} for each destination airport, joins the
#' coordinates from \code{nycflights13::airports} and plots the airports by
#' longitude and latitude with \pkg{ggplot2}, coloured by mean delay.
#'
#' @return A \code{ggplot} object (printed when called interactively).
#' @examples
#' \donttest{
#' visualize_airport_delays()
#' }
#' @importFrom dplyr filter group_by summarise inner_join n
#' @importFrom ggplot2 ggplot aes geom_point scale_colour_gradient2 labs theme_bw .data
#' @export
visualize_airport_delays <- function() {
  delays <- nycflights13::flights |>
    dplyr::filter(!is.na(.data$arr_delay)) |>
    dplyr::group_by(.data$dest) |>
    dplyr::summarise(mean_delay = mean(.data$arr_delay), n_flights = dplyr::n(),
                     .groups = "drop") |>
    dplyr::inner_join(nycflights13::airports, by = c("dest" = "faa"))

  ggplot2::ggplot(delays, ggplot2::aes(x = .data$lon, y = .data$lat,
                                       colour = .data$mean_delay,
                                       size = .data$n_flights)) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::scale_colour_gradient2(low = "blue", mid = "grey80", high = "red",
                                    midpoint = 0) +
    ggplot2::labs(title = "Mean arrival delay by destination airport",
                  x = "Longitude", y = "Latitude",
                  colour = "Mean delay (min)", size = "Number of flights") +
    ggplot2::theme_bw()
}
