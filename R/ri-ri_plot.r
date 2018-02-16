#' Uses plotly to place the caluclated residence index on a map.
#'
#' @details This function uses plotly to place the caluclated RI on a map.
#'
#' @param df A data.frame from the \code{residence_index} containing
#' days_detected, residency_index, location, mean_latitude,
#' mean_longitude
#'
#' @param title A string for the title of the plot
#'
#' @return A plotly plot_geo object
#'
#' @import magrittr
#' @import plotly
#' @export
ri_plot <- function(df, title="Residence Index") {
  lat_range <- c(df$mean_latitude[which.min(df$mean_latitude)], df$mean_latitude[which.max(df$mean_latitude)])
  lon_range <- c(df$mean_longitude[which.min(df$mean_longitude)], df$mean_longitude[which.max(df$mean_longitude)])
  m <- list(colorbar = list(title = "Residence Index"))

  g <- list(
    showland = TRUE,
    landcolor = plotly::toRGB("gray"),
    subunitcolor = plotly::toRGB("white"),
    countrycolor = plotly::toRGB("white"),
    showlakes = TRUE,
    lakecolor = plotly::toRGB("white"),
    showsubunits = TRUE,
    showcountries = TRUE,
    resolution = 50,
    projection = list(
      type = 'conic conformal',
      rotation = list(lon = -100)
    ),
    lonaxis = list(
      showgrid = TRUE,
      gridwidth = 0.5,
      range = lon_range,
      dtick = 1
    ),
    lataxis = list(
      showgrid = TRUE,
      gridwidth = 0.5,
      range = lat_range,
      dtick = 1
    )
  )

  p <- plot_geo(df, lat = ~mean_latitude, lon = ~mean_longitude, color = ~residency_index) %>%
    add_markers(size = ~((df$residency_index * 5) + 1),
                text = ~paste(df$location, ":",df$residency_index), hoverinfo = "text"
    ) %>%
    layout(title = title, geo = g)
  show(p)
  return(p)
}
