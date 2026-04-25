#' @title Plot Interactive Histogram
#' @description Draws a histogram with interval notation [start - end), 
#' @author daotq
#'
#' @param data A data frame.
#' @param value_col The unquoted name of the numeric column.
#' @param bin_width Numeric. The interval size for each bin. 
#' >> Remember to input this bin_width - defaults is set to 100.
#' @param decimal Numeric. Number of decimal showing in x-axis. 
#' @param chart_title Character. Title of the chart. If NULL, no title is displayed.
#' @param colors Character vector. Custom colors for the columns.
#'
#' @export
PlotHistogram <- function(data, 
                          value_col, 
                          bin_width = 100, 
                          decimal = 0, 
                          chart_title = NULL, 
                          colors = NULL) {
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(echarts4r, dplyr, rlang)
  
  # Capture variable
  val_enquo <- rlang::enquo(value_col)
  
  # Helper function: trim white space
  fmt_num <- function(x) {
    trimmed <- trimws(format(x, big.mark = ",", nsmall = decimal, scientific = FALSE))
    return(trimmed)
  }
  
  # Data Preparation
  plot_data <- data |>
    select(!!val_enquo) |>
    filter(!is.na(!!val_enquo)) |>
    rename(val = !!val_enquo) |>
    mutate(
      bin_start = floor(val / bin_width) * bin_width,
      bin_end = bin_start + bin_width,
      bin_label = paste0("[", fmt_num(bin_start), " - ", fmt_num(bin_end), ")")
    ) |>
    group_by(bin_start, bin_label) |>
    summarise(count = n(), .groups = "drop") |>
    arrange(bin_start)
  
  # Histogram
  p <- plot_data |>
    e_charts(bin_label) |>
    e_bar(count, name = "Frequency", legend = FALSE) |>
    e_theme("macarons") |>
    e_tooltip(
      trigger = "axis",
      formatter = htmlwidgets::JS("
        function(params) {
          var count = params[0].value[1].toLocaleString('en-US', {maximumFractionDigits: 0});
          return '<b>Interval: ' + params[0].name + '</b><br/>' + 
                 params[0].marker + 'Count: ' + count;
        }
      ")
    )  |>
    e_toolbox_feature("saveAsImage") |>
    e_datazoom(type = "slider") |>
    e_x_axis(axisLabel = list(interval = 0, rotate = 45)) |> 
    e_grid(bottom = 150, left = "10%", right = "10%", top = "15%") 
  
  # Conditional Title
  if (!is.null(chart_title)) {
    p <- p |> e_title(chart_title, textStyle = list(color = "black"))
  }
  
  # Apply colors
  if (!is.null(colors)) {
    p <- p |> e_color(colors)
  }
  
  return(p)
}
