#' @title Plot Interactive Line Chart
#' @description Draws a line chart with zoomer and customisable line styles.
#' The function is optimised for data at the daily and monthly granularity, 
#' and can be combined with function CreateDateHierarchy() in the same repo.
#' @author daotq
#' 
#' @param data A data frame.
#' @param date_col Unquoted name of the Date/Posixct column.
#' @param value_col Unquoted name of the numeric value column.
#' @param line_type Character. Style of the line: "straight" or "curve". Defaults to "straight".
#' @param chart_title Character. Title of the chart. Defaults to NULL.
#' @param line_color Character. Hex code for the line color. Defaults to dark blue.
#'
#' @export

PlotLineChart <- function(data, date_col, value_col, 
                          line_type = "straight", 
                          chart_title = NULL, 
                          line_color = "#003366") {
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(echarts4r, dplyr, rlang)
  
  # Variables
  date_enquo <- rlang::enquo(date_col)
  val_enquo  <- rlang::enquo(value_col)
  val_label  <- rlang::as_name(val_enquo)
  
  is_smooth <- ifelse(line_type == "curve", TRUE, FALSE)
  
  # Data Preparation
  plot_data <- data %>%
    mutate(date_clean = as.Date(!!date_enquo)) %>%
    filter(!is.na(date_clean)) %>%
    arrange(date_clean) %>%
    rename(val = !!val_enquo)
  
  # Build Chart
  p <- plot_data %>%
    e_charts(date_clean) %>%
    e_line(
      val, 
      name = val_label, 
      symbol = "circle", 
      symbolSize = 6,
      smooth = is_smooth
    ) %>%
    e_theme("macarons") %>%
    e_tooltip(
      trigger = "axis",
      formatter = htmlwidgets::JS("
        function(params) {
          if (!params || params.length === 0) return '';
          
          var item = params[0];
          // Format date
          var date = new Date(item.value[0]);
          var dateStr = date.toLocaleDateString('en-US', { 
            year: 'numeric', month: 'short', day: 'numeric' 
          });
          
          // Extract the numeric value 
          var rawVal = item.value[1];
          var formattedVal = parseFloat(rawVal).toLocaleString('en-US', {
            minimumFractionDigits: 2, 
            maximumFractionDigits: 2
          });
          
          return '<b>Date: ' + dateStr + '</b><br/>' + 
                 item.marker + item.seriesName + ': ' + formattedVal;
        }
      ")
    ) %>%
    e_datazoom(
      type = "slider", 
    ) %>%
    e_datazoom(type = "inside") %>% 
    e_x_axis(type = "time") %>%
    e_y_axis(
      axisLabel = list(
        formatter = htmlwidgets::JS("
          function(value){
            return value.toLocaleString('en-US', {minimumFractionDigits: 0});
          }
        ")
      )
    ) %>%
    e_toolbox_feature("saveAsImage") %>%
    e_toolbox_feature("restore") %>% 
    e_grid(bottom = 80, left = "10%", right = "5%")
  
  # Styling
  if (!is.null(line_color)) p <- p %>% e_color(line_color)
  if (!is.null(chart_title)) p <- p %>% e_title(chart_title)
  
  return(p)
}
