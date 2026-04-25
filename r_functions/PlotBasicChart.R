#' @title Plot Column or Pie chart   
#' @description Only for Pie and Column charts using summarized data.
#' The output chart will be html format
#' @author daotq
#' 
#' @param data A summarized data frame.
#' @param label The unquoted name of the categorical column (e.g., CategoryName).
#' @param value The unquoted name of the numeric column (e.g., SalesAmount).
#' @param chart_type Character. Either "column" (as default) or "pie".
#' @param title Character. Optional chart title
#' @param title_size Character. Optional font size of chart title.
#' @export

PlotBasicChart <- function(data, label, value, chart_type = "column", 
                           title = NULL, title_size = 18) {
  
  # Library 
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(echarts4r, dplyr, rlang, htmlwidgets)
  
  # Data preparation
  label_enquo <- rlang::enquo(label)
  value_enquo <- rlang::enquo(value)
  value_name  <- rlang::as_name(value_enquo)
  
  plot_data <- data %>%
    ungroup() %>%
    select(!!label_enquo, !!value_enquo) %>%
    rename(nm = !!label_enquo, val = !!value_enquo) %>%
    mutate(nm = as.character(nm), val = as.numeric(val)) %>%
    arrange(desc(val)) %>%
    mutate(nm = factor(nm, levels = nm))
  
  # Base Chart
  p <- plot_data %>%
    e_charts(nm) %>%
    e_theme("macarons") %>%
    e_title(title, textStyle = list(fontSize = title_size)) %>%
    e_toolbox_feature("saveAsImage")
  
  num_fmt <- "style: 'decimal', minimumFractionDigits: 2, maximumFractionDigits: 2"
  
  # Chart Types 
  ## Pie chart 
  if (chart_type == "pie") {
    p <- p %>% 
      e_pie(val, name = value_name) %>% 
      e_legend(
        orient = "vertical", 
        right = "15%" , 
        top = "middle"
      ) %>%
      e_tooltip(
        trigger = "item",
        formatter = htmlwidgets::JS(paste0("
          function(params) {
            var formattedValue = params.value.toLocaleString('en-US', {", num_fmt, "});
            return '<b>' + params.name + '</b><br/>' + 
                   params.marker + 'Value: ' + formattedValue + '<br/>' + 
                   'Percent: ' + params.percent.toFixed(2) + '%';
          }
        "))
      )
    ## Column chart 
  } else {
    p <- p %>% 
      e_bar(val, name = value_name) %>% 
      e_legend(bottom = "0%") %>% 
      e_tooltip(
        trigger = "axis",
        formatter = htmlwidgets::JS(paste0("
          function(params) {
            var valRaw = params[0].value[1];
            var formattedValue = Number(valRaw).toLocaleString('en-US', {", num_fmt, "});
            return '<b>' + params[0].name + '</b><br/>' + 
                   params[0].marker + params[0].seriesName + ': ' + formattedValue;
          }
        "))
      ) %>%
      e_x_axis(axisLabel = list(interval = 0, rotate = 15))
  }
  
  return(p)
}

