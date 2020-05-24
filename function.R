count_occurencies <- function(data, ...) {
  result <-data %>%
    group_by(...) %>%
    summarise(numb = n()) %>%
    mutate(perc_group = numb/sum(numb)) %>%
    ungroup() %>%
    mutate(perc_ungroup = numb/sum(numb))
  return(result)
}

count_param <- function(data, col_to_sum, ...) {
  col_name <- enquo(col_to_sum)
  result <- data %>%
    group_by(...) %>%
    summarise(numb = n(), sum_net = sum(!!col_name)) %>%
    mutate(count_sum_group = sum_net/sum(sum_net)) %>%
    ungroup() %>%
    mutate(count_sum_ungroup = sum_net/sum(sum_net))
  return(result)
}

create_waffle_chart <- function(data, fill, values, subtitle) {
  ggplot(data, aes(fill = {{fill}}, values = {{values}})) +
    geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
    labs(subtitle=subtitle) +
    coord_equal() +
    theme_enhance_waffle() +
    waffle_theme_custom
}

create_waffle_count <- function(data, fun_name, ...) {
  fun_name(data, ...) %>%
    mutate(perc_chart = percent(perc_group),
           waffle_chart = round(perc_group*100))
}

create_waffle_param <- function(data, fun_name, ...) {
  fun_name(data, ...) %>%
    mutate(perc_chart = percent(count_sum_group),
           waffle_chart = round(count_sum_group*100))
}

basic_line_season <- function (y_val, title ="add title", ylab_title = "name_lab") {
  ggplot_obj <- ggplot(date_group, aes(x = day_month, y = {{y_val}}, group = sales_season, colour = sales_season)) +
    geom_line() +
    geom_point() +
    labs(title=title,
         x = "Day and Month of sales",
         y = ylab_title) +
    simple_line_theme
  
  plotly_ojb <- ggplotly(ggplot_obj) 
  
  ret_obj <- plotly_ojb %>% 
    layout(legend = list(title=list(text='<b> Sales Season </b>')))
  
  return(ret_obj)
}