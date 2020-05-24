raw_game_data <- readxl::read_xlsx("raw_data/Recrutiment task - data set.xlsx")

data_clean_names <- janitor::clean_names(raw_game_data)

number_of_unique_values_in_col <- data_clean_names %>% 
  summarise_all(n_distinct) %>%
  pivot_longer(cols = everything(),
               names_to = "n_distinct")

data_column_choice <- data_clean_names %>%
  select(- package, -product_name, -type, -product_id_number) %>%
  mutate_if(~(is_character(.) & n_distinct(.) <= 130), as.factor)

country_map_table <- data_column_choice %>%
  select(country_code, country)

game_is_promo <- data_column_choice %>%
  select(-country) %>%
  mutate(
    unit_net_price_usd = net_sales_usd/net_units_sold,
    is_promo = !(near(base_price, sale_price)),
    promo_rev = ifelse(is_promo == T, net_sales_usd,0),
    normal_rev = ifelse(is_promo == F, net_sales_usd,0),
    discount = ifelse(is_promo, round((1-(sale_price/base_price))*100),0),
    sales_season = as.factor(ifelse(date < '2017-02-02',1,2)),
    winter_sales = as.factor(ifelse(date > '2016-12-22' & date < '2017-01-03',
                                    "ws1",
                                    ifelse(date > '2017-12-21' & date < '2018-01-04',
                                           "ws2",
                                           "no_ws")))
  ) %>%
  arrange(country_code)

date_group <- game_is_promo %>%
  group_by(date) %>%
  summarise(numb = n(), sum_revenu = sum(net_sales_usd), sum_net_sales = sum(net_units_sold), 
            sum_ret = sum(returns), mean_reve = mean(net_sales_usd), sum_promo_rev = sum(promo_rev), 
            sum_norm_rev = sum(normal_rev), ret_to_sell = sum_ret/sum_net_sales, unit_net_rev = sum_revenu/sum_net_sales) %>%
  mutate(sales_season = as.factor(ifelse(date < '2017-02-02',1,2)),
         day_month = format(date, "%m-%d")) %>%
  arrange(date) %>%
  mutate(id = row_number()) %>%
  ungroup() %>%
  mutate(day_month = fct_reorder(day_month,id),
         winter_sales = as.factor(ifelse(date > '2016-12-22' & date < '2017-01-03',
                                         "ws1",
                                         ifelse(date > '2017-12-21' & date < '2018-01-04',
                                                "ws2",
                                                "no_ws"))))



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

basic_line_season <- function (y_val, title ="add title", ylab_title = "name_lab", df_data = date_group) {
  ggplot_obj <- ggplot(data = df_data, aes(x = day_month, y = {{y_val}}, group = sales_season, colour = sales_season)) +
    geom_line() +
    geom_point() +
    labs(title=title,
         x = "Day and Month of sales",
         y = ylab_title) +
    simple_line_theme
  
  plotly_ojb <- ggplotly(ggplot_obj) 
  
  ret_obj <- plotly_ojb %>% 
    layout(legend = list(title=list(text='<b>Sales Season:</b>')))
  
  return(ret_obj)
}

draw_simple_bar <- function(data, y_val, title = "add title", ylab_title = "add_y_lab_name") {
  simple_bar <- ggplot(data, aes(x = reorder(region, -{{y_val}}), y = {{y_val}})) +
    geom_bar(stat="identity") +
    labs(title=title,
         x = "Regions",
         y = ylab_title) +
    simple_bar_theme 
  
  ret_obj <- ggplotly(simple_bar)
  return(ret_obj)
}