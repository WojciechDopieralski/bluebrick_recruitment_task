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

first_sales <- game_is_promo %>%
  filter(date < '2017-02-02')

second_sales <- game_is_promo %>%
  filter(date > '2017-02-02')

sales_period <- game_is_promo %>%
  filter(winter_sales != "no_ws", is_promo == T)

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

only_ws_data <- date_group %>%
  filter(winter_sales != "no_ws") %>%
  group_by(winter_sales) %>%
  mutate(rev_promo_rate = round(sum_promo_rev/lag(sum_promo_rev) * 100,2)) %>%
  ungroup()

mean_price <- only_ws_data %>%
  group_by(winter_sales) %>%
  summarise(mean_unit = round(mean(unit_net_rev),2)) %>%
  ungroup() %>%
  mutate(discount = c(0.32,0.4),
         norm_price = round((mean_unit/(1-discount)),2))

data_pie_chart_s1 <- date_group %>%
  filter(sales_season == 1)

data_pie_chart_s2 <- date_group %>%
  filter(sales_season == 2)

region_revenue <- game_is_promo %>%
  group_by(sales_season, winter_sales, region) %>%
  summarise(sum_rev = sum(net_sales_usd)) %>%
  mutate(rev_group_perc = sum_rev/sum(sum_rev)) %>%
  ungroup() %>%
  mutate(rev_group_perc = percent(rev_group_perc)) %>%
  mutate(rev_ungroup_perc = percent(sum_rev/sum(sum_rev)))


region_sales <- game_is_promo %>%
  group_by(sales_season, winter_sales, region) %>%
  summarise(sum_net_sales = sum(net_units_sold)) %>%
  mutate(copy_sales_group_perc = sum_net_sales/sum(sum_net_sales)) %>%
  ungroup() %>%
  mutate(copy_sales_group_perc = percent(copy_sales_group_perc)) %>%
  mutate(copy_sales_ungroup_perc = percent(sum_net_sales/sum(sum_net_sales)))


region_rev_whole_year <- region_revenue %>%
  group_by(region) %>%
  summarise(sum_rev = sum(sum_rev), perc = sum(rev_ungroup_perc))

region_sales_whole_year <- region_sales %>%
  group_by(region) %>%
  summarise(sum_rev = sum(sum_net_sales), perc = sum(copy_sales_ungroup_perc))



