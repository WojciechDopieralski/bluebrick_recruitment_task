plat_numb_trans_whole <- count_occurencies(game_is_promo, platform) %>%
  mutate(ypos = cumsum(perc_group) - perc_group/25,
         perc_chart = percent(perc_group),
         waffle_chart = round(perc_group*100))

plat_numb_trans_s1 <- create_waffle_count(first_sales, count_occurencies, platform)
plat_numb_trans_s2 <- create_waffle_count(second_sales, count_occurencies, platform)

plat_sales_whole <- create_waffle_param(game_is_promo, count_param, net_sales_usd, platform)
plat_sales_s1 <- create_waffle_param(first_sales, count_param, net_sales_usd, platform)
plat_sales_s2 <- create_waffle_param(second_sales, count_param, net_sales_usd, platform)

plat_net_num_whole <- create_waffle_param(game_is_promo, count_param, net_units_sold, platform)
plat_net_num_s1 <- create_waffle_param(first_sales, count_param, net_units_sold, platform)
plat_net_num_s2 <- create_waffle_param(second_sales, count_param,net_units_sold, platform)

both_promo_waffle <- ggplot(plat_numb_trans_whole, aes(fill = platform, values = waffle_chart)) +
  geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
  labs(subtitle="transactions made in both sales") +
  coord_equal() +
  theme_enhance_waffle() +
  waffle_theme_custom

first_promo_waffle <- create_waffle_chart(plat_numb_trans_s1, platform, waffle_chart, 
                                          "transactions made in sales 1")

second_promo_waffle <- create_waffle_chart(plat_numb_trans_s2, platform, waffle_chart, 
                                           "transactions made in sales 2")

both_promo_waffle_sales <- create_waffle_chart(plat_sales_whole, platform, waffle_chart,
                                               "Revenue in both sales")

first_promo_waffle_sales <- create_waffle_chart(plat_sales_s1, platform, waffle_chart,
                                                "Revenue in S1")

second_promo_waffle_sales <- create_waffle_chart(plat_sales_s2, platform, waffle_chart,
                                                 "Revenue in S2")

both_promo_waffle_numb <- create_waffle_chart(plat_net_num_whole, platform, waffle_chart,
                                              "Copies sold in both sales")

first_promo_waffle_numb <- create_waffle_chart(plat_net_num_s1, platform, waffle_chart,
                                               "Copies sold in S1")

second_promo_waffle_numb <- create_waffle_chart(plat_net_num_s2, platform, waffle_chart,
                                                "Copies sold in S2")

arrange_trans <- ggarrange(both_promo_waffle, first_promo_waffle, second_promo_waffle, 
                           legend = "none", ncol = 3)

arrange_sold_copies <- ggarrange(both_promo_waffle_numb, first_promo_waffle_numb, second_promo_waffle_numb, 
                                 legend = "none",  ncol = 3)

arrange_revenues <- ggarrange(both_promo_waffle_sales, first_promo_waffle_sales, second_promo_waffle_sales, 
                              common.legend = TRUE, legend = "bottom", ncol = 3)

ann_trans <- annotate_figure(p = arrange_trans, top = text_grob("Waffle charts in percentage divided by platform", face = "bold", size = 14))

ann_revenues <- annotate_figure(p = arrange_revenues,bottom = text_grob("1 square == 1pp", color = "blue", hjust = 1, x = 1, face = "italic", size = 8))