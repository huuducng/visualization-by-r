if (!require(pacman)) {
  install.packages("pacman")
}

pacman::p_load(here, rio,
               tidyverse, lubridate, reshape2, scales, xts,
               janitor,
               cowplot,
               extrafont)

loadfonts(device = "win")

data_file <- here(Sys.getenv("USERPROFILE"),
                  "CloudDrive/My Drive/data", "vn_money_market", "vn_m2.xlsx")

df <- import(data_file) %>% 
  janitor::clean_names()

max_date <- max(make_date(year = df$year, month = df$month, day = 1))

df <- complete(df, item, year, month)

df$year_month <- make_date(year = df$year, month = df$month, day = 1)
df <- df %>% 
  filter(year_month <= max_date)

df[, "d12_m2"] = df[, "so_du"]/dplyr::lag(df[, "so_du"], n = 12)*100-100

df_to_plot <- df %>% 
  select(year_month, d12_m2, item) %>% 
  filter(year_month >= max_date - months(12))

theme_set(theme_minimal())

plot <- ggplot(df_to_plot, aes(x = year_month, y = d12_m2,
                               color = item, shape = item)) +
  geom_path() +
  geom_point() +
  scale_x_date(labels = date_format("%m/%Y"), expand=c(0.05,0.05)) +
  theme(text=element_text(size=14,  family="serif"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        legend.position = "top",
        legend.title = element_blank()) +
  scale_color_brewer(palette="Set1", #color palette
                     breaks=c("Tổng", #select order of labels name
                              "Tiền gửi các TCKT",
                              "Tiền gửi dân cư"),
                     labels=c("M2", #rename labels
                              "Tiền gửi các TCKT",
                              "Tiền gửi dân cư")) +
  scale_shape(breaks = c("Tổng",
                         "Tiền gửi các TCKT",
                         "Tiền gửi dân cư"),
              labels = c("M2",
                         "Tiền gửi các TCKT",
                         "Tiền gửi dân cư")) +
  labs(title = str_c("Tổng phương tiện thanh toán ",
                     strftime(min(df_to_plot$year_month), "%m/%Y"),
                     " - ", strftime(max(df_to_plot$year_month), "%m/%Y")),
       subtitle = "% tăng so với cùng kỳ năm trước",
       caption = str_c("Nguồn: NHNN"))

ggsave(path = "img",
       filename = "vn_m2.png",
       plot = plot,
       width = 17, height = 13,
       units = "cm", dpi = 300,
       bg = "white")