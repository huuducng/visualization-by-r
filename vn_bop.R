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
                  "CloudDrive/My Drive/data", "vn_national_account", "vn_bop.xlsx")

df <- import(data_file) %>% 
  janitor::clean_names()

df_y <- df %>%
  select(-c(quarter)) %>% 
  group_by(item, year) %>% 
  summarise(value = sum(value))
  # as.data.frame()

df_to_plot <- df_y %>% 
  filter(year > max(df_y$year) - 5, 
           item == "A. Cán cân vãng lai" | 
           item == "B. Cán cân tài chính" | 
           item == "C. Lỗi và Sai sót" |
           item == "D. Cán cân tổng thể")

# df_to_plot$year <- as.character(df_to_plot$year)

theme_set(theme_minimal())

plot1 <- ggplot(df_to_plot, aes(x = item, y = value/1000, fill = as.character(year))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = round(value/1000, 1), y = value/1000/2),
            position = position_dodge(width = 0.9),
            vjust = 0.5, size = 3, family = "serif") +
  theme(text=element_text(size = 14,  family = "serif"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        legend.position = "top",
        legend.title = element_blank()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = str_c("Cán cân thanh toán ",
                     as.character(min(df_to_plot$year)), "-",
                     as.character(max(df_to_plot$year))),
       subtitle = "tỷ USD",
       caption = str_c("Nguồn: NHNN"))

ggsave(path = "img",
       filename = "vn_bop.png",
       plot = plot1,
       width = 17, height = 13,
       units = "cm", dpi = 300,
       bg = "white")
