pacman::p_load(here, rio,
               tidyverse, lubridate, reshape2, scales,
               janitor,
               cowplot,
               extrafont)

# font_import()
loadfonts(device = "win")

data_file <- here(Sys.getenv("USERPROFILE"),
                  "CloudDrive/My Drive/data", "w_cmo-wb.xlsx")
df1 <- import(data_file, which = "month") %>%
  janitor::clean_names()

df1$year_month <- make_date(year = df1$year, month = df1$month)

df1_melted <- df1[df1$year >= max(df1$year) - 5,] %>%
  select(year_month, energy, agriculture, food,
         base_metals_ex_iron_ore, fertilizers) %>% 
  reshape2::melt(id.var = "year_month")

theme_set(theme_minimal())

plot1 <- ggplot(df1_melted,
       aes(x=year_month, y=value, col=variable, shape=variable)) +
  geom_path() +
  scale_x_date(labels=date_format("%Y"), expand=c(0.03,0.03)) +
  theme(text=element_text(size=14,  family="serif"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        legend.position = "top",
        legend.title = element_blank()) +
  scale_color_brewer(palette = "Set1",
                     breaks = c("energy",
                                "agriculture",
                                "food",
                                "base_metals_ex_iron_ore",
                                "fertilizers"),
                     labels = c("Năng lượng",
                                "Nông sản",
                                "Thực phẩm",
                                "Kim loại cơ bản",
                                "Phân bón")) +
  labs(title = str_c("Giá hàng hoá thế giới ", 
                     strftime(min(df1_melted$year_month), "%Y"),
                     "-", strftime(max(df1_melted$year_month), "%Y")),
       subtitle = "Chỉ số USD, 2010=100",
       caption = "Nguồn: World Bank") +
  guides(color=guide_legend(ncol=3, bycol = TRUE))

ggsave(path = "img",
       filename = "global_comm_wb_monthly.png",
       plot = plot1,
       width = 17, height = 13,
       units = "cm", dpi = 300,
       bg = "white")

df2 <- import(data_file, which = "year") %>%
  janitor::clean_names()

df3 <- df2 %>%
  select(-c(year))

df3 <- df3/lag(df3)*100-100
df3$year = make_date(df2$year)


df3_melted <- df3[df3$year > max(df3$year) - years(9),] %>%
  select(year, energy, agriculture, food,
         base_metals_ex_iron_ore, fertilizers) %>% 
  reshape2::melt(id.var = "year")


plot2 <- ggplot(df3_melted,
                aes(x=year, y=value, col=variable, shape=variable)) +
  geom_path() +
  geom_point() +
  # scale_x_date(labels=date_format("%Y")) +
  theme(text = element_text(size=14,  family="serif"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        legend.position = "top",
        legend.title = element_blank()) +
  scale_color_brewer(palette = "Set1",
                     breaks = c("energy",
                                "agriculture",
                                "food",
                                "base_metals_ex_iron_ore",
                                "fertilizers"),
                     labels = c("Năng lượng",
                                "Nông sản",
                                "Thực phẩm",
                                "Kim loại cơ bản",
                                "Phân bón")) +
  scale_shape(labels = c("Năng lượng",
                         "Nông sản",
                         "Thực phẩm",
                         "Kim loại cơ bản",
                         "Phân bón")) +
  labs(title = str_c("Giá hàng hoá thế giới ",
                     strftime(min(df3_melted$year), "%Y"),
                     "-", strftime(max(df3_melted$year), "%Y")),
       subtitle = "% tăng so với năm trước",
       caption = "Nguồn: World Bank") +
  guides(color=guide_legend(ncol=3, bycol = TRUE))

ggsave(path = "img",
       filename = "global_comm_wb_yearly.png",
       plot = plot2,
       width = 17, height = 13,
       units = "cm", dpi = 300,
       bg = "white")