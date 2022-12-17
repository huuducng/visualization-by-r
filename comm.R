# install.packages("pacman") #Install a package for managing other lib
#library("pacman") #Not required

pacman::p_load(here, rio,
               tidyverse, lubridate, reshape2, scales,
               janitor,
               cowplot)

data_file <- here(Sys.getenv("USERPROFILE"), "data", "w_cmo-economist.xlsx")

df <- import(data_file) %>%
  janitor::clean_names() #Clean columns name

# create date column
df$date <- make_date(year = df$year, month = df$month, day = df$day)

# aggregate to monthly data
df$year_month <- floor_date(df$date, "month")
df_m <- df %>%
  select(-c(year, month, day, date)) %>% 
  group_by(year_month) %>% 
  summarise(across(.cols = everything(), .fns = mean)) %>% 
  as.data.frame()

df_to_plot <- df_m[df_m$year_month >= max(df_m$year_month) - months(12),]

df_melted <- df_to_plot %>%
  select(year_month, all_items, food,
  non_food_agriculturals, metals, brent) %>%
  reshape2::melt(id.var="year_month")

theme_set(theme_minimal())

plot1 <- ggplot(df_melted[df_melted$variable != "brent", ],
                aes(x=year_month, y=value, col=variable, shape=variable)) +
  geom_path() +
  geom_point() +
  scale_x_date(labels = date_format("%m/%Y"), expand=c(0.01,0.01)) +
  theme(axis.title = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        legend.position = "top",
        legend.title = element_blank()) +
  scale_color_brewer(palette="Set1", #color palette
                     breaks=c("all_items", #select order of labels name
                              "food",
                              "non_food_agriculturals",
                              "metals"),
                     labels=c("Tổng", #rename labels
                              "Thực phẩm",
                              "Nông sản ngoài thực phẩm",
                              "Kim loại")) +
  scale_shape(labels = c("Tổng", "Thực phẩm", "Nông sản ngoài thực phẩm", "Kim loại")) +
  labs(title = str_c("Giá hàng hoá thế giới ",
                     strftime(min(df_to_plot$year_month), "%m/%Y"),
                     " - ", strftime(max(df_to_plot$year_month), "%m/%Y"), "*"),
       subtitle = "Chỉ số USD, trung bình các tuần trong tháng, 2015 = 100",
       caption = str_c("Nguồn: The Economist",
                       "\n* Số liệu đến ngày ",
                       strftime(max(df$date), "%d/%m")))

ggsave(filename = "global_comm.png",
       plot = plot1,
       width = 17, height = 13,
       units = "cm", dpi = 300,
       bg = "white")

plot2 <- ggplot(df_melted[df_melted$variable == "brent",],
                aes(x=year_month, y=value, fill=variable)) +
  geom_bar(stat = "identity") +
  scale_x_date(labels = date_format("%m/%Y"), expand=c(0.01,0.01)) +
  theme(axis.title = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        legend.position = "none") +
  scale_fill_brewer(palette="Set1",
                    labels=c()) +
  labs(title = str_c("Giá dầu Brent ",
                     strftime(min(df_to_plot$year_month), "%m/%Y"),
                     " - ", strftime(max(df_to_plot$year_month), "%m/%Y"), "*"),
       subtitle = "USD/thùng, trung bình các tuần trong tháng",
       caption = str_c("Nguồn: The Economist",
                       "\n* Số liệu đến ngày ",
                       strftime(max(df$date), "%d/%m")))

ggsave(filename = "global_brent.png",
       plot = plot2,
       width = 17, height = 13,
       units = "cm", dpi = 300,
       bg = "white")